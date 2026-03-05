{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Streamly.Coreutils.FileTest.Common
-- Copyright   : (c) 2021 Composewell Technologies
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- See "Streamly.Coreutils.FileTest" module for general module level
-- documentation. This module provides both posix and windows implementations.
--
-- Design Notes:
--
-- The "unix" package exposes low-level accessors for 'FileStatus'.
-- This module builds a higher-level predicate abstraction on top of it.
-- Predicates encapsulate common file tests and allow composable logic
-- (for example, comparing file sizes or timestamps) while ensuring that
-- multiple checks share a single underlying file status query.
--
-- 'FileTest' predicates operate on 'FileState', which carries:
--
--   * The 'FilePath' that was supplied to the runner ('test' / 'testl').
--   * A lazily-populated 'IORef' that caches the 'FileStatus' after the
--     first predicate that needs it fetches it.
--   * The OS stat action to use ('getFileStatus' for 'test',
--     'getSymbolicLinkStatus' for 'testl').  Storing it in 'FileState' means
--     every predicate automatically uses the right variant without needing to
--     know how it was invoked.
--
-- The cache guarantee: no matter how many predicates are composed with 'and_'
-- / 'or_', at most one @stat@ system call is issued per 'test' / 'testl'
-- invocation.  Predicates that are short-circuited away pay no stat cost.
--
-- FileTest is essentially a Reader. TODO: We can replace Predicate with
-- ReaderT FileState IO
--
-- We could use a more restricted StatusTest predicates which consume only the
-- file status argument. StatusTest can then be lifted into a FileTest which
-- passes a FilePath argument as well and maybe some others. StatusTest
-- predicates can be moved into a separate module. But does it buy us anything
-- worthwhile?
--
-- Naming: unary predicates are either isSomething or hasSomething. Binary
-- predicates are nouns. Predicates are named so that they read well on the
-- call site e.g. "test path doesExist" or "test path isReadable".
--
-- Files supported by windows:
--
-- Regular files
-- Directory files
-- Symbolic links: .symlink, .lnk
-- Hard links
-- Named pipes: .pipe
-- Device files: .sys, .dll
-- Mount point files: .mount, .vhd, .vhdx, .iso, .img
--
-- See FileType in Win32 package.
--
-- File Permissions:
--
-- See AccessMode and ShareMode in the Win32 package

-- Testing TODO:
-- XXX Need tests for Windows. Especially for file access permissions. How do
-- ACLs affect it? Also file times.

module Streamly.Coreutils.FileTest.Common
    (
    -- * File Test Predicate Type
      Predicate (..)
    , mkFileState
    , FileTest (..)

    -- * Creating FileTest Predicates
    , withState
    , withStateM
    , withStatusM
    , withStatus
    , withPathM
    , withPath

    -- * Predicate Combinators
    , not_
    , and_
    , or_

    -- * Folds
    , true
    , false
    , and
    , or

    -- * Running Predicates
    , test
    , testl
    , testGeneral

    -- * Predicates

    -- ** General
    , doesExist
#if !defined(CABAL_OS_WINDOWS)
    -- , isHardLinkTo
#endif

    -- ** File Type
    , isDir
    , isFile
    , isSymLink
    , isCharDevice
    , isBlockDevice
    , isPipe
    , isSocket
#if !defined(CABAL_OS_WINDOWS)
    -- , isTerminalFD
#endif

    -- ** File Permissions

    , hasMode
    , hasOwnerRead
    , hasOwnerWrite
    , hasOwnerExec

    , hasGroupRead
    , hasGroupWrite
    , hasGroupExec

    , hasOtherRead
    , hasOtherWrite
    , hasOtherExec

    -- ** File Flags

    , hasSetUid
    , hasSetGid
    , hasStickyBit

    -- ** File size
    -- XXX Need convenient size units and conversions (e.g. kB 1, kiB 1, mB 2)
    , size
    , sizeComparedTo
    , largerThan
    , smallerThan
    , sizeEquals
    , nonEmpty
    , largerThanFile
    , smallerThanFile
    , sameSizeAs

    -- ** File times
    -- XXX Need convenient time units and conversions (e.g. sec 5,
    -- "2022-01-01")
    -- Time units
    , seconds
    , minutes
    , hours
    , days

    -- *** File age
    , modifyAge
    , modifiedWithin
    , modifiedOlderThan

    , accessAge
    , accessedWithin
    , accessedOlderThan

    -- , createAge

    -- *** File timestamp
    , modifyTime
    , modifiedBefore
    , modifiedAfter

    -- , accessTime

    -- *** Compare timestamps with file
    , modifyTimeComparedTo
    , modifiedBeforeFile
    , modifiedAfterFile

    , accessTimeComparedTo
    , accessedBeforeFile
    , accessedAfterFile

    -- * Deprecated
    , isExisting
    )
where

import Control.Exception (catch, throwIO)
import Data.Bits ((.&.))
import Data.Int (Int64)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Time.Clock.POSIX (POSIXTime)
import Data.Time.Clock (NominalDiffTime)
import System.IO.Error (isDoesNotExistError)

-- XXX Remove the dependency on unix-compat and directory
import System.PosixCompat.Files (FileStatus)
import System.Posix.Types (COff(..), FileMode)

import qualified System.PosixCompat.Files as Files

import Prelude hiding (and, or)
import Streamly.Internal.Data.Time.Clock
import Streamly.Internal.Data.Time.Units

-- $setup
-- >>> import Prelude hiding (or, and)

newtype Predicate m a =
    Predicate (a -> m Bool)

------------------------------------------------------------------------------
-- FileState
------------------------------------------------------------------------------

-- | Carries all the information a 'FileTest' predicate needs to evaluate.
--
-- [@filepath@] The path supplied to the runner ('test' \/ 'testl').  Available
-- to any predicate that needs the path in addition to the status (e.g. for a
-- second stat call on a related file).
--
-- [@fileStatus@] A write-once lazy cache.  Starts as 'Nothing'.  The first
-- predicate that needs the 'FileStatus' calls 'requireStatus', which invokes
-- 'fetchStatus' and stores the result.  Every subsequent predicate in the same
-- composed expression reuses the cached value, so at most one @stat@ system
-- call is ever issued per runner invocation.
--
-- [@fetchStatus@] The OS stat action to use when the cache is empty.  Set by
-- the runner: 'test' supplies 'Files.getFileStatus' (follows symlinks);
-- 'testl' supplies 'Files.getSymbolicLinkStatus' (examines the link itself).
-- Storing the action here keeps the choice invisible to individual predicates.
data FileState = FileState
    { filepath    :: FilePath
      -- ^ The path supplied to 'test' \/ 'testl'.
    , fileStatus  :: IORef (Maybe FileStatus)
      -- XXX store it in IORef using Either type.
      -- ^ Lazily-populated 'FileStatus' cache.
    , fetchStatus :: IO FileStatus
      -- ^ OS stat action; called at most once, on cache miss.
    }

-- | Obtain the cached 'FileStatus', fetching and caching it on first call.
--
-- This is the single point through which all leaf predicates access the
-- 'FileStatus'. It guarantees the "at most one @stat@ call" invariant.
requireStatus :: FileState -> IO FileStatus
requireStatus fs = do
    cached <- readIORef (fileStatus fs)
    case cached of
        Just st -> pure st
        Nothing -> do
            st <- fetchStatus fs
            writeIORef (fileStatus fs) (Just st)
            pure st

-- | Construct a fresh 'FileState' with an empty (unfetched) status cache.
--
-- @fetchFn@ is the OS stat action predicates will use. Pass
-- 'Files.getFileStatus' for symlink-following behaviour, or
-- 'Files.getSymbolicLinkStatus' to examine the link itself.
newFileState :: FilePath -> IO FileStatus -> IO FileState
newFileState path fetchFn = do
    ref <- newIORef Nothing
    pure $ FileState
        { filepath    = path
        , fileStatus  = ref
        , fetchStatus = fetchFn
        }

-- | Constructs a 'FileState' whose cache is pre-populated with the supplied
-- 'FileStatus', so no additional @stat@ call is ever issued.  The 'filepath'
-- is left empty because no path is available at this call site; 'fetchStatus'
-- is set to an error thunk since it must never be called when the cache is
-- already populated.
mkFileState :: String -> FilePath -> FileStatus -> IO FileState
mkFileState tag fp st = do
    ref <- newIORef (Just st)
    return $ FileState
        { filepath    = fp
        , fileStatus  = ref
        , fetchStatus = error $ tag ++ ": BUG. fetchStatus cannot be used here"
        }

------------------------------------------------------------------------------
-- FileTest
------------------------------------------------------------------------------

-- Naming Notes: Named FileTest rather than "Test" to be more explicit and
-- specific. The command can also be named fileTest or testFile.
--
-- We do not provide a Semigroup/Monoid instance, though it provides a
-- convenient <> for the `and` operation but then we need a newtype wrapper for
-- the "or" operation. Also, the generic foldMap or mconcat provided by Monoids
-- are of limited use in this case.

-- Predicates receive a 'FileState' rather than a raw 'FileStatus'.  This
-- gives them access to the file path and lets them share the lazily-cached
-- 'FileStatus' without issuing redundant @stat@ calls.

-- | A predicate type for testing boolean statements about a file.
--
newtype FileTest =
    FileTest (Predicate IO FileState)

-- | A boolean @and@ function for combining two 'FileTest' predicates.
--
-- Note that 'and_' uses a single @stat@ system call for both the tests,
-- even if you combine many tests using a combination of 'and_' and 'or_'.
--
-- It short circuits i.e. if the first predicate evaluates to false it does not
-- evaluate the second.
--
and_ :: FileTest -> FileTest -> FileTest
and_ (FileTest (Predicate p)) (FileTest (Predicate q)) =
    -- The applicative does not short circuit, evaluates both the predicates.
    -- FileTest (Predicate $ \a -> (&&) <$> p a <*> q a)
    FileTest (Predicate f)

    where

    f a = do
        r <- p a
        if r
        then q a
        else pure False

-- | A boolean @or@ function for combining two 'FileTest' predicates.
--
-- Note that 'or_' uses a single @stat@ system call for both the tests,
-- even if you combine many tests using a combination of 'and_' and 'or_'.
--
-- It short circuits i.e. if the first predicate evaluates to true it does not
-- evaluate the second.
--
or_ :: FileTest -> FileTest -> FileTest
or_ (FileTest (Predicate p)) (FileTest (Predicate q)) =
    -- The applicative does not short circuit, evaluates both the predicates.
    -- FileTest (Predicate $ \a -> (||) <$> p a <*> q a)
    FileTest (Predicate f)

    where

    f a = do
        r <- p a
        if r
        then pure True
        else q a

-- NOTE: We can also use &&_, ||_ operators but probably not worth it.
-- For && we can provide a Semigroup instance and <> can be used, it can be
-- useful as it is auto imported by Prelude. But then we do not have a similar
-- solution for or. The operator will have to be imported explicitly from this
-- module.
infixr 3 `and_`
infixr 2 `or_`

-- Naming note: usually we would import this module qualified. If unqualified
-- use of and/or is needed we can rename them anded/ored, andL/orL,
-- conjunction/disjunction, andB/orB (boolean).

-- NOTE: Use foldr instead of foldl' as it allows short circuiting and infinite
-- containers.

-- | A boolean @and@ for combining a list of 'FileTest' predicates.
--
-- >>> and = foldr and_ true
--
and :: [FileTest] -> FileTest
and = foldr and_ true

-- | A boolean @or@ for combining a list of 'FileTest' predicates.
--
-- >>> or = foldr or_ false
--
or :: [FileTest] -> FileTest
or = foldr or_ false

-- | A boolean @not@ function for negating a 'FileTest' predicate.
--
not_ :: FileTest -> FileTest
not_ (FileTest (Predicate p)) = FileTest (Predicate (fmap not . p))

-- XXX Use Path instead of filepath.

applyCatchENOENT :: (t -> IO Bool) -> t -> IO Bool
applyCatchENOENT f fs =
    f fs `catch` eatENOENT
  where
    -- isDoesNotExistError covers NoSuchThing (ENOENT)
    eatENOENT e
        | isDoesNotExistError e = return False
        | otherwise             = throwIO e

-- | Apply a predicate to a 'FilePath', if the path is a symlink uses the link
-- target and not the link itself. See 'testl' for testing the link itself.
--
-- * 'test' returns 'True' if the file exists and the predicate is 'True'
-- * Returns 'False' if the file does not exist or the predicate is 'False'
-- * Fails with an IO exception if the path to the file is not accessible due
-- to lack of permissions. The exception type can be used to determine the
-- reason for failure.
--  * test 'isSymLink' always returns false.
--  * test 'doesExist' returns false if the path is symlink but it does not
--  point to an existing file.
--
test :: FilePath -> FileTest -> IO Bool
test path (FileTest (Predicate f)) = do
    -- 'Files.getFileStatus' dereferences symlinks.
    newFileState path (Files.getFileStatus path) >>= applyCatchENOENT f

-- | Like 'test' but uses the path and not the link target if the path is a
-- symlink.
--
--  * 'isSymLink' returns true if path is a symlink, false otherwise.
--  * 'doesExist' returns true if the link exists irrespective of whether it
--  points to an existing file.
--  * Predicates related to file permission mode bits are meaningless, and
--  should not be used.
--  * Predicates related to file owner, group, size, time stamps are relevant.
--
testl :: FilePath -> FileTest -> IO Bool
testl path (FileTest (Predicate f)) =
    newFileState path (Files.getSymbolicLinkStatus path) >>= applyCatchENOENT f

-- | Apply a predicate to a pre-fetched 'FileStatus'. Note you cannot use
-- predicates that require filepath when using apply.
testGeneral :: FilePath -> FileStatus -> FileTest -> IO Bool
testGeneral fp st (FileTest (Predicate f)) =
    mkFileState "FileTest.apply" fp st >>= f

-- | Like 'withState' but the supplied function may perform IO.
withStateM :: (FilePath -> FileStatus -> IO Bool) -> FileTest
withStateM p =
    FileTest $ Predicate $ \fs -> requireStatus fs >>= p (filepath fs)

-- | Convert a @FilePath -> FileStatus -> Bool@ function into a 'FileTest'
-- predicate.
withState :: (FilePath -> FileStatus -> Bool) -> FileTest
withState p = withStateM (\fp fs -> pure $ p fp fs)

-- | Like 'withStatus' but the supplied function may perform IO.
withStatusM :: (FileStatus -> IO Bool) -> FileTest
withStatusM p = FileTest $ Predicate $ \fs -> requireStatus fs >>= p

-- | Convert a @FileStatus -> Bool@ function into a 'FileTest' predicate.
withStatus :: (FileStatus -> Bool) -> FileTest
withStatus p = withStatusM (pure . p)

-- | Like 'withPath' but the supplied function may perform IO.
withPathM :: (FilePath -> IO Bool) -> FileTest
withPathM p = FileTest $ Predicate $ \fs -> p (filepath fs)

-- | Convert a @FilePath -> Bool@ function into a 'FileTest' predicate.
withPath :: (FilePath -> Bool) -> FileTest
withPath p = withPathM (pure . p)

-- | A predicate which is always 'True'. Identity of 'and' style folds.
--
true :: FileTest
true = FileTest $ Predicate $ const (pure True)

-- | A predicate which is always 'False'. Identity of 'or' style folds.
--
false :: FileTest
false = FileTest $ Predicate $ const (pure False)

--------------------
-- Global properties
--------------------

-- Note: these are all boolean predicates, therefore, named with "is", "has",
-- prefix.

-- NOTE: This could be (Path -> IO Bool) type as we will never combine this
-- with anything else. But as a FileTest the same predicate can be used with
-- either "test" or "testl" to execute the predicate.

-- XXX Rename to doesItExist or pathExists

-- | True if the path exists. In case of symlink whether it tests the link file
-- or the file pointed to by it depends on whether you use 'test' or 'testl' to
-- execute the predicate.
--
-- Like coreutil @test -e file@
doesExist :: FileTest
doesExist = withStatus (const True)

{-# DEPRECATED isExisting "Use doesExist instead." #-}
isExisting :: FileTest
isExisting = doesExist

---------------
-- Type of file
---------------

-- | True if file is a directory.
--
-- Like @test -d file@
isDir :: FileTest
isDir = withStatus Files.isDirectory

-- | True if file is a regular file.
--
-- Like coreutil @test -f file@
isFile :: FileTest
isFile = withStatus Files.isRegularFile

-- NOTE: On Windows true if FILE_ATTRIBUTE_REPARSE_POINT is set.

-- | True if path is a symbolic link. This is meaningful only when 'testl' is
-- used, in case of 'test' it always returns false.
--
-- Like coreutil @test -h/-L file@
isSymLink :: FileTest
isSymLink = withStatus Files.isSymbolicLink

-- Note: Device files are supported in Windows/NTFS.

-- | True if file is a block special file.
--
-- Like the coreutil @test -b file@.
--
-- Always false on Windows.
--
isBlockDevice :: FileTest
isBlockDevice = withStatus Files.isBlockDevice

-- | True if is a character special file.
--
-- Like @test -c file@.
--
-- Always false on Windows.
--
isCharDevice :: FileTest
isCharDevice = withStatus Files.isCharacterDevice

-- Note: Named pipes are supported in Windows/NTFS.

-- | True if file is a named pipe (FIFO).
--
-- Like coreutil @test  -p file@.
--
-- Always false on Windows.
--
isPipe :: FileTest
isPipe = withStatus Files.isNamedPipe

-- | True if file is a socket.
--
-- Like coreutil @test -S file@.
--
-- Always false on Windows.
--
isSocket :: FileTest
isSocket = withStatus Files.isSocket

{-
-- | True if the file whose file descriptor number is
-- file_descriptor is open and is associated with a terminal.
--
-- Like coreutil @test -t file_descriptor@
--
-- /Unimplemented/
isTerminalFD :: FileTest
isTerminalFD = undefined
-}

---------------
-- Permissions
---------------

-- | True if the file has specified permission mode.
--
{-# INLINE hasMode #-}
hasMode :: FileMode -> FileTest
hasMode mode = withStatus (\st -> (Files.fileMode st .&. mode) == mode)

-- | True if the owner (u) has read (r) permission.
--
hasOwnerRead :: FileTest
hasOwnerRead = hasMode Files.ownerReadMode

-- | True if the owner (u) has write (w) permission.
--
hasOwnerWrite :: FileTest
hasOwnerWrite = hasMode Files.ownerWriteMode

-- | True if the owner (u) has execute (x) permission.
--
hasOwnerExec :: FileTest
hasOwnerExec = hasMode Files.ownerExecuteMode

-- | True if the group (g) has read (r) permission.
--
hasGroupRead :: FileTest
hasGroupRead = hasMode Files.groupReadMode

-- | True if the group (g) has write (w) permission.
--
hasGroupWrite :: FileTest
hasGroupWrite = hasMode Files.groupWriteMode

-- | True if the group (g) has execute (x) permission.
--
hasGroupExec :: FileTest
hasGroupExec = hasMode Files.groupExecuteMode

-- | True if others (o) have read (r) permission.
--
hasOtherRead :: FileTest
hasOtherRead = hasMode Files.otherReadMode

-- | True if others (o) have write (w) permission.
--
hasOtherWrite :: FileTest
hasOtherWrite = hasMode Files.otherWriteMode

-- | True if others (o) have execute (x) permission.
--
hasOtherExec :: FileTest
hasOtherExec = hasMode Files.otherExecuteMode

-- | True if the file has set user ID flag is set.
--
-- Like coreutil @test -u file@
--
-- Always false on Windows.
--
hasSetUid :: FileTest
hasSetUid = hasMode Files.setUserIDMode

-- | True if the file has set group ID flag is set.
--
-- Like coreutil @test -g file@
--
-- Always false on Windows.
--
hasSetGid :: FileTest
hasSetGid = hasMode Files.setGroupIDMode

-- | True if file has sticky bit is set.
--
-- Like coreutil @test -k file@
--
-- Always false on Windows.
--
-- /Unimplemented/
hasStickyBit :: FileTest
hasStickyBit = undefined

-- Note: Hard links are supported in Windows/NTFS.
{-
-- | True if file1 and file2 exist and have the same device id and inode.
--
-- Like coreutil @test file1 -ef file2@.
--
-- /Unimplemented/
isHardLinkTo :: FilePath -> FileTest
isHardLinkTo = undefined
-}

-----------------------------------
-- Time
-----------------------------------

-- NOTES: NominalDiffTime is actually time duration in seconds possibly
-- fractional. In contrast to DiffTime it ignores the leap seconds, so it is
-- the actual elapsed time duration. A more specific and intuitive name for
-- this would be NominalDiffSeconds or Duration or simply Seconds.
--
-- We could use (Integer -> NominalDiffTime) in the APIs below and that would
-- disallow nesting of units e.g. (seconds (minutes 5)). But that is unlikely
-- error and NominalDiffTime allows fractional seconds which is a good thing.

-- | Time duration in seconds.
--
-- >>> modifiedOlderThan (seconds 30)
--
seconds :: NominalDiffTime -> NominalDiffTime
seconds = id

-- | Time duration in minutes.
--
-- >>> modifiedWithin (minutes 5)
--
minutes :: NominalDiffTime -> NominalDiffTime
minutes n = n * 60

-- | Time duration in hours.
hours :: NominalDiffTime -> NominalDiffTime
hours n = n * 3600

-- | Time duration in days.
--
-- >>> accessedOlderThan (days 1)
--
days :: NominalDiffTime -> NominalDiffTime
days n = n * 86400

-----------------------------------
-- Comparing times
-----------------------------------

timeSatisfiesWith :: (FileStatus -> POSIXTime) -> (POSIXTime -> Bool) -> FileTest
timeSatisfiesWith getFileTime p = withStatus (p . getFileTime)

-- | True if the modification time satisfies the supplied predicate.
--
-- >>> modifyTime (< someTime)
--
modifyTime :: (POSIXTime -> Bool) -> FileTest
modifyTime = timeSatisfiesWith Files.modificationTimeHiRes

-- | True if modified before the given timestamp.
modifiedBefore :: POSIXTime -> FileTest
modifiedBefore t = modifyTime (< t)

-- | True if modified after the given timestamp.
modifiedAfter :: POSIXTime -> FileTest
modifiedAfter t = modifyTime (> t)

-- XXX Use Path instead of filepath.
-- XXX no dereference option for symlinks?
timeComparedToWith ::
       (FileStatus -> POSIXTime)
    -> FilePath
    -> (POSIXTime -> POSIXTime -> Bool)
    -> FileTest
timeComparedToWith getFileTime path cmp =
  withStatusM $ \st -> do
    st1 <- Files.getFileStatus path
    pure $ cmp (getFileTime st) (getFileTime st1)

-- Works on both Posix and Windows via unix-compat.
-- XXX no dereference option for symlinks?

-- | Compare the modification time of the file with the modification time of
-- another file.
--
-- If the file path is a symlink dereferences it.
modifyTimeComparedTo ::
    FilePath -> (POSIXTime -> POSIXTime -> Bool) -> FileTest
modifyTimeComparedTo = timeComparedToWith Files.modificationTimeHiRes

-- | Compare modification time with another file.
--
-- >>> modifiedBeforeFile "ref.txt"
--
modifiedBeforeFile :: FilePath -> FileTest
modifiedBeforeFile path = modifyTimeComparedTo path (<)

modifiedAfterFile  :: FilePath -> FileTest
modifiedAfterFile path = modifyTimeComparedTo path (>)

-- | Compare the access time of the file with the access time of
-- another file.
--
-- If the file path is a symlink dereferences it.
accessTimeComparedTo ::
    FilePath -> (POSIXTime -> POSIXTime -> Bool) -> FileTest
accessTimeComparedTo = timeComparedToWith Files.accessTimeHiRes

-- | Compare access time with another file.
accessedBeforeFile :: FilePath -> FileTest
accessedBeforeFile path = accessTimeComparedTo path (<)

accessedAfterFile  :: FilePath -> FileTest
accessedAfterFile path = accessTimeComparedTo path (>)

-----------------------------------
-- Comparing age with other files
-----------------------------------

getLocalTime :: IO TimeSpec
getLocalTime = fromAbsTime <$> getTime Realtime

timeSpecToPOSIX :: TimeSpec -> POSIXTime
timeSpecToPOSIX (TimeSpec s ns) =
  fromIntegral s + fromIntegral ns / 1_000_000_000

ageOfWith :: (FileStatus -> POSIXTime) -> FileStatus -> IO NominalDiffTime
ageOfWith getFileTime st = do
  now <- timeSpecToPOSIX <$> getLocalTime
  pure (now - getFileTime st)

ageSatisfiesWith
  :: (FileStatus -> POSIXTime)
  -> (NominalDiffTime -> Bool)
  -> FileTest
ageSatisfiesWith getFileTime p =
  withStatusM $ \st -> p <$> ageOfWith getFileTime st

-- | True if access age satisfies the predicate.
--
-- >>> accessAge (> minutes 10)
--
accessAge :: (NominalDiffTime -> Bool) -> FileTest
accessAge = ageSatisfiesWith Files.accessTimeHiRes

accessedWithin :: NominalDiffTime -> FileTest
accessedWithin dt = accessAge (< dt)

accessedOlderThan :: NominalDiffTime -> FileTest
accessedOlderThan dt = accessAge (> dt)

-- | True if modify age satisfies the predicate.
--
-- >>> modifyAge (> minutes 10)
--
modifyAge :: (NominalDiffTime -> Bool) -> FileTest
modifyAge = ageSatisfiesWith Files.modificationTimeHiRes

-- | True if modified within the given duration.
--
-- >>> modifiedWithin (minutes 5)
--
modifiedWithin :: NominalDiffTime -> FileTest
modifiedWithin dt = modifyAge (< dt)

-- | True if modified older than the given duration.
modifiedOlderThan :: NominalDiffTime -> FileTest
modifiedOlderThan dt = modifyAge (> dt)

{-
-- See https://unix.stackexchange.com/questions/91197/how-to-find-creation-date-of-file
hasCreateAge :: (POSIXTime -> POSIXTime -> Bool) -> Double -> FileTest
hasCreateAge = undefined
-}

-----------------------------------
-- Absolute size
-- Comparing size with other files
-----------------------------------

-- XXX Should use Int or Int64?

getSize :: FileStatus -> Int64
getSize st = let COff sz = Files.fileSize st in sz

-- | True if file size satisfies the supplied predicate.
--
-- >>> size (> 1024)
--
size :: (Int64 -> Bool) -> FileTest
size cmp = withStatus (\st -> cmp (getSize st))

-- | True if file size is greater than the given size in bytes.
--
-- >>> largerThan 4096
--
largerThan :: Int64 -> FileTest
largerThan n = size (> n)

smallerThan :: Int64 -> FileTest
smallerThan n = size (< n)

sizeEquals :: Int64 -> FileTest
sizeEquals n = size (== n)

-- | True if file is non-empty.
--
-- Like coreutil @test -s file@
--
nonEmpty :: FileTest
nonEmpty = size (> 0)

-----------------------------------
-- Comparing size with other files
-----------------------------------

-- XXX no dereference option for symlinks?

-- | Compare the file size with the size of another file using supplied
-- comparison function. The first argument of the comparison function is the
-- size of the file being tested and the second argument is the size of the
-- file in the argument.
--
-- >>> sizeComparedTo (>) "abc.txt"
--
-- If the supplied file path is a symlink dereferences it.
sizeComparedTo :: FilePath -> (Int64 -> Int64 -> Bool) -> FileTest
sizeComparedTo path rel =
  withStatusM $ \st -> do
    st1 <- Files.getFileStatus path
    pure $ rel (getSize st) (getSize st1)

largerThanFile :: FilePath -> FileTest
largerThanFile path = sizeComparedTo path (>)

smallerThanFile :: FilePath -> FileTest
smallerThanFile path = sizeComparedTo path (<)

sameSizeAs :: FilePath -> FileTest
sameSizeAs path = sizeComparedTo path (==)
