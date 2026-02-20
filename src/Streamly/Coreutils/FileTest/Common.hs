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
-- XXX Need tests for Windows. Especially for file access permissions. How do
-- ACLs affect it? Also file times.
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

module Streamly.Coreutils.FileTest.Common
    (
    -- * File Test Predicate Type
      FileTest

    -- * Primitives
    , predicate
    , predicateM
    , true
    , false

    -- * Predicate Combinators
    , not_
    , and_
    , or_
    , and
    , or

    -- * Running Predicates
    , test
    , testl
    , apply
#if !defined(CABAL_OS_WINDOWS)
    , testFD
    , testHandle
#endif

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
import Data.Time.Clock.POSIX (POSIXTime)
import Data.Time.Clock (NominalDiffTime)
import Foreign.C.Error (Errno(..), eNOENT)
import GHC.IO.Exception (IOException(..), IOErrorType(..))

-- XXX Remove the dependency on unix-compat and directory
import System.PosixCompat.Files (FileStatus)
import System.Posix.Types (COff(..), FileMode)
import qualified System.PosixCompat.Files as Files

#if !defined(CABAL_OS_WINDOWS)
import System.IO (Handle)
import System.Posix.Types (Fd)
#endif

import Prelude hiding (and, or)
import Streamly.Internal.Data.Time.Clock
import Streamly.Internal.Data.Time.Units

newtype Predicate m a =
    Predicate (a -> m Bool)

-- $setup
-- >>> import Prelude hiding (or, and)

-- Naming Notes: Named FileTest rather than "Test" to be more explicit and
-- specific. The command can also be named fileTest or testFile.
--
-- We do not provide a Semigroup/Monoid instance, though it provides a
-- convenient <> for the `and` operation but then we need a newtype wrapper for
-- the "or" operation. Also, the generic foldMap or mconcat provided by Monoids
-- are of limited use in this case.

-- XXX Supply the FilePath as well.

-- | A predicate type for testing boolean statements about a file.
--
newtype FileTest =
    FileTest (Predicate IO FileStatus)

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
-- Note that 'or_ uses a single @stat@ system call for both the tests,
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

-- We can also use &&_, ||_ operators but probably not worth it.
infixr 3 `and_`
infixr 2 `or_`

-- | A boolean @and@ for combining a list of 'FileTest' predicates.
--
-- >>> and = foldl and_ true
--
and :: [FileTest] -> FileTest
and = foldl' and_ true

-- | A boolean @and@ for combining a list of 'FileTest' predicates.
--
-- >>> or = foldl or_ false
--
or :: [FileTest] -> FileTest
or = foldl' or_ false

-- | A boolean @not@ function for combining two 'FileTest' predicates.
--
not_ :: FileTest -> FileTest
not_ (FileTest (Predicate p)) = FileTest (Predicate (fmap not . p))

-- XXX Use Path instead of filepath.
--
-- XXX We should make the system calls only once and only if needed, and pass
-- the results to the subsequent tests. File status is always required and we
-- can fetch it at the beginning but things like process uid should be fetched
-- only if the predicate needs it and then pass it around for any other
-- predicates that may need it. But that will require monadic composition.

-- | Apply a predicate to a 'FilePath', if the path is a symlink uses the link
-- target and not the link itself. See 'testl' for testing the link itself.
--
-- * 'test' returns 'True' if the file exists and the predicate is 'True'
-- * Returns 'False' if the file does not exist or the predicate is 'False'
-- * Fails with an IO exception if the path to the file is not accessible due
-- to lack of permissions. The exception type can be used to determine the
-- reason for failure.
--  * test 'isSymlink' always returns false.
--  * test 'doesExist' returns false if the path is symlink but it does not
--  point to an existing file.
--
test :: FilePath -> FileTest -> IO Bool
test path (FileTest (Predicate f)) =
    -- See unix-compat package for the meaning of unix fields on windows.
    -- Note: getFileStatus dereferences symlinks.
    (Files.getFileStatus path >>= f) `catch` eatENOENT

    where

    isENOENT e =
        case e of
            IOError
                { ioe_type = NoSuchThing
                , ioe_errno = Just ioe
                } -> Errno ioe == eNOENT
            _ -> False

    eatENOENT e = if isENOENT e then return False else throwIO e

-- | Like 'test' but uses the path and not the link target if the path is a
-- symlink.
--
--  * 'isSymlink' returns true if path is a symlink, false otherwise.
--  * 'doesExist' returns true if the link exists irrespective of whether it
--  points to an existing file.
--  * Predicates related to file permission mode bits are meaningless, and
--  should not be used.
--  * Predicates related to file owner, group, size, time stamps are relevant.
--
testl :: FilePath -> FileTest -> IO Bool
testl path (FileTest (Predicate f)) =
    (Files.getSymbolicLinkStatus path >>= f) `catch` eatENOENT

    where

    isENOENT e =
        case e of
            IOError
                { ioe_type = NoSuchThing
                , ioe_errno = Just ioe
                } -> Errno ioe == eNOENT
            _ -> False

    eatENOENT e = if isENOENT e then return False else throwIO e

-- XXX rename to testStat?

-- | Apply a predicate to 'FileStatus'.
apply :: FileStatus -> FileTest -> IO Bool
apply st (FileTest (Predicate f)) = f st

-- XXX rename to testFd

#if !defined(CABAL_OS_WINDOWS)
-- | Like 'test' but uses a file descriptor instead of file path.
testFD :: Fd -> FileTest -> IO Bool
-- getFdStatus is not implemented for Windows in unix-compat.
testFD fd (FileTest (Predicate f)) = Files.getFdStatus fd >>= f

testHandle :: Handle -> FileTest -> IO Bool
testHandle = undefined
#endif

-- | Convert a @FileStatus -> Bool@ type of function to a 'FileTest' predicate.
predicate :: (FileStatus -> Bool) -> FileTest
predicate p = FileTest (Predicate (pure . p))

-- | Convert a @FileStatus -> IO Bool@ type of function to a 'FileTest'
-- predicate.
predicateM :: (FileStatus -> IO Bool) -> FileTest
predicateM p = FileTest (Predicate p)

-- | A predicate which is always 'True'.
--
true :: FileTest
true = predicate (const True)

-- | A predicate which is always 'False'.
--
false :: FileTest
false = predicate (const False)

--------------------
-- Global properties
--------------------

-- Note: these are all boolean predicates, therefore, named with "is", "has",
-- "cmp" prefix.

-- NOTE: This could be (Path -> IO Bool) type as we will never combine this
-- with anything else. But as a FileTest the same predicate can be used with
-- either "test" or "testl".

-- >>> doesExist = true

-- | True if the path exists. In case of symlink whether it tests the link file
-- or the file pointed to by it depends on whether you use 'test' or 'testl' to
-- execute the predicate.
--
-- Note: 'doesExist' itself performs no check. File existence is determined
-- by 'test' or 'testl', which return False if the path does not exist.
--
-- Like coreutil @test -e file@
doesExist :: FileTest
doesExist = true

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
isDir = predicate Files.isDirectory

-- | True if file is a regular file.
--
-- Like coreutil @test -f file@
isFile :: FileTest
isFile = predicate Files.isRegularFile

-- NOTE: On Windows true if FILE_ATTRIBUTE_REPARSE_POINT is set.

-- | True if path is a symbolic link. This is meaningful only when 'testl' is
-- used, in case of 'test' it always returns false.
--
-- Like coreutil @test -h/-L file@
isSymLink :: FileTest
isSymLink = predicate Files.isSymbolicLink

-- Note: Device files are supported in Windows/NTFS.

-- | True if file is a block special file.
--
-- Like the coreutil @test -b file@.
--
-- Always false on Windows.
--
isBlockDevice :: FileTest
isBlockDevice = predicate Files.isBlockDevice

-- | True if is a character special file.
--
-- Like @test -c file@.
--
-- Always false on Windows.
--
isCharDevice :: FileTest
isCharDevice = predicate Files.isCharacterDevice

-- Note: Named pipes are supported in Windows/NTFS.

-- | True if file is a named pipe (FIFO).
--
-- Like coreutil @test  -p file@.
--
-- Always false on Windows.
--
isPipe :: FileTest
isPipe = predicate Files.isNamedPipe

-- | True if file is a socket.
--
-- Like coreutil @test -S file@.
--
-- Always false on Windows.
--
isSocket :: FileTest
isSocket = predicate Files.isSocket

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
hasMode mode = predicate (\st -> (Files.fileMode st .&. mode) == mode)

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
timeSatisfiesWith getFileTime p = predicate (p . getFileTime)

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
    -> (POSIXTime -> POSIXTime -> Bool)
    -> FilePath
    -> FileTest
timeComparedToWith getFileTime cmp path =
  predicateM $ \st -> do
    st1 <- Files.getFileStatus path
    pure $ cmp (getFileTime st) (getFileTime st1)

-- Works on both Posix and Windows via unix-compat.
-- XXX no dereference option for symlinks?

-- | Compare the modification time of the file with the modification time of
-- another file.
--
-- If the file path is a symlink dereferences it.
modifyTimeComparedTo ::
    (POSIXTime -> POSIXTime -> Bool) -> FilePath -> FileTest
modifyTimeComparedTo = timeComparedToWith Files.modificationTimeHiRes

-- | Compare modification time with another file.
--
-- >>> modifiedBeforeFile "ref.txt"
--
modifiedBeforeFile :: FilePath -> FileTest
modifiedBeforeFile = modifyTimeComparedTo (<)

modifiedAfterFile  :: FilePath -> FileTest
modifiedAfterFile = modifyTimeComparedTo (>)

-- | Compare the access time of the file with the access time of
-- another file.
--
-- If the file path is a symlink dereferences it.
accessTimeComparedTo ::
    (POSIXTime -> POSIXTime -> Bool) -> FilePath -> FileTest
accessTimeComparedTo = timeComparedToWith Files.accessTimeHiRes

-- | Compare access time with another file.
accessedBeforeFile :: FilePath -> FileTest
accessedBeforeFile = accessTimeComparedTo (<)

accessedAfterFile  :: FilePath -> FileTest
accessedAfterFile = accessTimeComparedTo (>)

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
  predicateM $ \st -> p <$> ageOfWith getFileTime st

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
size cmp = predicate (\st -> cmp (getSize st))

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
-- >> sizeComparedTo (>) "/x"
--
-- If the supplied file path is a symlink dereferences it.
sizeComparedTo :: (Int64 -> Int64 -> Bool) -> FilePath -> FileTest
sizeComparedTo rel path =
  predicateM $ \st -> do
    st1 <- Files.getFileStatus path
    pure $ rel (getSize st) (getSize st1)

largerThanFile :: FilePath -> FileTest
largerThanFile = sizeComparedTo (>)

smallerThanFile :: FilePath -> FileTest
smallerThanFile = sizeComparedTo (<)

sameSizeAs :: FilePath -> FileTest
sameSizeAs = sizeComparedTo (==)
