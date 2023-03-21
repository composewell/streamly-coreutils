-- |
-- Module      : Streamly.Coreutils.FileTest
-- Copyright   : (c) 2021 Composewell Technologies
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- A predicate DSL to filter files based on their properties.
--
-- Combine predicates for the same file and test those in one go for good
-- performance.
--
-- This is just a convenience wrapper on top of the POSIX functions.  It covers
-- the functionality provided by the GNU coreutils @test@ utility.  String
-- testing is not provided as it can be trivially done using built-in Haskell
-- functionality. That leaves only file test routines. The routines provided in
-- this module have a one to one correspondence with the @test@ utility.

-- Design Notes:
--
-- "unix" package provides accessor functions for FileStatus.  Why not get the
-- FileStatus and use those directly for testing properties of a file?
-- Predicates are easier to understand and can wrap high level logic e.g.
-- compare the file size with the size of another file. Predicates are easy to
-- combine efficiently without worrying about passing around the FileStatus
-- structure or accessing it multiple times.  It is easier to make predicates
-- OS independent.
--
-- XXX This is for POSIX but some of it could be applicable to Windows as well.
-- Should we create a platform independent abstraction too?

module Streamly.Coreutils.FileTest
    (
    -- * File Test Predicate Type
      FileTest

    -- * Primitives
    , predicate
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
    , isDir
    , isFile
    , isSymLink
#if defined(CABAL_OS_LINUX)
    , testFD

    -- * Predicates

    -- ** General
    , isExisting
    , isHardLinkOf

    -- ** File Type

    , isCharDevice
    , isBlockDevice
    , isPipe
    , isSocket
    , isTerminalFD

    -- ** File Permissions
    -- *** For current user
    , isReadable
    , isWritable
    , isExecutable

    -- *** Mode check
    -- , mkMode -- quasiquoters?
    -- , hasMode
    , hasSticky
    , hasSetUID
    , hasSetGID

    -- ** File Ownership
    , isOwnedByEUID
    , isOwnedByEGID

    --, isOwnedByUser
    --, isOwnedByUid
    --, isOwnedByGroup
    --, isOwnedByGid

    -- ** File size
    , hasSize
    , cmpSize
    -- XXX Need convenient size units and conversions (e.g. kB 1, kiB 1, mB 2)


    -- ** File times
    -- XXX Need convenient time units and conversions (e.g. sec 5,
    -- "2022-01-01")

    -- *** File age
    , hasAccessAge
    , hasModifyAge
    -- , hasCreateAge

    -- *** File timestamp
    , hasModifyTime

    -- *** Compare timestamps with file
    , cmpModifyTime
#endif
    )
where

import Control.Exception (catch, throwIO)
import Control.Monad (when)
import Data.Bits ((.&.))
import Data.Int (Int64)
import Data.Time.Clock.POSIX (POSIXTime)
import Foreign.C.Error (Errno(..), eNOENT)
import GHC.IO.Exception (IOException(..), IOErrorType(..))
#if defined(CABAL_OS_LINUX)
import System.Posix.Types (Fd, COff(..), FileMode)
import System.Posix.Files (FileStatus)
import qualified System.Posix.User as User
import qualified System.Posix.Files as Files
#endif

#if defined(CABAL_OS_WINDOWS)
import System.PosixCompat.Files (FileStatus)
import qualified System.PosixCompat.Files as Files
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
-- We do not provide a Semigroup instance for the `and` operation because then
-- we either do not have a similar op for `or` operation, or we need a newtype
-- for that. So we just do not have it.

-- | A predicate type for testing boolean statements about a file.
--
newtype FileTest =
    FileTest (Predicate IO FileStatus)

-- | A boolean @and@ function for combining two 'FileTest' predicates.
--
and_ :: FileTest -> FileTest -> FileTest
and_ (FileTest (Predicate p)) (FileTest (Predicate q)) =
    FileTest (Predicate $ \a -> (&&) <$> p a <*> q a)

-- | A boolean @or@ function for combining two 'FileTest' predicates.
--
or_ :: FileTest -> FileTest -> FileTest
or_ (FileTest (Predicate p)) (FileTest (Predicate q)) =
    FileTest (Predicate $ \a -> (||) <$> p a <*> q a)

-- | A boolean @and@ for combining a list of 'FileTest' predicates.
--
-- >>> and = foldl and_ true
--
and :: [FileTest] -> FileTest
and = foldl and_ true

-- | A boolean @and@ for combining a list of 'FileTest' predicates.
--
-- >>> or = foldl or_ false
--
or :: [FileTest] -> FileTest
or = foldl or_ false

-- | A boolean @not@ function for combining two 'FileTest' predicates.
--
not_ :: FileTest -> FileTest
not_ (FileTest (Predicate p)) = FileTest (Predicate (fmap not . p))

-- XXX Use a byte array instead of string filepath.
--
-- | Apply a predicate to a 'FilePath'. Returns 'True' if the file exists and
-- the predicate is 'True' otherwise returns 'False'.
--
-- Fails with exception if the directory entry of the file is not accessible
-- due to lack of permissions in the path.
--
test :: FilePath -> FileTest -> IO Bool
test path (FileTest (Predicate f)) =
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

-- | Apply a predicate to 'FileStatus'.
apply :: FileStatus -> FileTest -> IO Bool
apply st (FileTest (Predicate f)) = f st

-- XXX Use Handle instead
-- | Like 'test' but uses a file descriptor instead of file path.
#if defined(CABAL_OS_LINUX)
testFD :: Fd -> FileTest -> IO Bool
testFD fd (FileTest (Predicate f)) = Files.getFdStatus fd >>= f
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
-- >>> true = predicate (const True)
--
true :: FileTest
true = predicate (const True)

-- | A predicate which is always 'False'.
--
-- >>> false = predicate (const False)
--
false :: FileTest
false = predicate (const False)

--------------------
-- Global properties
--------------------

-- XXX Drop the "is" from the predicates, like in parsers.

-- | True if file exists.
--
-- Like coreutil @test -e file@
isExisting :: FileTest
isExisting = predicate (const True)

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

-- | True if file is a symbolic link.
--
-- Like coreutil @test -h/-L file@
isSymLink :: FileTest
isSymLink = predicate Files.isSymbolicLink

-- | True if file is a block special file.
--
-- Like the coreutil @test -b file@.
isBlockDevice :: FileTest
isBlockDevice = predicate Files.isBlockDevice

-- | True if is a character special file.
--
-- Like @test -c file:
isCharDevice :: FileTest
isCharDevice = predicate Files.isCharacterDevice

-- | True if file is a named pipe (FIFO).
--
-- Like coreutil @test  -p file@
isPipe :: FileTest
isPipe = predicate Files.isNamedPipe

-- | True if file is a socket.
--
-- Like coreutil @test -S file@
isSocket :: FileTest
isSocket = predicate Files.isSocket

-- | True if the file whose file descriptor number is
-- file_descriptor is open and is associated with a terminal.
--
-- Like coreutil @test -t file_descriptor@
--
-- /Unimplemented/
isTerminalFD :: FileTest
isTerminalFD = undefined

---------------
-- Permissions
---------------

-- | True if the file has specified permission mode.
--
#if defined(CABAL_OS_LINUX)
hasMode :: FileMode -> FileTest
hasMode mode = predicate (\st -> (Files.fileMode st .&. mode) == mode)


-- | True if the file has set user ID flag is set.
--
-- Like coreutil @test -u file@
hasSetUID :: FileTest
hasSetUID = hasMode Files.setUserIDMode

-- | True if the file has set group ID flag is set.
--
-- Like coreutil @test -g file@
hasSetGID :: FileTest
hasSetGID = hasMode Files.setGroupIDMode

-- | True if file has sticky bit is set.
--
-- Like coreutil @test -k file@
--
-- /Unimplemented/
hasSticky :: FileTest
hasSticky = undefined

-- | True if the file owner matches the effective user id of this process.
--
-- Like coreutil @test -O file@
--
-- /Unimplemented/

isOwnedByEUID  :: FileTest
isOwnedByEUID = predicateM $ \st ->
    (Files.fileOwner st ==) <$> User.getEffectiveUserID

-- | True if file exists and its group matches the effective
-- group id of this process.
--
-- Like coreutil @test -G file@
--
-- /Unimplemented/
isOwnedByEGID :: FileTest
isOwnedByEGID = predicateM $ \st ->
    (Files.fileGroup st ==) <$> User.getEffectiveGroupID

hasPermissions :: (FileMode, FileMode, FileMode) -> FileTest
hasPermissions (user, group, other) = predicateM $ \st -> do
    isOwner <- apply st isOwnedByEUID
    let checkMode = apply st . hasMode
    if isOwner
    then checkMode user
    else do
        isGroup <- apply st isOwnedByEGID
        if isGroup
        then checkMode group
        else checkMode other

-- | True if the file is readable for the current user.
--
-- Like coreutil @test -r file@
--
-- /Pre-release/
isReadable :: FileTest
isReadable =
    hasPermissions
        (
          Files.ownerReadMode
        , Files.groupReadMode
        , Files.otherReadMode
        )

-- | True if the file is writable for the current user.
--
-- Note that the file is not writable on a read-only file system even if this
-- test indicates true.
--
-- Like coreutil @test -w file@
--
-- /Pre-release/
isWritable :: FileTest
isWritable =
    hasPermissions
        (
          Files.ownerWriteMode
        , Files.groupWriteMode
        , Files.otherWriteMode
        )

-- | True if the file is executable for the current user.
--
-- Like coreutil @test -x file@
--
-- /Pre-release/
isExecutable :: FileTest
isExecutable =
    hasPermissions
        (
          Files.ownerExecuteMode
        , Files.groupExecuteMode
        , Files.otherExecuteMode
        )

------------------------------
-- Comparing with other files
------------------------------

compareTime ::
       (FileStatus -> POSIXTime)
    -> (POSIXTime -> POSIXTime -> Bool)
    -> POSIXTime
    -> FileTest
compareTime getFileTime cmp t = predicate (\st -> getFileTime st `cmp` t)

-- | Compare the modification time of the file with a timestamp.
hasModifyTime ::
    (POSIXTime -> POSIXTime -> Bool) -> POSIXTime -> FileTest
hasModifyTime = compareTime Files.modificationTimeHiRes

compareTimeWith ::
       (FileStatus -> POSIXTime)
    -> (POSIXTime -> POSIXTime -> Bool)
    -> FilePath
    -> FileTest
compareTimeWith getFileTime cmp path = predicateM $ \st -> do
    st1 <- Files.getFileStatus path
    apply st $ compareTime getFileTime cmp (getFileTime st1)

-- | Compare the modification time of the file with the modification time of
-- another file.
cmpModifyTime ::
    (POSIXTime -> POSIXTime -> Bool) -> FilePath -> FileTest
cmpModifyTime = compareTimeWith Files.modificationTimeHiRes

-- | True if file1 and file2 exist and have the same device id and inode.
--
-- Like coreutil @test file1 -ef file2@.
isHardLinkOf :: FilePath -> FileTest
isHardLinkOf = undefined

getLocalTime :: IO TimeSpec
getLocalTime = fromAbsTime <$> getTime Realtime

compareAge ::
       (FileStatus -> POSIXTime)
    -> (POSIXTime -> POSIXTime -> Bool)
    -> Double
    -> FileTest
compareAge getFileTime cmp ageSec = predicateM $ \st -> do
    when (ageSec < 0) $ error "compareAge: age cannot be negative"

    ts <- getLocalTime
    let now = timespecToPosixTime ts
        age = doubleToPosixTime ageSec
    apply st $ compareTime getFileTime (flip cmp) (now - age)

    where

    timespecToPosixTime (TimeSpec s ns) =
        fromIntegral s + fromIntegral ns * 1E-9

    -- XXX handle negative double value?
    doubleToPosixTime :: Double -> POSIXTime
    doubleToPosixTime sec =
        let s = floor sec
            ns = round $ (sec - fromIntegral s) * 1E9
         in timespecToPosixTime (TimeSpec s ns)

-- XXX Use a -> a -> Bool instead
hasAccessAge :: (POSIXTime -> POSIXTime -> Bool) -> Double -> FileTest
hasAccessAge = compareAge Files.accessTimeHiRes

hasModifyAge :: (POSIXTime -> POSIXTime -> Bool) -> Double -> FileTest
hasModifyAge = compareAge Files.modificationTimeHiRes
#endif

{-
-- See https://unix.stackexchange.com/questions/91197/how-to-find-creation-date-of-file
hasCreateAge :: (POSIXTime -> POSIXTime -> Bool) -> Double -> FileTest
hasCreateAge = undefined
-}

-- XXX Should use Int or Int64?

#if !defined (CABAL_OS_WINDOWS)
getSize :: FileStatus -> Int64
getSize st = let COff size = Files.fileSize st in size


-- | Compare the file size with the supplied size.
--
-- Coreutil @test -s file@ would be @hasSize (/=) 0@
--
hasSize :: (Int64 -> Int64 -> Bool) -> Int64 -> FileTest
hasSize cmp n = predicate (\st -> getSize st `cmp` n)

-- | Compare the file size with the size of another file.
--
cmpSize :: (Int64 -> Int64 -> Bool) -> FilePath -> FileTest
cmpSize cmp path = predicateM $ \st -> do
    st1 <- Files.getFileStatus path
    apply st $ hasSize cmp (getSize st1)
#endif