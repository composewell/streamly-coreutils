-- |
-- Module      : Streamly.Coreutils.FileTest
-- Copyright   : (c) 2021 Composewell Technologies
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- A predicate DSL to filter files based on their properties. This module is
-- portable across Linux, macOS and Windows platforms. For Posix specific
-- APIs please see "Streamly.Coreutils.FileTest.Posix".
--
-- For good performance, combine multiple predicates for the same file and test
-- those in one go.
--
-- This module covers a subset of the functionality provided by the GNU
-- coreutils @test@ utility.  String testing is not provided as it can be
-- trivially done using built-in Haskell functionality. That leaves only file
-- test routines. The routines provided in this module have a one to one
-- correspondence with the @test@ utility.

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
    -- TODO: Create Streamly.Coreutils.FileTest.Posix for Posix specific
    -- functions.
#if !defined(CABAL_OS_WINDOWS)
    , testFD
#endif

    -- * Predicates

    -- ** General
    , isExisting
#if !defined(CABAL_OS_WINDOWS)
    , isHardLinkOf
#endif

    -- ** File Type
    , isDir
    , isFile
    , isSymLink
#if !defined(CABAL_OS_WINDOWS)
    , isCharDevice
    , isBlockDevice
    , isPipe
    , isSocket
    -- , isTerminalFD
#endif

    -- ** File Permissions
    -- *** For current user
    , isReadable
    , isWritable
    , isExecutable

#if !defined(CABAL_OS_WINDOWS)
    -- *** Mode check
    -- , mkMode -- quasiquoters?
    -- , hasMode
    , hasSticky
    , hasSetUID
    , hasSetGID
#endif

    -- ** File Ownership
    , isOwnedByEUID
#if !defined(CABAL_OS_WINDOWS)
    , isOwnedByEGID
#endif

    --, isOwnedByUser
    --, isOwnedByUid
    --, isOwnedByGroup
    --, isOwnedByGid

    -- ** File size
    -- XXX Need convenient size units and conversions (e.g. kB 1, kiB 1, mB 2)
    , hasSize
    , cmpSize

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
    )
where

import Control.Exception (catch, throwIO)
import Control.Monad (when)
import Data.Bits ((.&.))
import Data.Int (Int64)
import Data.Time.Clock.POSIX (POSIXTime)
import Foreign.C.Error (Errno(..), eNOENT)
import GHC.IO.Exception (IOException(..), IOErrorType(..))

-- XXX Remove the dependency on unix-compat and directory
import System.PosixCompat.Files (FileStatus)
import System.Posix.Types (COff(..), FileMode)
import qualified System.PosixCompat.Files as Files

#if !defined(CABAL_OS_WINDOWS)
import System.Posix.Types (Fd)
import qualified System.Posix.User as User
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

-- | Apply a predicate to a 'FilePath'.
--
-- * Returns 'True' if the file exists and the predicate is 'True'
-- * Returns 'False' if the file does not exist or the predicate is 'False'
-- * Fails with an IO exception if the path to the file is not accessible due
-- to lack of permissions. The exception type can be used to determine the
-- reason for failure.
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
#if !defined(CABAL_OS_WINDOWS)
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

-- Note: these are all boolean predicates, therefore, named with "is", "has",
-- "cmp" prefix.

-- | True if file exists.
--
-- Like coreutil @test -e file@
isExisting :: FileTest
isExisting =
    -- "test" would fail by default if the file does not exist, no other test
    -- is required.
    predicate (const True)

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

#if !defined(CABAL_OS_WINDOWS)
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
#endif

-- XXX This can be done for windows as well using GetSecurityInfo. Should we
-- use that when getting FileStatus in unix-compat?
-- See getFileSecurity in System/Win32/Security.hsc .
--
-- XXX rename this to isOwnedByCurrentUser to make it portable.

-- | True if the file owner matches the effective user id of this process.
--
-- Like coreutil @test -O file@
--
-- /Unimplemented/
isOwnedByEUID  :: FileTest
isOwnedByEUID = predicateM $ \st ->
#if !defined(CABAL_OS_WINDOWS)
    (Files.fileOwner st ==) <$> User.getEffectiveUserID
#else
    predicate (const True)
#endif

-- XXX rename this to isOwnedByCurrentGroup
-- XXX This applies only to POSIX because windows does not have a notion of
-- current group.

-- | True if file exists and its group matches the effective
-- group id of this process.
--
-- Like coreutil @test -G file@
--
-- /Unimplemented/
isOwnedByEGID :: FileTest
isOwnedByEGID = predicateM $ \st ->
#if !defined(CABAL_OS_WINDOWS)
    (Files.fileGroup st ==) <$> User.getEffectiveGroupID
#else
    predicate (const False)
#endif

-- On Widnows we will have to use
-- GetSecurityInfo/GetNamedSecurityInfo/GetEffectiveRightsFromAcl to check
-- permissions properly.
--
-- Also, how to handle CIFS shares. On windows, check ShareMode? On Linux, are
-- the file permissions enough for CIFS?

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

-- XXX What does "isWritable" mean on windows? Windows has separate write and
-- modify permissions. We can use two separate functions, isWritableData (or
-- just isWritable), isWritableMeta. On unix both will have the same underlying
-- permission.
--
-- We may also need to check if the filesystem/mount is writable, or on windows
-- check the drive type. Or do we need a separate stricter checking API for
-- that? Or do we encode that in a separate mount/drive level API to be checked
-- by the programmer separately.

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

-- XXX What does "isExecutable" mean on windows? For files, unix-compat
-- determines executable on windows based on the file type, which does not
-- sound right, why not use the execute permission? On unix, directories
-- executable mean searchable. On Windows there is a separate attribute for
-- that.
--
-- We should have a separate isSearchable test for directories. And make
-- isExecutable not-applicable/False for directories. But utilities like chmod
-- may still need to use +x for executable. But we can use "+X" or "+S" to mean
-- searchable.

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
--
-- /Unimplemented/
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

{-
-- See https://unix.stackexchange.com/questions/91197/how-to-find-creation-date-of-file
hasCreateAge :: (POSIXTime -> POSIXTime -> Bool) -> Double -> FileTest
hasCreateAge = undefined
-}

-- XXX Should use Int or Int64?

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
