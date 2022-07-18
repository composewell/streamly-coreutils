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

    -- * Predicate Combinators
    , neg
    , negM
    , and
    , andM
    , or
    , orM

    -- * Running Predicates
    , test
    , testM
    , testFD

    -- * Predicates
    , predicate

    -- ** General
    , isExisting
    , isHardLinkOf

    -- ** File Type
    , isDir
    , isFile
    , isSymLink
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
import System.Posix.Types (Fd, COff(..), FileMode)
import System.Posix.Files (FileStatus)
import qualified System.Posix.User as User
import qualified System.Posix.Files as Files

import Prelude hiding (and, or)
import Streamly.Internal.Data.Time.Clock
import Streamly.Internal.Data.Time.Units

#if MIN_VERSION_base(4,12,0)
import Data.Functor.Contravariant (Predicate(..))
#else

newtype Predicate a =
    Predicate (a -> Bool)

instance Semigroup (Predicate a) where
    Predicate p1 <> Predicate p2 = Predicate $ \a -> p1 a && p2 a

instance Monoid (Predicate a) where
    mempty = Predicate $ \_ -> True
    mappend = (<>)

#endif

-- $setup
-- >>> import Prelude hiding (or, and)

-- Naming Notes: Named FileTest rather than "Test" to be more explicit and
-- specific. The command can also be named fileTest or testFile.

-- | A predicate type for testing boolean statements about a file.
--
-- The 'Semigroup' instance acts as boolean @&&@. The 'Monoid' instance uses
-- 'True' as 'mempty'.
--
newtype FileTest = FileTest (Predicate FileStatus) deriving (Semigroup, Monoid)

-- | A boolean @or@ function for 'FileTest' predicates.
--
or :: FileTest -> FileTest -> FileTest
FileTest (Predicate p) `or` FileTest (Predicate q) =
    FileTest (Predicate $ \a -> p a || q a)

-- | Like `or` but for monadic predicates.
--
-- >>> orM t1 t2 = pure or <*> t1 <*> t2
--
orM :: IO FileTest -> IO FileTest -> IO FileTest
orM t1 t2 = pure or <*> t1 <*> t2

-- | A boolean @and@ function for 'FileTest' predicates.
--
-- >>> and = (<>)
--
and :: FileTest -> FileTest -> FileTest
and = (<>)

-- | Like `and` but for monadic predicates.
--
-- >>> andM t1 t2 = pure and <*> t1 <*> t2
--
andM :: IO FileTest -> IO FileTest -> IO FileTest
andM t1 t2 = pure and <*> t1 <*> t2

-- Naming notes: I would prefer to use "not" instead of "neg" but this has to
-- be used unqualified to remain short for common use and prelude "not" is also
-- very common so we do not want to conflict with that.
--
-- XXX Should we have an IsBool type class in Data.Bool so that the boolean
-- operations (&&, ||, not) can be overloaded.
--
-- class IsBool a where
--  not :: a -> a
--    (&&) :: a -> a -> a -- and
--    (||) :: a -> a -> a -- or

-- | A boolean @not@ function for 'FileTest' predicates.
--
-- >>> and = (<>)
--
neg :: FileTest -> FileTest
neg (FileTest (Predicate p)) = FileTest (Predicate $ \a -> Prelude.not (p a))

-- | Like `neg` but for monadic predicates.
--
-- >>> negM = fmap neg
--
negM :: IO FileTest -> IO FileTest
negM = fmap neg

-- XXX Use a byte array instead of string filepath.
--
-- | Run a predicate on a 'FilePath'. Returns 'True' if the file exists and
-- the predicate is 'True' otherwise returns 'False'.
--
-- Fails with exception if the directory entry of the file is not accessible
-- due to lack of permissions in the path.
--
test :: FilePath -> FileTest -> IO Bool
test path (FileTest (Predicate f)) =
    (Files.getFileStatus path >>= return . f) `catch` eatENOENT

    where

    isENOENT e =
        case e of
            IOError
                { ioe_type = NoSuchThing
                , ioe_errno = Just ioe
                } -> Errno ioe == eNOENT
            _ -> False

    eatENOENT e = if isENOENT e then return False else throwIO e

-- | Like 'test' but for a monadic predicate.
--
-- >>> testM path t = test path =<< t
--
testM :: FilePath -> IO FileTest -> IO Bool
testM path t = test path =<< t

-- XXX Use Handle instead
-- | Like 'test' but uses a file descriptor instead of file path.
testFD :: Fd -> FileTest -> IO Bool
testFD fd (FileTest (Predicate f)) = Files.getFdStatus fd >>= return . f

-- | Convert a @FileStatus -> Bool@ type of function to a 'FileTest' predicate.
predicate :: (FileStatus -> Bool) -> FileTest
predicate p = FileTest (Predicate p)

--------------------
-- Global properties
--------------------

-- | True if file exists.
--
-- Like coreutil @test -e file@
isExisting :: FileTest
isExisting = FileTest (Predicate (const True))

---------------
-- Type of file
---------------

-- | True if file is a directory.
--
-- Like @test -d file@
isDir :: FileTest
isDir = FileTest (Predicate Files.isDirectory)

-- | True if file is a regular file.
--
-- Like coreutil @test -f file@
isFile :: FileTest
isFile = FileTest (Predicate Files.isRegularFile)

-- | True if file is a symbolic link.
--
-- Like coreutil @test -h/-L file@
isSymLink :: FileTest
isSymLink = FileTest (Predicate Files.isSymbolicLink)

-- | True if file is a block special file.
--
-- Like the coreutil @test -b file@.
isBlockDevice :: FileTest
isBlockDevice = FileTest (Predicate Files.isBlockDevice)

-- | True if is a character special file.
--
-- Like @test -c file:
isCharDevice :: FileTest
isCharDevice = FileTest (Predicate Files.isCharacterDevice)

-- | True if file is a named pipe (FIFO).
--
-- Like coreutil @test  -p file@
isPipe :: FileTest
isPipe = FileTest (Predicate Files.isNamedPipe)

-- | True if file is a socket.
--
-- Like coreutil @test -S file@
isSocket :: FileTest
isSocket = FileTest (Predicate Files.isSocket)

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
hasMode :: FileMode -> Predicate FileStatus
hasMode mode = Predicate (\st -> (Files.fileMode st .&. mode) == mode)

-- | True if the file has set user ID flag is set.
--
-- Like coreutil @test -u file@
hasSetUID :: FileTest
hasSetUID = FileTest $ hasMode Files.setUserIDMode

-- | True if the file has set group ID flag is set.
--
-- Like coreutil @test -g file@
hasSetGID :: FileTest
hasSetGID = FileTest $ hasMode Files.setGroupIDMode

-- | True if file has sticky bit is set.
--
-- Like coreutil @test -k file@
--
-- /Unimplemented/
hasSticky :: FileTest
hasSticky = undefined
    --FileTest (Predicate (\st -> Files.fileMode st == Files.stickyMode))

hasPermissions :: (FileMode, FileMode, FileMode) -> FilePath -> IO FileTest
hasPermissions (user, group, other) path = do
    -- XXX We are stating the file twice, ideally we should do it only
    -- once. Maybe we can use monadic predicates to avoid that.
    isOwner <- testM path isOwnedByEUID
    if isOwner
    then return $ FileTest $ hasMode user
    else do
        isGroup <- testM path isOwnedByEGID
        if isGroup
        then return $ FileTest $ hasMode group
        else return $ FileTest $ hasMode other

-- XXX It's odd to use the filepath twice to use this predicate.
-- e.g. testM path (isReadable path)

-- | True if the file is readable for the current user.
--
-- Like coreutil @test -r file@
--
-- /Pre-release/
isReadable :: FilePath -> IO FileTest
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
isWritable :: FilePath -> IO FileTest
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
isExecutable :: FilePath -> IO FileTest
isExecutable =
    hasPermissions
        (
          Files.ownerExecuteMode
        , Files.groupExecuteMode
        , Files.otherExecuteMode
        )

-- | True if the file owner matches the effective user id of this process.
--
-- Like coreutil @test -O file@
--
-- /Unimplemented/
isOwnedByEUID  :: IO FileTest
isOwnedByEUID =
    FileTest . Predicate . f <$> User.getEffectiveUserID

    where

    f euid st = Files.fileOwner st == euid

-- | True if file exists and its group matches the effective
-- group id of this process.
--
-- Like coreutil @test -G file@
--
-- /Unimplemented/
isOwnedByEGID :: IO FileTest
isOwnedByEGID =
    FileTest . Predicate . f <$> User.getEffectiveGroupID

    where

    f guid st = Files.fileGroup st == guid

------------------------------
-- Comparing with other files
------------------------------

compareTime ::
       (FileStatus -> POSIXTime)
    -> (POSIXTime -> POSIXTime -> Bool)
    -> POSIXTime
    -> FileTest
compareTime getFileTime cmp t =
    FileTest (Predicate (\st -> getFileTime st `cmp` t))

-- | Compare the modification time of the file with a timestamp.
hasModifyTime ::
    (POSIXTime -> POSIXTime -> Bool) -> POSIXTime -> FileTest
hasModifyTime = compareTime Files.modificationTimeHiRes

compareTimeWith ::
       (FileStatus -> POSIXTime)
    -> (POSIXTime -> POSIXTime -> Bool)
    -> FilePath
    -> IO FileTest
compareTimeWith getFileTime cmp path = do
    st <- Files.getFileStatus path
    return $ compareTime getFileTime cmp (getFileTime st)

-- | Compare the modification time of the file with the modification time of
-- another file.
cmpModifyTime ::
    (POSIXTime -> POSIXTime -> Bool) -> FilePath -> IO FileTest
cmpModifyTime = compareTimeWith Files.modificationTimeHiRes

-- | True if file1 and file2 exist and have the same device id and inode.
--
-- Like coreutil @test file1 -ef file2@.
isHardLinkOf :: FilePath -> IO FileTest
isHardLinkOf = undefined

getLocalTime :: IO TimeSpec
getLocalTime = fromAbsTime <$> getTime Realtime

compareAge ::
       (FileStatus -> POSIXTime)
    -> (POSIXTime -> POSIXTime -> Bool)
    -> Double
    -> IO FileTest
compareAge getFileTime cmp ageSec = do
    when (ageSec < 0) $ error "compareAge: age cannot be negative"

    ts <- getLocalTime
    let now = timespecToPosixTime ts
        age = doubleToPosixTime ageSec
    return $ compareTime getFileTime cmp (now - age)

    where

    timespecToPosixTime (TimeSpec s ns) =
        (fromIntegral s) + (fromIntegral ns) * 1E-9

    -- XXX handle negative double value?
    doubleToPosixTime :: Double -> POSIXTime
    doubleToPosixTime sec =
        let s = floor sec
            ns = round $ (sec - fromIntegral s) * 1E9
         in timespecToPosixTime (TimeSpec s ns)

-- XXX Use a -> a -> Bool instead
hasAccessAge :: (POSIXTime -> POSIXTime -> Bool) -> Double -> IO FileTest
hasAccessAge = compareAge Files.accessTimeHiRes

hasModifyAge :: (POSIXTime -> POSIXTime -> Bool) -> Double -> IO FileTest
hasModifyAge = compareAge Files.modificationTimeHiRes

{-
-- See https://unix.stackexchange.com/questions/91197/how-to-find-creation-date-of-file
hasCreateAge :: (POSIXTime -> POSIXTime -> Bool) -> Double -> IO FileTest
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
hasSize cmp n = FileTest (Predicate (\st -> getSize st `cmp` n))

-- | Compare the file size with the size of another file.
--
cmpSize :: (Int64 -> Int64 -> Bool) -> FilePath -> IO FileTest
cmpSize cmp path = do
    st <- Files.getFileStatus path
    return $ hasSize cmp (getSize st)
