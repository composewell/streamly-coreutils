-- |
-- Module      : Streamly.Coreutils.FileTest
-- Copyright   : (c) 2021 Composewell Technologies
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Predicates to test certain properties of a file.
--
-- Combine predicates for the same file and test those in one go for good
-- performance.
--
-- This is just a convenience wrapper on top of the POSIX functions.  It covers
-- the functionality provided by the GNU coreutils @test@ utility.  String
-- testing is not provided as it can be trivially done using built-in Haskell
-- functionality. That leaves only file test routines. The routines provided in
-- this module have a one to one correspondence with the @test@ utility.

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
    , isNotEmpty

    -- ** File Type
    , isDir
    , isFile
    , isSymLink
    , isChar
    , isBlock
    , isPipe
    , isSocket
    , isTerminalFD

    -- ** File Permissions
    , isReadable
    , isWritable
    , isExecutable
    , isSticky
    , isSetUID
    , isSetGID
    , ownerMatchesEUID
    , groupMatchesEGID

    -- ** Comparing with other files
    , isNewerThan
    , isOlderThan
    , isSameFile

    -- ** Comparing access time with current time
    , isAccessedBefore
    , isAccessedWithin
    -- ** Comparing modifications time with current time
    , isModifiedBefore
    , isModifiedWithin
    -- ** Comparing size in bytes
    , isSmallerThan
    , isLargerThan
    , hasSize

    -- ** Comparing size with a reference file
    , isSmallerThanFile
    , isLargerThanFile
    , hasSizeSameAs
    )
where

import Control.Exception (catch, throwIO)
import Data.Bits ((.&.))
import Data.Int (Int64)
import Data.Time.Clock.POSIX (POSIXTime)
import Foreign.C.Error (Errno(..), eNOENT)
import Foreign.C.Types (CTime(CTime))
import GHC.IO.Exception (IOException(..), IOErrorType(..))
import System.Posix.Types (Fd, EpochTime, COff(..), FileMode)
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

-- | True if file has a size greater than zero.
--
-- Like coreutil @test -s file@
isNotEmpty :: FileTest
isNotEmpty = FileTest (Predicate (\st -> Files.fileSize st > 0))

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
isBlock :: FileTest
isBlock = FileTest (Predicate Files.isBlockDevice)

-- | True if is a character special file.
--
-- Like @test -c file:
isChar :: FileTest
isChar = FileTest (Predicate Files.isCharacterDevice)

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
isSetUID :: FileTest
isSetUID = FileTest $ hasMode Files.setUserIDMode

-- | True if the file has set group ID flag is set.
--
-- Like coreutil @test -g file@
isSetGID :: FileTest
isSetGID = FileTest $ hasMode  Files.setGroupIDMode

-- | True if file has sticky bit is set.
--
-- Like coreutil @test -k file@
--
-- /Unimplemented/
isSticky :: FileTest
isSticky = undefined
    --FileTest (Predicate (\st -> Files.fileMode st == Files.stickyMode))

hasPermissions :: (FileMode, FileMode, FileMode) -> FilePath -> IO FileTest
hasPermissions (user, group, other) path = do
    -- XXX We are stating the file twice, ideally we should do it only
    -- once. Maybe we can use monadic predicates to avoid that.
    isOwner <- testM path ownerMatchesEUID
    if isOwner
    then return $ FileTest $ hasMode user
    else do
        isGroup <- testM path groupMatchesEGID
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
ownerMatchesEUID  :: IO FileTest
ownerMatchesEUID  =
    FileTest . Predicate . f <$> User.getEffectiveUserID

    where

    f euid st = Files.fileOwner st == euid

-- | True if file exists and its group matches the effective
-- group id of this process.
--
-- Like coreutil @test -G file@
--
-- /Unimplemented/
groupMatchesEGID :: IO FileTest
groupMatchesEGID =
    FileTest . Predicate . f <$> User.getEffectiveGroupID

    where

    f guid st = Files.fileGroup st == guid

------------------------------
-- Comparing with other files
------------------------------

-- | Returns the result of the comparison function with provided file's
-- modification time as second argument and the that of the file being tested as
-- the first argument
--
compareModTime :: (POSIXTime -> POSIXTime -> Bool) -> FilePath -> IO FileTest
compareModTime cmp path = do
    st <- Files.getFileStatus path
    return $ FileTest (Predicate ( `f` st))

    where

    f st1 st2 =
        let t1 = Files.modificationTimeHiRes st1
            t2 = Files.modificationTimeHiRes st2
        in t1 `cmp` t2

-- | True if the file being tested is newer than the provided file path.
--
-- Like the coreutil @test file1 -nt file2@
isNewerThan :: FilePath -> IO FileTest
isNewerThan = compareModTime (>)

-- | True if the file being tested is older than the provided file path.
--
-- Like coreutil @test file1 -ot file2@.
isOlderThan :: FilePath -> IO FileTest
isOlderThan = compareModTime (<)

-- | True if file1 and file2 exist and refer to the same file.
--
-- Like coreutil @test file1 -ef file2@.
isSameFile :: FilePath -> IO FileTest
isSameFile = undefined

getLocalTime :: IO MilliSecond64
getLocalTime = fromAbsTime <$> getTime Realtime

compareFileStatusNsecondsBefore ::
       (MilliSecond64 -> MilliSecond64 -> Bool)
    -> (FileStatus -> EpochTime)
    -> Double
    -> IO FileTest
compareFileStatusNsecondsBefore cmp getFileTime sec  =
    FileTest . Predicate . f <$> getLocalTime

    where

    f ct st =
        let CTime at = getFileTime st
            touchedInMsecbefore = (ct - MilliSecond64 (at * 1000))
         in cmp touchedInMsecbefore $ MilliSecond64 (round $ sec * 1000)

-- | A file is accessed less than n seconds before the current time.
-- /Pre-release/
isAccessedBefore :: Double -> IO FileTest
isAccessedBefore =
    compareFileStatusNsecondsBefore (<) Files.accessTime

-- | A file is accessed more than or equal to n seconds before
-- the current time.
-- /Pre-release/
isAccessedWithin :: Double -> IO FileTest
isAccessedWithin =
    compareFileStatusNsecondsBefore (>=) Files.accessTime

-- | A file is modified less than n seconds before the current time.
-- /Pre-release/
isModifiedBefore :: Double -> IO FileTest
isModifiedBefore =
    compareFileStatusNsecondsBefore (<) Files.modificationTime

-- | A file is modified more than or equal to n seconds before
-- the current time.
-- /Pre-release/
isModifiedWithin :: Double -> IO FileTest
isModifiedWithin =
    compareFileStatusNsecondsBefore (>=) Files.modificationTime

compareFileSizeWith :: (Int64 -> Int64 -> Bool) -> Int64 -> IO FileTest
compareFileSizeWith cmp n =
    return $ FileTest (Predicate f)

    where

    f st =
        let COff size = Files.fileSize st
         in cmp size n

-- XXX Should use Int or Int64?

-- | True if the file size is smaller than the specified size.
isSmallerThan :: Int64 -> IO FileTest
isSmallerThan = compareFileSizeWith (<)

-- | True if the file size is larger than the specified size.
isLargerThan :: Int64 -> IO FileTest
isLargerThan = compareFileSizeWith (>)

-- | True if the file size is equal to the specified size.
hasSize :: Int64 -> IO FileTest
hasSize = compareFileSizeWith (==)

compareFileSizeWithRef :: (Int64 -> Int64 -> Bool) -> FilePath -> IO FileTest
compareFileSizeWithRef cmp refPath = do
    st <- Files.getFileStatus refPath
    let COff size = Files.fileSize st
    return $ FileTest (Predicate (f size))

    where

    f sizeRef st =
        let COff size = Files.fileSize st
         in cmp size sizeRef

-- | True if the file size is smaller than the specified file's size.
isSmallerThanFile :: FilePath -> IO FileTest
isSmallerThanFile = compareFileSizeWithRef (<)

-- | True if the file size is larger than the specified file's size.
isLargerThanFile :: FilePath -> IO FileTest
isLargerThanFile = compareFileSizeWithRef (>)

-- | True if the file size is equal to the specified file's size.
hasSizeSameAs  :: FilePath -> IO FileTest
hasSizeSameAs  = compareFileSizeWithRef (==)
