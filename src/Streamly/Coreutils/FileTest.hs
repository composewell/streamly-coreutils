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
    , pOr

    -- * Running Predicates
    , test
    , testFD

    -- * Predicates
    , predicate

    -- ** General
    , isExisting
    , isNonNull

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
    , euidIsUs
    , egidIsUs

    -- ** Comparing with other files
    , isNewerThan
    , isOlderThan
    , isSameFile
    )
where

import Control.Exception (catch, throwIO)
import Data.Time.Clock.POSIX (POSIXTime)
import Foreign.C.Error (Errno(..), eNOENT)
import GHC.IO.Exception (IOException(..), IOErrorType(..))
import System.Posix.Types (Fd)
import System.Posix.Files (FileStatus)
import qualified System.Posix.Files as Files

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

-- | A predicate type for testing boolean statements about a file.
--
-- The 'Semigroup' instance acts as boolean @&&@. The 'Monoid' instance uses
-- 'True' as 'mempty'.
--
newtype FileTest = FileTest (Predicate FileStatus) deriving (Semigroup, Monoid)

-- Though || can be expressed in terms of not and && but || is really
-- convienient.  If we had a type class for <|> we could have defined an
-- instance of that for Predicate. Or maybe we can have a typeclass for Bool
-- like we have for Num, defining "and", "or" and "not".

-- | A boolean @or@ function for 'FileTest' predicates.
--
-- For boolean and operation use the Semigroup instance.
pOr :: FileTest -> FileTest -> FileTest
FileTest (Predicate p) `pOr` FileTest (Predicate q) =
    FileTest (Predicate $ \a -> p a || q a)

-- XXX Use a byte array instead of string filepath.
--
-- | Run a predicate on a 'FilePath'. Returns False if the path does not exist.
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

-- XXX Use Handle instead
-- | Run a predicate on an 'Fd'.
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
isNonNull :: FileTest
isNonNull = FileTest (Predicate (\st -> Files.fileSize st > 0))

---------------
-- Type of file
---------------

-- | True if file exists and is a directory.
--
-- Like @test -d file@
isDir :: FileTest
isDir = FileTest (Predicate Files.isDirectory)

-- | True if file exists and is a regular file.
--
-- Like coreutil @test -f file@
isFile :: FileTest
isFile = FileTest (Predicate Files.isRegularFile)

-- | True if file exists and is a symbolic link.
--
-- Like coreutil @test -h/-L file@
isSymLink :: FileTest
isSymLink = FileTest (Predicate Files.isSymbolicLink)

-- | True if file exists and is a block special file.
--
-- Like the coreutil @test -b file@.
isBlock :: FileTest
isBlock = FileTest (Predicate Files.isBlockDevice)

-- | True if file exists and is a character special file.
--
-- Like @test -c file:
isChar :: FileTest
isChar = FileTest (Predicate Files.isCharacterDevice)

-- | True if file is a named pipe (FIFO).
--
-- Like coreutil @test  -p file@
isPipe :: FileTest
isPipe = FileTest (Predicate Files.isNamedPipe)

-- | True if file exists and is a socket.
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

-- | True if file exists and its set user ID flag is set.
--
-- Like coreutil @test -u file@
isSetUID :: FileTest
isSetUID =
    FileTest (Predicate (\st -> Files.fileMode st == Files.setUserIDMode))

-- | True if file exists and its set group ID flag is set.
--
-- Like coreutil @test -g file@
isSetGID :: FileTest
isSetGID =
    FileTest (Predicate (\st -> Files.fileMode st == Files.setGroupIDMode))

-- | True if file exists and its sticky bit is set.
--
-- Like coreutil @test -k file@
--
-- /Unimplemented/
isSticky :: FileTest
isSticky = undefined
    -- FileTest (Predicate (\st -> Files.fileMode st == Files.stickyMode)

-- XXX To implement these we need to check:
--
-- If we are owner, file is readable by owner
-- else check if we can read it via group membership
-- else if the file has permissions for others

-- | True if file exists and is readable.
--
-- Like coreutil @test -r file@
--
-- /Unimplemented/
isReadable :: FileTest
isReadable = undefined

-- | True if file exists and is writable.  True indicates
-- only that the write flag is on.  The file is not writable on a read-only
-- file system even if this test indicates true.
--
-- Like coreutil @test -w file@
--
-- /Unimplemented/
isWritable :: FileTest
isWritable = undefined

-- | True if file exists and is executable.  True indicates
-- only that the execute flag is on.  If file is a directory, true
-- indicates that file can be searched.
--
-- Like coreutil @test -x file@
--
-- /Unimplemented/
isExecutable :: FileTest
isExecutable = undefined

-- | True if file exists and its owner matches the effective
-- user id of this process.
--
-- Like coreutil @test -O file@
--
-- /Unimplemented/
euidIsUs :: FileTest
euidIsUs = undefined

-- | True if file exists and its group matches the effective
-- group id of this process.
--
-- Like coreutil @test -G file@
--
-- /Unimplemented/
egidIsUs :: FileTest
egidIsUs = undefined

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
