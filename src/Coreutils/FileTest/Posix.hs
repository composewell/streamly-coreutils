-- |
-- Module      : Coreutils.FileTest.Posix
-- Copyright   : (c) 2021 Composewell Technologies
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- See "Coreutils.FileTest" module for general module level
-- documentation. This is a posix specific version of
-- "Coreutils.FileTest" with some additional posix specific functions.

module Coreutils.FileTest.Posix
    ( testFd
    , testHandle
    , sameFileAs
    , isTerminalFd
    , isReadable
    , isWritable
    , isExecutable
    , isOwnedByCurrentUser
    , isOwnedByCurrentGroup
    , Uid
    , Gid
    , isOwnedByUserId
    , isOwnedByGroupId
    )
where

import System.IO (Handle)
import System.Posix.Types (Fd, GroupID, UserID)
import qualified System.Posix.Files as Posix
import qualified System.PosixCompat.Files as Files
import qualified System.Posix.User as User
import qualified System.Posix.Terminal as Terminal

import Streamly.FileSystem.Path (Path)
import qualified Streamly.FileSystem.Path as Path
import Coreutils.FileTest.Common

-- XXX 'getFdStatus' is not implemented for Windows in unix-compat.

-- XXX The 'FileStatus' is fetched eagerly before constructing the 'FileState'
-- because the file descriptor may be closed by the time a lazy fetch would
-- occur. Instead we can wrap this in an exception handler.

-- | Like 'test' but uses a file descriptor instead of a file path.
--
testFd :: Fd -> FileTest -> IO Bool
testFd fd (FileTest (Predicate f)) =
    -- XXX We should pass "Either Fd Path" in state.
    let fp = error $ "FileTest.testFd: filepath cannot be used"
     in Files.getFdStatus fd >>= mkFileState "FileTest.testFd" fp >>= f

testHandle :: Handle -> FileTest -> IO Bool
testHandle = undefined

-- | True if file1 and file2 exist and have the same device id and inode.
--
-- Like coreutil @test file1 -ef file2@.
--
-- The supplied file path is dereferenced if it is a symlink.
--
sameFileAs :: Path -> FileTest
sameFileAs path =
  withStatusM $ \st -> do
    st1 <- Files.getFileStatus (Path.toString path)
    pure $
      Files.deviceID st == Files.deviceID st1 &&
      Files.fileID   st == Files.fileID   st1

-- XXX Need to pass the Fd in the state.

-- | True if the supplied file descriptor refers to a terminal device.
--
-- Equivalent to the POSIX @isatty@ check and the shell command
-- @test -t fd@.
isTerminalFd :: Fd -> FileTest
isTerminalFd fd =
    withPathM $ \_ ->
        Terminal.queryTerminal fd

-- XXX large Int may get truncated to some valid id.
-- XXX Need a protable "Uid" (unix CUid or windows SID) to expose hasUid.
-- Also portable ways to get effective user id of the process.
newtype Uid = Uid UserID
newtype Gid = Gid GroupID

isOwnedByUserId :: Uid -> FileTest
isOwnedByUserId (Uid uid) = withStatus $ \st -> Files.fileOwner st == uid

isOwnedByGroupId :: Gid -> FileTest
isOwnedByGroupId (Gid gid) = withStatus $ \st -> Files.fileGroup st == gid

isOwnedByCurrentUser :: FileTest
isOwnedByCurrentUser =
    withStatusM $ \st -> (Files.fileOwner st ==) <$> User.getEffectiveUserID

isOwnedByCurrentGroup :: FileTest
isOwnedByCurrentGroup =
    withStatusM $ \st -> (Files.fileGroup st ==) <$> User.getEffectiveGroupID

-- The coreutil "test" utility checks for acls as well.
--
-- touch x
-- chmod -rwx x
-- setfacl -m u:harendra:r x
-- getfacl x
-- test -r x || echo "not readable"

pathIsReadable :: Path -> IO Bool
pathIsReadable path = Posix.fileAccess (Path.toString path) True False False

isReadable :: FileTest
isReadable = withPathM pathIsReadable

pathIsWritable :: Path -> IO Bool
pathIsWritable path = Posix.fileAccess (Path.toString path) False True False

isWritable :: FileTest
isWritable = withPathM pathIsWritable

pathIsExecutable :: Path -> IO Bool
pathIsExecutable path = Posix.fileAccess (Path.toString path) False False True

isExecutable :: FileTest
isExecutable = withPathM pathIsExecutable
