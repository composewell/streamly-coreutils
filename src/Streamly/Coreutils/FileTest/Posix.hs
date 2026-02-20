-- |
-- Module      : Streamly.Coreutils.FileTest.Posix
-- Copyright   : (c) 2021 Composewell Technologies
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- See "Streamly.Coreutils.FileTest" module for general module level
-- documentation. This is a posix specific version of
-- "Streamly.Coreutils.FileTest" with some additional posix specific functions.

module Streamly.Coreutils.FileTest.Posix
    ( isReadable
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

import System.Posix.Types (GroupID, UserID)
import qualified System.Posix.Files as Posix
import qualified System.PosixCompat.Files as Files
import qualified System.Posix.User as User

import Streamly.Coreutils.FileTest.Common

-- XXX large Int may get truncated to some valid id.
-- XXX Need a protable "Uid" (unix CUid or windows SID) to expose hasUid.
-- Also portable ways to get effective user id of the process.
newtype Uid = Uid UserID
newtype Gid = Gid GroupID

isOwnedByUserId :: Uid -> FileTest
isOwnedByUserId (Uid uid) = predicate $ \st -> Files.fileOwner st == uid

isOwnedByGroupId :: Gid -> FileTest
isOwnedByGroupId (Gid gid) = predicate $ \st -> Files.fileGroup st == gid

isOwnedByCurrentUser :: FileTest
isOwnedByCurrentUser =
    predicateM $ \st -> (Files.fileOwner st ==) <$> User.getEffectiveUserID

isOwnedByCurrentGroup :: FileTest
isOwnedByCurrentGroup =
    predicateM $ \st -> (Files.fileGroup st ==) <$> User.getEffectiveGroupID

-- The coreutil "test" utility checks for acls as well.
--
-- touch x
-- chmod -rwx x
-- setfacl -m u:harendra:r x
-- getfacl x
-- test -r x || echo "not readable"

pathIsReadable :: FilePath -> IO Bool
pathIsReadable path = Posix.fileAccess path True False False

isReadable :: FileTest
isReadable = predicateM $ \st -> pathIsReadable undefined

pathIsWritable :: FilePath -> IO Bool
pathIsWritable path = Posix.fileAccess path False True False

isWritable :: FileTest
isWritable = predicateM $ \st -> pathIsWritable undefined

pathIsExecutable :: FilePath -> IO Bool
pathIsExecutable path = Posix.fileAccess path False False True

isExecutable :: FileTest
isExecutable = predicateM $ \st -> pathIsExecutable undefined
