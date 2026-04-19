-- |
-- Module      : Streamly.Coreutils.Id
-- Copyright   : (c) 2022 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Experimental alternative wrapper API over System.Posix.User.
--
-- Shorter names, closer to shell commands.
-- Int for user-id/group-id for covnenience.
-- Adds one missing function
--
-- Functions to get and set the user and group id of the current process.
--
-- Substitutes the functionality of the @id@ and @whoami@ coreutils commands.
--
-- This is a Posix only module.

-- TODO: create a portable module with "idNum" and "idName" commands to print
-- the current user id, name.
--
-- Can separate process API and the user DB API.

module Streamly.Coreutils.Id
    (
      uid
    , euid
    , gid
    , egid
    , uid2name
    , gid2name
    -- , groups
    )
where

import System.Posix (getGroupEntryForID)
import Prelude hiding (id)
import qualified System.Posix.User as Posix

------------------------------------------------------
-- Current process settings
------------------------------------------------------

-- | Return current process real user id.
--
-- id -ru
uid :: IO Int
uid = fromIntegral <$> Posix.getRealUserID

-- | Return current process real group id.
--
-- id -rg
gid :: IO Int
gid = fromIntegral <$> Posix.getRealGroupID

-- | Return current process effective user id.
--
-- id -u
euid :: IO Int
euid = fromIntegral <$> Posix.getEffectiveUserID

-- | Return current process effective group id.
--
-- id -g
egid :: IO Int
egid = fromIntegral <$> Posix.getEffectiveGroupID

-- | Get groups associated with the current process.
--
-- id -G
groups :: IO [Int]
groups = fmap fromIntegral <$> Posix.getGroups

-- | The original login name of the process. Note: the current user name may
-- change by setuid but login name remains the same.
logname :: IO String
logname = Posix.getLoginName

------------------------------------------------------
-- These should go to user database module?
------------------------------------------------------

-- XXX We can parse the passwd file ourselves instead of using C code
-- XXX Use an Compact Array/OsString instead?

{-
-- getpwuid
uid2pwent =

-- getgrgid
gid2grent =

-- getpwnam
name2pwent =

-- getgrnam
name2grent =

-- Stream the entries.
getpwents  =
-}

-- | Convert numeric user id to user name.
uid2name :: Int -> IO String
uid2name i = do
    -- XXX fromIntegral downcast
    pw <- Posix.getUserEntryForID (fromIntegral i)
    return (Posix.userName pw)

-- XXX Use an Compact Array/OsString instead?

-- | Convert numeric group id to group name.
gid2name :: Int -> IO String
gid2name i = do
    -- XXX fromIntegral downcast
    pw <- Posix.getGroupEntryForID (fromIntegral i)
    return (Posix.groupName pw)

-- Note there is no uid2groups as anyway the groups file has the user name. We
-- search by name even if uid is provided. So we can convert uid to name and
-- then search.

-- | List all the groups in which a uid occurs. Returns (gid, group name).
--
-- id -Gn <user>
user2groups :: Int -> [(Int, String)]
user2groups = undefined

-- | Current process user name.
--
-- id -un
whoami :: IO String
whoami = uid >>= uid2name

-- | Current process group names.
--
-- id -Gn
groupNames :: IO [String]
groupNames = do
    xs <- Posix.getGroups
    fmap Posix.groupName <$> mapM getGroupEntryForID xs
