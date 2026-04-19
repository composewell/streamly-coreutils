{-# OPTIONS_GHC -Wno-missing-signatures #-}

-- |
-- Module      : Streamly.Coreutils.Id
-- Copyright   : (c) 2022 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC (POSIX only)
--
-- Experimental alternative wrapper API over "System.Posix.User".
--
-- Provides the read-only functionality of the @id@, @whoami@, and @logname@
-- coreutils commands, intended for programmatic use.
--
-- = Design notes
--
-- * __Scope: current process only.__ This module only reports identity
--   information about the /current process/. Looking up identity details for
--   an arbitrary named user (i.e. @id \<username\>@) is a separate concern
--   that requires reading the user/group database (@/etc/passwd@,
--   @/etc/group@, NSS, etc.). That functionality will live in a separate
--   module (e.g. @Streamly.Coreutils.UserDB@) and is not implemented here.
--
-- * __Scope: read-only.__ Setting the uid/gid of the current process
--   (@setuid@, @setgid@) is the domain of @sudo@-style utilities and has
--   significant security implications. It is intentionally not exposed from
--   this module.
--
-- * __Portability.__ This is a POSIX-only module. A portable alternative
--   exposing a minimal common subset (e.g. @idNum@, @idName@) could be added
--   later.
--
-- * __Int for ids.__ User and group ids are exposed as 'Int' for
--   convenience and brevity at call sites. Alternatives considered:
--   'System.Posix.Types.UserID' / 'System.Posix.Types.GroupID' (which are
--   @CUid@ / @CGid@ newtypes) would be more type-safe (a uid cannot be
--   confused with a gid) and avoid any downcast concerns on platforms where
--   these are wider than 'Int'. If type safety or exotic-platform
--   correctness becomes a priority, switch to those. Downcasts via
--   'fromIntegral' are safe on all mainstream 64-bit platforms where
--   uid_t/gid_t are 32 bits.
--
-- * __'Maybe' for DB lookups.__ Functions that may fail to find a matching
--   entry return 'Maybe' rather than throwing, which is friendlier for
--   callers. The underlying @System.Posix.User@ primitives throw on a miss;
--   we catch and convert.
--
-- * __Individual functions over a bundled record.__ Each piece of
--   information is exposed as its own function rather than a combined
--   @IdInfo@ record. A record would not save syscalls here (each field
--   corresponds to a distinct primitive), so the extra surface area isn't
--   justified. Revisit if a batched primitive becomes available.
--
-- * __@whoami@ is just @effectiveUserName@__ and is not exposed as a
--   separate function to avoid redundancy.
--
module Streamly.Coreutils.Id
    (
    -- * Numeric ids of the current process
      realUserId
    , effectiveUserId
    , realGroupId
    , effectiveGroupId
    , groupIds

    -- * Names for the current process
    , realUserName
    , effectiveUserName
    , realGroupName
    , effectiveGroupName
    , groupNames
    , loginName
    )
where

import Control.Exception (try, SomeException)
import Data.List (nub)
import qualified System.Posix.User as Posix

------------------------------------------------------------------------------
-- Internal helpers
------------------------------------------------------------------------------

-- Look up a user-db entry by id, returning Nothing if it doesn't exist rather
-- than throwing. See "Maybe for DB lookups" in the module header.
--
-- Type signatures are intentionally omitted: UserEntry and GroupEntry are
-- defined in an internal module of the unix package and are not re-exported
-- from System.Posix.User, so they cannot be named here without pulling in
-- the internal module. The inferred types are correct.
-- tryLookupUser :: Int -> IO (Maybe UserEntry)
tryLookupUser i = do
    r <- try (Posix.getUserEntryForID (fromIntegral (i :: Int)))
    return $ case r of
        Left (_ :: SomeException) -> Nothing
        Right ue -> Just ue

-- tryLookupGroup :: Int -> IO (Maybe GroupEntry)
tryLookupGroup i = do
    r <- try (Posix.getGroupEntryForID (fromIntegral (i :: Int)))
    return $ case r of
        Left (_ :: SomeException) -> Nothing
        Right ge -> Just ge

------------------------------------------------------------------------------
-- Current process: numeric ids
------------------------------------------------------------------------------

-- | Real user id of the current process. Corresponds to @id -ru@.
realUserId :: IO Int
realUserId = fromIntegral <$> Posix.getRealUserID

-- | Effective user id of the current process. Corresponds to @id -u@.
effectiveUserId :: IO Int
effectiveUserId = fromIntegral <$> Posix.getEffectiveUserID

-- | Real group id of the current process. Corresponds to @id -rg@.
realGroupId :: IO Int
realGroupId = fromIntegral <$> Posix.getRealGroupID

-- | Effective group id of the current process. Corresponds to @id -g@.
effectiveGroupId :: IO Int
effectiveGroupId = fromIntegral <$> Posix.getEffectiveGroupID

-- | All group ids the current process belongs to: the effective primary
-- group plus all supplementary groups, deduplicated. Corresponds to @id -G@.
--
-- Note: @getgroups(2)@ alone returns only the supplementary list, and
-- whether that list includes the primary gid is OS-dependent. This function
-- explicitly merges the primary gid with the supplementary list to match
-- the @id -G@ command's output.
groupIds :: IO [Int]
groupIds = do
    primary <- effectiveGroupId
    supp <- map fromIntegral <$> Posix.getGroups
    return $ nub (primary : supp)

------------------------------------------------------------------------------
-- Current process: names
------------------------------------------------------------------------------

-- | Real user name of the current process. Corresponds to @id -unr@.
--
-- 'Nothing' if there is no user-db entry for the real uid.
realUserName :: IO (Maybe String)
realUserName = realUserId >>= fmap (fmap Posix.userName) . tryLookupUser

-- | Effective user name of the current process. Corresponds to @id -un@
-- and @whoami@.
--
-- 'Nothing' if there is no user-db entry for the effective uid.
effectiveUserName :: IO (Maybe String)
effectiveUserName =
    effectiveUserId >>= fmap (fmap Posix.userName) . tryLookupUser

-- | Real group name of the current process. Corresponds to @id -gnr@.
--
-- 'Nothing' if there is no group-db entry for the real gid.
realGroupName :: IO (Maybe String)
realGroupName = realGroupId >>= fmap (fmap Posix.groupName) . tryLookupGroup

-- | Effective group name of the current process. Corresponds to @id -gn@.
--
-- 'Nothing' if there is no group-db entry for the effective gid.
effectiveGroupName :: IO (Maybe String)
effectiveGroupName =
    effectiveGroupId >>= fmap (fmap Posix.groupName) . tryLookupGroup

-- | Names of all groups the current process belongs to, in the same order
-- as 'groupIds'. Corresponds to @id -Gn@.
--
-- Entries for which no group-db record exists are silently dropped.
groupNames :: IO [String]
groupNames = do
    gs <- groupIds
    mEntries <- mapM tryLookupGroup gs
    return [ Posix.groupName ge | Just ge <- mEntries ]

-- | Original login name of the session the current process belongs to.
--
-- Note: this can differ from 'effectiveUserName' after operations like
-- @su@ or @setuid@ — the login name reflects who originally logged in,
-- not who the process is currently acting as. Corresponds to the
-- @logname@ command.
loginName :: IO String
loginName = Posix.getLoginName
