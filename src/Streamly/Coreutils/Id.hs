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
-- * __Scalar router ('idNum' \/ 'idName').__ In addition to the individual
--   named functions, a pair of \"router\" functions driven by 'IdOptions'
--   is provided. They are convenient when the choice of real\/effective
--   and user\/group is determined dynamically (e.g. from parsed CLI flags).
--   The individual functions remain the preferred style when the choice
--   is known statically — they avoid the runtime dispatch and are more
--   self-describing at the call site.
--
-- * __Router is scalar-only.__ 'idNum' and 'idName' only cover the
--   scalar queries (real\/effective × user\/group). The list-returning
--   queries ('groupIds', 'groupNames') and 'loginName' do not fit the
--   router's return type and are kept as standalone functions rather than
--   forced into a polymorphic or awkwardly-typed router.
--
module Streamly.Coreutils.Id
    (
    -- * Option-driven router
    -- | Convenient when the choice of real\/effective and user\/group is
    -- determined dynamically. Start from 'defaultConfig' (effective user)
    -- and compose modifiers.
      IdOptions
    , defaultConfig
    , idReal
    , idEffective
    , idUser
    , idGroup
    , idNum
    , idName

    -- * Numeric ids of the current process
    , realUserId
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

-- Look up the user name for a uid, returning Nothing if no entry exists
-- rather than throwing. See "Maybe for DB lookups" in the module header.
--
-- Note: this is a user-DB lookup and logically belongs in a future
-- Streamly.Coreutils.UserDB module. It is kept internal here so the
-- migration is not a breaking change for callers.
userNameFromId :: Int -> IO (Maybe String)
userNameFromId i = do
    r <- try (Posix.getUserEntryForID (fromIntegral i))
    return $ case r of
        Left (_ :: SomeException) -> Nothing
        Right ue -> Just (Posix.userName ue)

-- Look up the group name for a gid, returning Nothing if no entry exists
-- rather than throwing. Same migration note as 'userNameFromId'.
groupNameFromId :: Int -> IO (Maybe String)
groupNameFromId i = do
    r <- try (Posix.getGroupEntryForID (fromIntegral i))
    return $ case r of
        Left (_ :: SomeException) -> Nothing
        Right ge -> Just (Posix.groupName ge)

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
realUserName = realUserId >>= userNameFromId

-- | Effective user name of the current process. Corresponds to @id -un@
-- and @whoami@.
--
-- 'Nothing' if there is no user-db entry for the effective uid.
effectiveUserName :: IO (Maybe String)
effectiveUserName = effectiveUserId >>= userNameFromId

-- | Real group name of the current process. Corresponds to @id -gnr@.
--
-- 'Nothing' if there is no group-db entry for the real gid.
realGroupName :: IO (Maybe String)
realGroupName = realGroupId >>= groupNameFromId

-- | Effective group name of the current process. Corresponds to @id -gn@.
--
-- 'Nothing' if there is no group-db entry for the effective gid.
effectiveGroupName :: IO (Maybe String)
effectiveGroupName = effectiveGroupId >>= groupNameFromId

-- | Names of all groups the current process belongs to, in the same order
-- as 'groupIds'. Corresponds to @id -Gn@.
--
-- Entries for which no group-db record exists are silently dropped.
groupNames :: IO [String]
groupNames = do
    gs <- groupIds
    mNames <- mapM groupNameFromId gs
    return [ n | Just n <- mNames ]

-- | Original login name of the session the current process belongs to.
--
-- Note: this can differ from 'effectiveUserName' after operations like
-- @su@ or @setuid@ — the login name reflects who originally logged in,
-- not who the process is currently acting as. Corresponds to the
-- @logname@ command.
loginName :: IO String
loginName = Posix.getLoginName

------------------------------------------------------------------------------
-- Option-driven router
------------------------------------------------------------------------------

-- | Options for the 'idNum' and 'idName' router functions. Use
-- 'defaultConfig' together with the modifiers 'idReal', 'idEffective',
-- 'idUser', 'idGroup' to configure.
--
-- The default is /effective user/, matching the behaviour of @id -u@ and
-- @id -un@ when invoked without @-r@ or @-g@.
data IdOptions = IdOptions
    { idoReal  :: Bool  -- ^ True = real id, False = effective id
    , idoGroup :: Bool  -- ^ True = group, False = user
    }

-- | Default configuration: effective user.
defaultConfig :: IdOptions
defaultConfig = IdOptions { idoReal = False, idoGroup = False }

-- | Select the real id instead of the effective id.
idReal :: IdOptions -> IdOptions
idReal cfg = cfg { idoReal = True }

-- | Select the effective id (the default). Useful when composing modifiers
-- dynamically and you need to explicitly override a prior 'idReal'.
idEffective :: IdOptions -> IdOptions
idEffective cfg = cfg { idoReal = False }

-- | Select the group id instead of the user id.
idGroup :: IdOptions -> IdOptions
idGroup cfg = cfg { idoGroup = True }

-- | Select the user id (the default). Useful when composing modifiers
-- dynamically and you need to explicitly override a prior 'idGroup'.
idUser :: IdOptions -> IdOptions
idUser cfg = cfg { idoGroup = False }

-- | Return a numeric id (uid or gid, real or effective) of the current
-- process, based on the given options.
--
-- > idNum id                          -- effective uid  (id -u)
-- > idNum idReal                      -- real uid       (id -ru)
-- > idNum idGroup                     -- effective gid  (id -g)
-- > idNum (idReal . idGroup)          -- real gid       (id -rg)
idNum :: (IdOptions -> IdOptions) -> IO Int
idNum f =
    case (idoReal cfg, idoGroup cfg) of
        (False, False) -> effectiveUserId
        (True,  False) -> realUserId
        (False, True ) -> effectiveGroupId
        (True,  True ) -> realGroupId
  where
    cfg = f defaultConfig

-- | Return the name corresponding to a uid or gid (real or effective) of
-- the current process, based on the given options. Returns 'Nothing' if
-- the corresponding user- or group-db entry does not exist.
--
-- > idName id                         -- effective user name  (id -un)
-- > idName idReal                     -- real user name       (id -unr)
-- > idName idGroup                    -- effective group name (id -gn)
-- > idName (idReal . idGroup)         -- real group name      (id -gnr)
idName :: (IdOptions -> IdOptions) -> IO (Maybe String)
idName f = do
    n <- idNum f
    if idoGroup cfg then groupNameFromId n else userNameFromId n
  where
    cfg = f defaultConfig
