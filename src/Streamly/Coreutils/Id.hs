-- |
-- Module      : Streamly.Coreutils.Id
-- Copyright   : (c) 2022 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Get real and effective user and group IDs.

module Streamly.Coreutils.Id
    (
      id
      
    -- * Options
    , IdOptions
    , effectiveGroupId
    , groups
    , realGroupId
    , realUserId
    , effectiveUserId
    , effectiveUserName
    , realUserName
    , IdResult(..)
    )
where

import Data.Word (Word32)
import System.Posix.Types (CGid(CGid), CUid(CUid))

import qualified System.Posix.User as Posix

import Prelude hiding (id)

data IdOptions =
      RealGroupId
    | EffectiveGroupId
    | Groups
    | RealUserName
    | EffectiveUserName
    | RealUserId
    | EffectiveUserId

realGroupId :: IdOptions
realGroupId = RealGroupId

effectiveGroupId :: IdOptions
effectiveGroupId = EffectiveGroupId

groups :: IdOptions
groups = Groups

realUserName :: IdOptions
realUserName = RealUserName

effectiveUserName :: IdOptions
effectiveUserName = EffectiveUserName

realUserId :: IdOptions
realUserId = RealUserId

effectiveUserId :: IdOptions
effectiveUserId = EffectiveUserId

data IdResult =
      Id Word32
    | Ids [Word32]
    | Name String
        deriving Show

getRealUserID :: IO Word32
getRealUserID = do
    CUid x <- Posix.getRealUserID
    return x

getRealGroupID :: IO Word32
getRealGroupID = do
    CGid x <- Posix.getRealGroupID
    return x

getEffectiveUserID :: IO Word32
getEffectiveUserID =  do
    CUid x <- Posix.getEffectiveUserID
    return x

getEffectiveGroupID :: IO Word32
getEffectiveGroupID = do
    CGid x <- Posix.getEffectiveGroupID
    return x

getGroups :: IO [Word32]
getGroups =
    map (\(CGid x) -> x) <$> Posix.getGroups

id :: IdOptions -> IO IdResult
id opt = case opt of
    RealGroupId -> Id <$> getRealGroupID
    EffectiveGroupId -> Id <$> getEffectiveGroupID
    Groups -> Ids <$> getGroups
    RealUserName -> Name <$> Posix.getLoginName
    EffectiveUserName -> Name <$> Posix.getEffectiveUserName
    RealUserId -> Id <$> getRealUserID
    EffectiveUserId -> Id <$> getEffectiveUserID
