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
      getRealUserID
    , getRealGroupID
    , getEffectiveUserID
    , getEffectiveGroupID
    , getGroups
    , getLoginName
    , getEffectiveUserName
    )
where

import Data.Word (Word32)
import System.Posix.Types (CGid(CGid), CUid(CUid))

import qualified System.Posix.User as Posix

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

getLoginName :: IO String
getLoginName = Posix.getLoginName

getEffectiveUserName :: IO String
getEffectiveUserName = Posix.getEffectiveUserName
