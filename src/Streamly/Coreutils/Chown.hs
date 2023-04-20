-- |
-- Module      : Streamly.Coreutils.Ls
-- Copyright   : (c) 2022 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Change file owner and group.

module Streamly.Coreutils.Chown
    (
      chown

    -- * Options
    , Chown
    , followLinks
    ) where

import qualified System.Posix.Files as Posix
import System.Posix.Types ( CUid (CUid), CGid (CGid) )
import GHC.Word (Word32)
import Streamly.Coreutils.Common (Switch(..))

newtype Chown = Chown{deRef :: Switch}

defaultConfig :: Chown
defaultConfig = Chown Off

followLinks :: Switch -> Chown -> Chown
followLinks opt cfg = cfg {deRef = opt}

chown :: (Chown -> Chown) -> FilePath -> Word32 -> Word32 -> IO ()
chown f path uid gid = do
    let opt = f defaultConfig
    case deRef opt of
        Off -> Posix.setSymbolicLinkOwnerAndGroup path (CUid uid) (CGid gid)
        On -> Posix.setOwnerAndGroup path (CUid uid) (CGid gid)
