-- |
-- Module      : Streamly.Coreutils.Stat
-- Copyright   : (c) 2022 Composewell Technologies
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- gets the FileStatus information (user ID,
-- size, access times, etc.) for the file path.

-- XXX Reexport the required Posix types?

module Streamly.Coreutils.Stat
    (
      stat

    -- * Options
    , Stat
    , followLinks
    ) where


import Streamly.Coreutils.Common (Switch(..))
import System.PosixCompat.Files (FileStatus)

import qualified System.PosixCompat.Files as Files

newtype  Stat = Stat {deRef :: Switch}

defaultConfig :: Stat
defaultConfig = Stat On

followLinks :: Switch -> Stat -> Stat
followLinks opt cfg = cfg {deRef = opt}

stat :: (Stat -> Stat) -> FilePath -> IO FileStatus
stat f = do
    let opt = f defaultConfig
    case deRef opt of
        Off -> Files.getSymbolicLinkStatus
        On -> Files.getFileStatus
