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
    , StatOptions
    , followSymlinks
    ) where


import System.PosixCompat.Files (FileStatus)

import Streamly.FileSystem.Path (Path)
import qualified Streamly.FileSystem.Path as Path
import qualified System.PosixCompat.Files as Files

newtype  StatOptions = StatOptions {deRef :: Bool}

defaultConfig :: StatOptions
defaultConfig = StatOptions False

-- | If the path is a symbolic link, return the status of the file it points
-- to instead of the link itself, like @stat -L@ (@--dereference@).
--
-- Default (without this modifier): 'False', the status of the link itself,
-- like @stat@.
followSymlinks :: Bool -> StatOptions -> StatOptions
followSymlinks opt cfg = cfg {deRef = opt}

-- | Return the status of a file, like the @stat@ command. Pass 'id' for the
-- defaults; each modifier documents its own default.
stat :: (StatOptions -> StatOptions) -> Path -> IO FileStatus
stat f path = do
    let opt = f defaultConfig
    case deRef opt of
        False -> Files.getSymbolicLinkStatus (Path.toString path)
        True -> Files.getFileStatus (Path.toString path)
