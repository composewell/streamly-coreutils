-- |
-- Module      : Streamly.Coreutils.ReadLink
-- Copyright   : (c) 2022 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Returns resolved symbolic link target.

module Streamly.Coreutils.ReadLink
    (readLink)
where

import System.Directory (getSymbolicLinkTarget)

import Streamly.FileSystem.Path (Path)
import qualified Streamly.FileSystem.Path as Path

-- | If the path is a symbolic link return the link target.
readLink :: Path -> IO Path
readLink path = getSymbolicLinkTarget (Path.toString path) >>= Path.fromString
