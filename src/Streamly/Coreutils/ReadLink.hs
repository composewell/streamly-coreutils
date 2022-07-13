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

-- | If the path is a symbolic link return the link target.
readLink :: FilePath -> IO FilePath
readLink = getSymbolicLinkTarget
