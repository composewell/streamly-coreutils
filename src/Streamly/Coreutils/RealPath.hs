-- |
-- Module      : Streamly.Coreutils.RealPath
-- Copyright   : (c) 2022 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Returns resolved symbolic link target.

module Streamly.Coreutils.RealPath
    (realPath)
where

import System.Directory (canonicalizePath )

-- | Make a path absolute, normalize the path,
-- and remove as many indirections from it as possible.

realPath :: FilePath -> IO FilePath
realPath = canonicalizePath
