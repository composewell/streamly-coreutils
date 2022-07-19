-- |
-- Module      : Streamly.Coreutils.Dirname
-- Copyright   : (c) 2022 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Strip the last component from file name.

module Streamly.Coreutils.Dirname
    (dirname)
where

import System.FilePath (takeDirectory)

dirname :: FilePath -> FilePath
dirname = takeDirectory
