-- |
-- Module      : Streamly.Coreutils.Find
-- Copyright   : (c) 2022 Composewell Technologies
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Find a file in a given search PATHs.

module Streamly.Coreutils.Find
    ( find
    )
where

import System.Directory (findFiles)

-- | Search through the given list of directories for the given file and
-- returns all paths where the given file exists.
--
find ::  [FilePath] -> String -> IO [FilePath]
find = findFiles
