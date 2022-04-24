-- |
-- Module      : Streamly.Coreutils.Which
-- Copyright   : (c) 2022 Composewell Technologies
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Find an executable in a given search PATH.

module Streamly.Coreutils.Which
    (
      which
    , whichAll
    , whichAllIn
    )
where

import System.Directory

which :: String -> IO (Maybe FilePath)
which = findExecutable

whichAll :: String -> IO [FilePath]
whichAll = findExecutables

whichAllIn :: [FilePath] -> String -> IO [FilePath]
whichAllIn dirs = findExecutablesInDirectories dirs
