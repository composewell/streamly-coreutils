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

import Streamly.FileSystem.Path (Path)
import qualified Streamly.FileSystem.Path as Path

which :: String -> IO (Maybe Path)
which name = findExecutable name >>= traverse Path.fromString

whichAll :: String -> IO [Path]
whichAll name = findExecutables name >>= traverse Path.fromString

whichAllIn :: [Path] -> String -> IO [Path]
whichAllIn dirs name =
    findExecutablesInDirectories (map Path.toString dirs) name
        >>= traverse Path.fromString
