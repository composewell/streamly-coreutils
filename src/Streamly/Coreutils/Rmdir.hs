-- |
-- Module      : Streamly.Coreutils.Rmdir
-- Copyright   : (c) 2022 Composewell Technologies
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Deletes a directory if they are empty.
module Streamly.Coreutils.Rmdir
    ( Rmdir
    , rmdir
    , ignore
    , parent
    )
where

import Data.Default.Class (Default(..))
import System.Directory (removeDirectory, removeDirectoryRecursive)


data Rmdir = Ignore | Parent

ignore :: Rmdir
ignore = Ignore

parent :: Rmdir
parent = Parent

instance Default Rmdir
    where
    def = Ignore

rmdir :: Rmdir -> FilePath -> IO ()
rmdir opt = do
    case opt of
        Ignore -> removeDirectory
        Parent -> removeDirectoryRecursive
