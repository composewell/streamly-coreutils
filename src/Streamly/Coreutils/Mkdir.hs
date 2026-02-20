-- |
-- Module      : Streamly.Coreutils.Mkdir
-- Copyright   : (c) 2022 Composewell Technologies
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Create the Directory(ies), if they do not already exist.

module Streamly.Coreutils.Mkdir
    (
      mkdir

    -- * Options
    , Mkdir
    , parents
    )
where

import System.Directory (createDirectory, createDirectoryIfMissing)

newtype Mkdir = Mkdir {mdParents :: Bool}

defaultConfig :: Mkdir
defaultConfig = Mkdir False

parents :: Bool -> Mkdir -> Mkdir
parents opt cfg = cfg {mdParents = opt}

mkdir :: (Mkdir -> Mkdir) -> FilePath -> IO ()
mkdir f = do
  let opt = f defaultConfig
  case mdParents opt of
      False -> createDirectory
      True -> createDirectoryIfMissing True
