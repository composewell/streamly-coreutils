-- |
-- Module      : Streamly.Coreutils.Ls
-- Copyright   : (c) 2022 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- List directory contents.

module Streamly.Coreutils.Ls
    (
      ls
    , recursive
    )
where

import Data.Bifunctor (bimap)
import Data.Function ((&))
import Streamly.Coreutils.Common (Switch(..))
import Streamly.Internal.Data.Stream.Serial (SerialT)

import qualified Streamly.Prelude as Stream
import qualified Streamly.Internal.Data.Stream.IsStream as Stream
       (iterateMapLeftsWith)
import qualified Streamly.Internal.FileSystem.Dir as Dir

newtype Ls = Ls {lsRecursive :: Switch}

defaultConfig :: Ls
defaultConfig = Ls Off

recursive :: Switch -> Ls -> Ls
recursive opt cfg = cfg {lsRecursive = opt}

listDir :: String -> SerialT IO (Either String String)
listDir dir =
      Dir.toEither dir
    & Stream.map (bimap mkAbs mkAbs)

    where

    mkAbs x = dir ++ "/" ++ x

ls :: (Ls -> Ls) -> String -> SerialT IO (Either String String)
ls f dir = do
    let opt = f defaultConfig
    case lsRecursive opt of
        Off -> listDir dir
        On ->
            let start = Stream.fromPure (Left ".")
              in Stream.iterateMapLeftsWith Stream.ahead listDir start
