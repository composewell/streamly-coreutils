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

    -- * Options
    , Ls
    , recursive
    )
where

import Data.Bifunctor (bimap)
import Data.Function ((&))
import Streamly.Coreutils.Common (Switch(..))
import Streamly.Data.Stream.Prelude (Stream)

import qualified Streamly.Data.Stream.Prelude as Stream
import qualified Streamly.Internal.Data.Stream.Concurrent as Concur (ahead2)
import qualified Streamly.Internal.Data.Stream as Stream (concatIterateWith)
import qualified Streamly.Internal.FileSystem.Dir as Dir

newtype Ls = Ls {lsRecursive :: Switch}

defaultConfig :: Ls
defaultConfig = Ls Off

recursive :: Switch -> Ls -> Ls
recursive opt cfg = cfg {lsRecursive = opt}

listDir :: String -> Stream IO (Either String String)
listDir dir =
      Dir.readEither dir
    & fmap (bimap mkAbs mkAbs)

    where

    mkAbs x = dir ++ "/" ++ x

ls :: (Ls -> Ls) -> String -> Stream IO (Either String String)
ls f dir = do
    let opt = f defaultConfig
        mapper = either Dir.readEitherPaths (const Stream.nil)
    case lsRecursive opt of
        Off -> listDir dir
        On ->
            let start = Stream.fromPure (Left ".")
              in Stream.concatIterateWith Concur.ahead2 mapper start
