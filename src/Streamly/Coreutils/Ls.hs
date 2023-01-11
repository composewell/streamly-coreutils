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

import Streamly.Coreutils.Common (Switch(..))
import Streamly.Data.Stream.Prelude (Stream)

import qualified Streamly.Data.Stream.Prelude as Stream
import qualified Streamly.Internal.Data.Stream as Stream
import qualified Streamly.Internal.Data.Unfold as Unfold
import qualified Streamly.Internal.FileSystem.Dir as Dir

newtype Ls = Ls {lsRecursive :: Switch}

defaultConfig :: Ls
defaultConfig = Ls Off

recursive :: Switch -> Ls -> Ls
recursive opt cfg = cfg {lsRecursive = opt}

ls :: (Ls -> Ls) -> String -> Stream IO (Either String String)
ls f dir = do
    case lsRecursive (f defaultConfig) of
        Off -> Dir.readEitherPaths dir
        On ->
            -- Stream.unfoldIterateDfs unfoldOne
            -- BFS avoids opening too many file descriptors but may accumulate
            -- more data in memory.
            Stream.unfoldIterateBfs unfoldOne
                --  $ Stream.parConcatIterate id streamOne
                --  $ Stream.parConcatIterate (Stream.ordered True) streamOne
                $ Stream.fromPure (Left ".")

    where

    unfoldOne = Unfold.either Dir.eitherReaderPaths Unfold.nil
    -- streamOne = either Dir.readEitherPaths (const Stream.nil)
