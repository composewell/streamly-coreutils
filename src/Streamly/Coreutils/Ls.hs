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
import Streamly.FileSystem.Path (Path)

import qualified Streamly.Data.Stream.Prelude as Stream
import qualified Streamly.Internal.Data.Stream as Stream
import qualified Streamly.Internal.Data.Unfold as Unfold
import qualified Streamly.Internal.FileSystem.DirIO as Dir

-- Note: We can also have options to follow symlinks and other dir traversal
-- options once we decide on a good Configuration API.
newtype Ls = Ls {lsRecursive :: Switch}

defaultConfig :: Ls
defaultConfig = Ls Off

recursive :: Switch -> Ls -> Ls
recursive opt cfg = cfg {lsRecursive = opt}

ls :: (Ls -> Ls) -> Path -> Stream IO (Either Path Path)
ls f dir = do
    case lsRecursive (f defaultConfig) of
        Off -> Dir.readEitherPaths id dir
        On ->
            -- Stream.unfoldIterateDfs unfoldOne
            -- BFS avoids opening too many file descriptors but may accumulate
            -- more data in memory.
            Stream.unfoldIterateBfs unfoldOne
                --  $ Stream.parConcatIterate id streamOne
                --  $ Stream.parConcatIterate (Stream.ordered True) streamOne
                $ Stream.fromPure (Left dir)

    where

    unfoldOne = Unfold.either (Dir.eitherReaderPaths id) Unfold.nil
    -- streamOne = either Dir.readEitherPaths (const Stream.nil)
