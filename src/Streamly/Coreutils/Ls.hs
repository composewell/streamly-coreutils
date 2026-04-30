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
    , LsOptions
    , recursive
    )
where

import Streamly.Data.Stream.Prelude (Stream)
import Streamly.FileSystem.Path (Path)

import qualified Streamly.Data.Stream.Prelude as Stream
import qualified Streamly.Internal.Data.Stream as Stream
import qualified Streamly.Internal.Data.Unfold as Unfold
import qualified Streamly.Internal.FileSystem.DirIO as Dir

-- Note: We can also have options to follow symlinks and other dir traversal
-- options once we decide on a good Configuration API.
--
-- XXX ls is more of a printing to console tool. We can return chunked arrays
-- with newlines and short or long info in this API which are directly
-- printable. In the "find" API we can return Path and structured Stat data
-- instead for programmatic control.
newtype LsOptions = LsOptions {lsRecursive :: Bool}

defaultConfig :: LsOptions
defaultConfig = LsOptions False

recursive :: Bool -> LsOptions -> LsOptions
recursive opt cfg = cfg {lsRecursive = opt}

ls :: (LsOptions -> LsOptions) -> Path -> Stream IO (Either Path Path)
ls f dir = do
    case lsRecursive (f defaultConfig) of
        False -> Dir.readEitherPaths id dir
        True ->
            -- Stream.unfoldIterateDfs unfoldOne
            -- BFS avoids opening too many file descriptors but may accumulate
            -- more data in memory.
            Stream.bfsUnfoldIterate unfoldOne
                --  $ Stream.parConcatIterate id streamOne
                --  $ Stream.parConcatIterate (Stream.ordered True) streamOne
                $ Stream.fromPure (Left dir)

    where

    unfoldOne = Unfold.either (Dir.eitherReaderPaths id) Unfold.nil
    -- streamOne = either Dir.readEitherPaths (const Stream.nil)
