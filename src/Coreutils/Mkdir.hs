-- |
-- Module      : Coreutils.Mkdir
-- Copyright   : (c) 2022 Composewell Technologies
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Create directories, optionally creating missing parent directories along
-- the way.
--
-- Usage:
--
-- >> mkdir id "a"                            -- create a single directory
-- >> mkdir (makeParents True) "a/b/c"        -- create with all parents

-- NOTE: we are not executing the doctests in this module because they actually
-- create the directories when doctest is run which is an unexpected side
-- effect. We can do that in CIs though.
--
module Coreutils.Mkdir
    (
      mkdir

    -- * Options
    , MkdirOptions
    , makeParents

    -- * Deprecated
    , Mkdir
    , parents
    )
where

import System.Directory (createDirectory, createDirectoryIfMissing)

import Streamly.FileSystem.Path (Path)
import qualified Streamly.FileSystem.Path as Path

-- Design Notes
--
-- No existOk: makeParents True implies exist-ok (matching coreutils -p behaviour).
-- A separate existOk modifier would only serve the rare case of ignoring an
-- existing leaf while erroring on missing parents; omitted until needed.
--
-- mkdirAll/mkdirWithParents not provided as synonyms; mkdir (makeParents True)
-- is only marginally longer, more consistent, and avoids extra API surface.
--
-- makeParents vs parents: 'parents True' reads ambiguously as a value rather
-- than an option setter; 'makeParents' makes the intent unambiguous.
--
-- withMode (TODO): applies to the leaf directory only, matching coreutils -m and
-- Python mkdir(mode=). A separate withParentMode can be added if needed.

-- | Options controlling the behaviour of 'mkdir'.
--
newtype MkdirOptions = MkdirOptions {mdParents :: Bool}

-- | Deprecated alias for 'MkdirOptions'.
{-# DEPRECATED Mkdir "Use 'MkdirOptions' instead." #-}
type Mkdir = MkdirOptions

defaultConfig :: MkdirOptions
defaultConfig = MkdirOptions False

-- | When set to 'True', create all missing parent directories rather than
-- failing when an intermediate directory does not exist.  Has no effect on
-- directories that already exist.
--
-- >> mkdir (makeParents True) "a/b/c"   -- creates a, a/b, and a/b/c as needed
-- >> mkdir (makeParents False) "a/b/c" -- fails if a/b does not exist already
makeParents :: Bool -> MkdirOptions -> MkdirOptions
makeParents opt cfg = cfg {mdParents = opt}

-- | @'parents'@ is a deprecated alias for 'makeParents'.
{-# DEPRECATED parents "Use 'makeParents' instead." #-}
parents :: Bool -> MkdirOptions -> MkdirOptions
parents = makeParents

-- | Create a directory at the given 'Path'.
--
-- The first argument is an options modifier applied to the default
-- 'MkdirOptions'.  Pass 'id' to use all defaults.
--
-- >> mkdir id "a"                         -- create a single directory
-- >> mkdir (makeParents True) "a/b/c"     -- create with missing parents
mkdir :: (MkdirOptions -> MkdirOptions) -> Path -> IO ()
mkdir f path = do
  let opt = f defaultConfig
  case mdParents opt of
      False -> createDirectory (Path.toString path)
      True -> createDirectoryIfMissing True (Path.toString path)
