-- |
-- Module      : Streamly.Coreutils.Mv
-- Copyright   : (c) 2022 Composewell Technologies
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Rename source to dest, or move source(s) to directory.

module Streamly.Coreutils.Mv
    (
      mv

    -- * Options
    , MvOptions
    , Overwrite (..)
    , overwrite
    )
where

import Control.Monad (when)
import System.Directory (renamePath)

import Streamly.FileSystem.Path (Path)
import qualified Streamly.FileSystem.Path as Path

import Streamly.Coreutils.FileTest (testl)
import Streamly.Coreutils.Overwrite (Overwrite(..), shouldOverwrite)

-- = Design notes
--
-- * The destination is checked before 'renamePath' is called, so with
--   'OverwriteNever' a destination created between the check and the rename
--   is still replaced. GNU @mv@ avoids this race with @renameat2@ and
--   @RENAME_NOREPLACE@ on Linux.

-- | Options for 'mv'.
newtype MvOptions = MvOptions {mvOverwrite :: Overwrite}

defaultConfig :: MvOptions
defaultConfig = MvOptions OverwriteAlways

-- | Specify what to do when the destination exists. See 'Overwrite'.
--
-- A symlink at the destination counts as an existing destination even if
-- its target does not exist.
--
-- 'mv' never asks the user before replacing a destination. The default,
-- 'OverwriteAlways', is equivalent to GNU @mv -f@. GNU @mv@ without @-f@
-- asks before replacing a destination that is not writable, but only when
-- the input is a terminal; otherwise it replaces the destination, like
-- @mv -f@.
--
-- GNU @mv -i@ asks before replacing any existing destination. 'mv' does
-- not ask; use 'OverwriteNever' to keep every existing destination, or
-- 'OverwriteUpdate' to replace only a destination older than the source.
--
-- Default (without this modifier): 'OverwriteAlways', like GNU @mv@.
overwrite :: Overwrite -> MvOptions -> MvOptions
overwrite opt cfg = cfg {mvOverwrite = opt}

-- | @mv modifier source dest@ renames @source@ to @dest@. Pass 'id' for
-- defaults; each modifier documents its own default.
mv :: (MvOptions -> MvOptions) -> Path -> Path -> IO ()
mv f old new = do
    let opt = f defaultConfig
    r <- shouldOverwrite testl (mvOverwrite opt) old new
    when r $ renamePath (Path.toString old) (Path.toString new)
