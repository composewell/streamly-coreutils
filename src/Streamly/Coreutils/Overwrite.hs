-- |
-- Module      : Streamly.Coreutils.Overwrite
-- Copyright   : (c) 2026 Composewell Technologies
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Overwrite behavior shared by commands that write to a destination path
-- (@cp@, @mv@).

module Streamly.Coreutils.Overwrite
    ( Overwrite (..)
    , shouldOverwrite
    )
where

import Streamly.FileSystem.Path (Path)

import Streamly.Coreutils.FileTest

-- | What to do when the destination already exists.
data Overwrite =
      OverwriteNever  -- ^ Keep an existing destination and skip without an
                      -- error, like GNU @--update=none@.
    | OverwriteAlways -- ^ Replace an existing destination, the GNU default
                      -- (@--update=all@).
    | OverwriteOnly   -- ^ Act only if the destination exists. No GNU
                      -- equivalent.
    | OverwriteUpdate -- ^ Replace an existing destination only if it is
                      -- older than the source, like GNU @-u@
                      -- (@--update=older@).

-- | @shouldOverwrite runTest option src dest@ returns 'True' if the command
-- should write @src@ to @dest@. @runTest@ ('test' or 'testl') decides whether
-- a symlink at @dest@ is followed when checking that @dest@ exists.
shouldOverwrite ::
    (Path -> FileTest -> IO Bool) -> Overwrite -> Path -> Path -> IO Bool
shouldOverwrite runTest option src dest =
    case option of
        OverwriteAlways -> return True
        OverwriteOnly -> runTest dest doesItExist
        OverwriteNever -> not <$> runTest dest doesItExist
        OverwriteUpdate -> do
            r <- runTest dest doesItExist
            if r
            then test src $ newerThanFile dest
            else return True
