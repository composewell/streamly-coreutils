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
    , destinationExistsError
    , shouldOverwrite
    )
where

import System.IO.Error (alreadyExistsErrorType, mkIOError)

import Streamly.FileSystem.Path (Path)
import qualified Streamly.FileSystem.Path as Path

import Streamly.Coreutils.FileTest

-- | What to do when the destination already exists.
data Overwrite =
      OverwriteNever  -- ^ Keep an existing destination and skip without an
                      -- error, like GNU @--update=none@.
    | OverwriteError  -- ^ Keep an existing destination and raise an
                      -- 'IOError', like GNU @--update=none-fail@.
    | OverwriteAlways -- ^ Replace an existing destination, the GNU default
                      -- (@--update=all@).
    | OverwriteOnly   -- ^ Act only if the destination exists. No GNU
                      -- equivalent.
    | OverwriteUpdate -- ^ Replace an existing destination only if it is
                      -- older than the source, like GNU @-u@
                      -- (@--update=older@).

-- | The error raised by 'OverwriteError' for an existing @dest@.
destinationExistsError :: Path -> IOError
destinationExistsError dest =
    mkIOError
        alreadyExistsErrorType "not replacing" Nothing
        (Just (Path.toString dest))

-- | @shouldOverwrite runTest option src dest@ returns 'True' if the command
-- should write @src@ to @dest@, and raises 'destinationExistsError' for
-- 'OverwriteError' if @dest@ exists. @runTest@ ('test' or 'testl') decides
-- whether a symlink at @dest@ is followed when checking that @dest@ exists.
shouldOverwrite ::
    (Path -> FileTest -> IO Bool) -> Overwrite -> Path -> Path -> IO Bool
shouldOverwrite runTest option src dest =
    case option of
        OverwriteAlways -> return True
        OverwriteOnly -> runTest dest doesItExist
        OverwriteNever -> not <$> runTest dest doesItExist
        OverwriteError -> do
            r <- runTest dest doesItExist
            if r
            then ioError (destinationExistsError dest)
            else return True
        OverwriteUpdate -> do
            r <- runTest dest doesItExist
            if r
            then test src $ newerThanFile dest
            else return True
