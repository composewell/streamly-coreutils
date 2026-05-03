-- |
-- Module      : Coreutils.Touch
-- Copyright   : (c) 2022 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Update the access and modification times of a file to the current time.

module Coreutils.Touch
    (
      touch

    -- * Options
    , TouchOptions
    , create -- XXX rename for conflicts
    , followLinks -- XXX this is a common option in multiple commands
    )
where

import Control.Monad (unless)
import Coreutils.FileTest (test, doesItExist)
import System.IO (openFile, IOMode(WriteMode), hClose)
import Streamly.FileSystem.Path (Path)
import qualified Streamly.FileSystem.Path as Path

#if !defined (CABAL_OS_WINDOWS)
import qualified System.Posix.Files as Posix (touchSymbolicLink)
#endif
import qualified System.PosixCompat.Files as Posix

data TouchOptions = TouchOptions
    {
      createNew :: Bool
    , deRef :: Bool   -- touch the referenced file for symbolic link
    }

defaultConfig :: TouchOptions
defaultConfig = TouchOptions True True

-- | Default is 'True'.
followLinks :: Bool -> TouchOptions -> TouchOptions
followLinks opt cfg = cfg {deRef = opt}

-- | Default is 'True'.
create :: Bool -> TouchOptions -> TouchOptions
create opt cfg = cfg {createNew = opt}

-- | If the file does not exist create it only if both followLinks and create
-- are set to 'True'.
--
-- If the file or symbolic link exists then update the access and modification
-- times. If 'followLinks' is 'True' then the link target is updated otherwise
-- the symbolic link itself is updated.
--
-- Fails if the parent directories in the path do not exist or if there is no
-- permission to access a path component.
--
-- Defaults:
--
-- * create True
-- * followLinks True
--
touch :: (TouchOptions -> TouchOptions) -> Path -> IO ()
touch f path = do
    let opt = f defaultConfig
        pathStr = Path.toString path
    if (createNew opt == True && deRef opt == True)
    then do
        found <- test path doesItExist
        unless found $ openFile pathStr WriteMode >>= hClose
    else
        case deRef opt of
            True -> Posix.touchFile pathStr
#if !defined (CABAL_OS_WINDOWS)
            False -> Posix.touchSymbolicLink pathStr
#else
            -- XXX Is it possible to support this on Windows?
            False -> error "touch: followLinks=False not supported on Windows"
#endif
