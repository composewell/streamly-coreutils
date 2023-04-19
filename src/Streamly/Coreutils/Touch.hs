-- |
-- Module      : Streamly.Coreutils.Touch
-- Copyright   : (c) 2022 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Update the access and modification times of a file to the current time.

module Streamly.Coreutils.Touch
    (
      touch

    -- * Options
    , Touch
    , create
    , followLinks
    )
where

import Control.Monad (unless)
import Streamly.Coreutils.Common (Switch(..))
#if !defined (CABAL_OS_WINDOWS)
import qualified System.Posix.Files as Posix
#else
import qualified System.PosixCompat.Files as Posix
#endif
import System.IO (openFile, IOMode(WriteMode), hClose)
import qualified System.Directory as Directory

data Touch = Touch
    {
      createNew :: Switch
    , deRef :: Switch   -- touch the referenced file for symbolic link
    }

defaultConfig :: Touch
defaultConfig = Touch On On

-- | Default is 'On'.
followLinks :: Switch -> Touch -> Touch
followLinks opt cfg = cfg {deRef = opt}

-- | Default is 'On'.
create :: Switch -> Touch -> Touch
create opt cfg = cfg {createNew = opt}

-- | If the file does not exist create it only if both followLinks and create
-- are set to 'On'.
--
-- If the file or symbolic link exists then update the access and modification
-- times. If 'followLinks' is 'On' then the link target is updated otherwise
-- the symbolic link itself is updated.
--
-- Fails if the parent directories in the path do not exist or if there is no
-- permission to access a path component.
--
-- Defaults:
--
-- * create On
-- * followLinks On
--
touch :: (Touch -> Touch) -> FilePath -> IO ()
touch f path = do
    let opt = f defaultConfig
    if (createNew opt == On && deRef opt == On)
    then do
        -- TODO make isExisting portable instead.
        found <- Directory.doesFileExist path
        unless found $ openFile path WriteMode >>= hClose
    else
        case deRef opt of

            -- TODO: touchFile is available in unix-compat.
            On -> Posix.touchFile path
            -- TODO: in case of windows this could be just touchFile
#if !defined (CABAL_OS_WINDOWS)
            Off -> Posix.touchSymbolicLink path
#else
            Off -> Posix.touchFile path
#endif
