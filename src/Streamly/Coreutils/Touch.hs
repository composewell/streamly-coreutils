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

import Control.Monad (when, unless)
import Streamly.Coreutils.Common (Switch(..))
import Streamly.Coreutils.FileTest (test, isExisting)
import System.IO (openFile, IOMode(WriteMode), hClose)

import qualified System.Posix.Files as Posix

data Touch = Touch
    {
      createNew :: Switch
    , deRef :: Switch   -- touch the referenced file for symbolic link
    }

defaultConfig :: Touch
defaultConfig = Touch On On

followLinks :: Switch -> Touch -> Touch
followLinks opt cfg = cfg {deRef = opt}

create :: Switch -> Touch -> Touch
create opt cfg = cfg {createNew = opt}

touch :: (Touch ->Touch) -> FilePath -> IO ()
touch f path = do
    let opt = f defaultConfig
    when (createNew opt == On) $ do
        found <- test path isExisting
        unless found $ openFile path WriteMode >>= hClose
    case deRef opt of
        On -> Posix.touchFile path
        Off -> Posix.touchSymbolicLink path
