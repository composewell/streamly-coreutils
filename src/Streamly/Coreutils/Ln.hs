-- |
-- Module      : Streamly.Coreutils.Ln
-- Copyright   : (c) 2022 Composewell Technologies
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- create a link to TARGET with the given name

module Streamly.Coreutils.Ln
    ( ln

    -- * Options
    , LnOptions
    , force
    , symbolic
    )
where

import Control.Monad (when)
import Streamly.Coreutils.FileTest (test, doesItExist)

import qualified System.PosixCompat.Files as Posix
import Streamly.FileSystem.Path (Path)
import qualified Streamly.FileSystem.Path as Path

data LnOptions = LnOptions
    { lnForce :: Bool
    , lnSymbolic :: Bool
    }

defaultConfig :: LnOptions
defaultConfig = LnOptions False False

force :: Bool -> LnOptions -> LnOptions
force opt cfg = cfg {lnForce = opt}

symbolic :: Bool -> LnOptions -> LnOptions
symbolic opt cfg = cfg {lnSymbolic = opt}

ln :: (LnOptions -> LnOptions) -> Path -> Path -> IO ()
ln f src tgt = do
    let opt = f defaultConfig
    when (lnForce opt == False) $ do
        found <- test tgt doesItExist
        when found $ error msg
    case lnSymbolic opt of
        False -> Posix.createLink (Path.toString src) (Path.toString tgt)
        True -> Posix.createSymbolicLink (Path.toString src) (Path.toString tgt)

    where

    msg = "ln: Link target exists, use force to create anyway"
