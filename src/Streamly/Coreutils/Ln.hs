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
    , Ln
    , force
    , symbolic
    )
where

import Control.Monad (when)
import Streamly.Coreutils.FileTest (test, doesItExist)

import qualified System.PosixCompat.Files as Posix
import Streamly.FileSystem.Path (Path)
import qualified Streamly.FileSystem.Path as Path

data Ln = Ln
    { lnForce :: Bool
    , lnSymbolic :: Bool
    }

defaultConfig :: Ln
defaultConfig = Ln False False

force :: Bool -> Ln -> Ln
force opt cfg = cfg {lnForce = opt}

symbolic :: Bool -> Ln -> Ln
symbolic opt cfg = cfg {lnSymbolic = opt}

ln :: (Ln -> Ln) -> Path -> Path -> IO ()
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
