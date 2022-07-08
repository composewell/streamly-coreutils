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
    (
      ln

    -- * Options
    , Ln
    , force
    , symbolic
    )
where

import Control.Monad (when)
import Streamly.Coreutils.Common (Switch(..))
import Streamly.Coreutils.FileTest (test, isExisting)

import qualified System.Posix.Files as Posix

data Ln = Ln
    { lnForce :: Switch
    , lnSymbolic :: Switch
    }

defaultConfig :: Ln
defaultConfig = Ln Off Off

force :: Switch -> Ln -> Ln
force opt cfg = cfg {lnForce = opt}

symbolic :: Switch -> Ln -> Ln
symbolic opt cfg = cfg {lnSymbolic = opt}

ln :: (Ln -> Ln) -> FilePath -> FilePath -> IO ()
ln f src tgt = do
    let opt = f defaultConfig
    when (lnForce opt == Off) $ do
        found <- test tgt isExisting
        when found $ error msg
    case lnSymbolic opt of
        Off -> Posix.createLink src tgt
        On -> Posix.createSymbolicLink src tgt

    where

    msg = "ln: Link target exists, use force to create anyway"
