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
    , Mv
    , force
    )
where

import Streamly.Coreutils.Common (Switch(..))
import System.Directory (doesPathExist, renamePath)

newtype Mv = Mv {mvForce :: Switch}

defaultConfig :: Mv
defaultConfig = Mv Off

force :: Switch -> Mv -> Mv
force opt cfg = cfg {mvForce = opt}

mv :: (Mv -> Mv) -> FilePath -> FilePath -> IO ()
mv f old new = do
    let opt = f defaultConfig
    case mvForce opt of
        On -> renamePath old new
        Off -> do
            exists <- doesPathExist new
            if exists
            then error msg
            else renamePath old new

    where

    msg = "mv: Move target exists, use force to move anyway"
