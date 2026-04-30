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

import System.Directory (doesPathExist, renamePath)

import Streamly.FileSystem.Path (Path)
import qualified Streamly.FileSystem.Path as Path

newtype Mv = Mv {mvForce :: Bool}

defaultConfig :: Mv
defaultConfig = Mv False

force :: Bool -> Mv -> Mv
force opt cfg = cfg {mvForce = opt}

mv :: (Mv -> Mv) -> Path -> Path -> IO ()
mv f old new = do
    let opt = f defaultConfig
        oldStr = Path.toString old
        newStr = Path.toString new
    case mvForce opt of
        True -> renamePath oldStr newStr
        False -> do
            exists <- doesPathExist newStr
            if exists
            then error msg
            else renamePath oldStr newStr

    where

    msg = "mv: Move target exists, use force to move anyway"
