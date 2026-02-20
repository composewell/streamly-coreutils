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

newtype Mv = Mv {mvForce :: Bool}

defaultConfig :: Mv
defaultConfig = Mv False

force :: Bool -> Mv -> Mv
force opt cfg = cfg {mvForce = opt}

mv :: (Mv -> Mv) -> FilePath -> FilePath -> IO ()
mv f old new = do
    let opt = f defaultConfig
    case mvForce opt of
        True -> renamePath old new
        False -> do
            exists <- doesPathExist new
            if exists
            then error msg
            else renamePath old new

    where

    msg = "mv: Move target exists, use force to move anyway"
