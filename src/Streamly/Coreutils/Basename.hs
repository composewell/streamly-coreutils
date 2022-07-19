-- |
-- Module      : Streamly.Coreutils.Basename
-- Copyright   : (c) 2022 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Return pathe with any leading directory components removed.
-- If specified, also remove a trailing suffix (.extension).

module Streamly.Coreutils.Basename
    ( basename

    -- * Options
    , Basename
    , suffix
    )
where

import System.FilePath (takeBaseName, takeFileName)
import Streamly.Coreutils.Common (Switch(..))

newtype Basename = Basename {keepSuffix :: Switch}

suffix :: Switch -> Basename -> Basename
suffix opt cfg = cfg {keepSuffix = opt}

defaultConfig :: Basename
defaultConfig = Basename On

basename :: (Basename -> Basename) -> FilePath -> IO String
basename f path = do
    let opt = f defaultConfig
    return $
        case keepSuffix opt of
            Off -> takeBaseName path
            On -> takeFileName path
