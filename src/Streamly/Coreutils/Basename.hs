-- |
-- Module      : Streamly.Coreutils.Basename
-- Copyright   : (c) 2022 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Return pathe with any leading directory components removed.
-- If specified, also remove a trailing suffix.

module Streamly.Coreutils.Basename
    ( basename
    , basenameWith

    -- * Options
    , Basename
    , Suffix(..)
    , suffix
    )
where

import Data.List (stripPrefix)

data Suffix = None | Suffix [Char]

newtype Basename = Basename {removeSuffix :: Suffix}

suffix :: Suffix -> Basename -> Basename
suffix opt cfg = cfg {removeSuffix = opt}

defaultConfig :: Basename
defaultConfig = Basename None

basenameWith :: (Basename -> Basename) -> FilePath -> String
basenameWith f path =
    let opt = f defaultConfig
        base = reverse $ takeWhile (/= '/') $ reverse path
        in case removeSuffix opt of
            None -> base
            Suffix x ->
                let suf = reverse x
                    val0 = stripPrefix suf $ takeWhile (/= '/') $ reverse path
                    val = maybe base reverse val0
                    in val

basename :: FilePath -> String
basename  = basenameWith id
