-- |
-- Module      : Streamly.Coreutils.String
-- Copyright   : (c) 2022 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
module Streamly.Coreutils.String
    (
        deIndent
    )
where

import Data.Char (isSpace)

-- | Remove the common minimum space at beginning of each line in a string,
-- ignoring blank lines.
--
deIndent :: String -> String
deIndent input =
    let lns = lines input
        indent =
              minimum
            $ filter ( > 0)
            $ fmap (length . takeWhile isSpace) lns
     in   unlines $ fmap (drop indent) lns
