-- |
-- Module      : Streamly.Coreutils.Sleep
-- Copyright   : (c) 2022 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Delay for a number of seconds.

module Streamly.Coreutils.Sleep
    (sleep)
where

import Control.Concurrent (threadDelay)

sleep :: Int -> IO ()
sleep n = threadDelay (n * 1000000)
