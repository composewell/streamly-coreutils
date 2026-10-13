-- |
--
-- Module      : Streamly.Test.Coreutils.Sleep
-- Copyright   : (c) 2026 Composewell Technologies
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Tests for 'Streamly.Coreutils.Sleep'.

module Streamly.Test.Coreutils.Sleep (main) where

import Data.List (isInfixOf)
import GHC.Clock (getMonotonicTime)
import System.IO.Error (ioeGetErrorString)
import Test.Hspec

import Streamly.Coreutils.Sleep

-- | Seconds taken by the action.
timed :: IO () -> IO Double
timed action = do
    start <- getMonotonicTime
    action
    end <- getMonotonicTime
    return (end - start)

main :: IO ()
main = hspec $
    describe "sleep" $ do
        it "waits at least a fractional time" $ do
            elapsed <- timed (sleep 0.2)
            elapsed `shouldSatisfy` (>= 0.2)

        it "waits at least a time given in units" $ do
            elapsed <- timed (sleep (seconds 0.1 + minutes 0.001))
            elapsed `shouldSatisfy` (>= 0.16)

        it "returns for zero" $
            sleep 0 `shouldReturn` ()

        it "fails on a negative time" $
            sleep (-1) `shouldThrow`
                (("invalid time interval" `isInfixOf`) . ioeGetErrorString)
