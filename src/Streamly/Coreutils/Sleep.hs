{-# LANGUAGE NumericUnderscores #-}

-- |
-- Module      : Streamly.Coreutils.Sleep
-- Copyright   : (c) 2022 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Delay for a given time, like GNU @sleep@.
--
-- The time is a 'NominalDiffTime', a number of seconds that can have a
-- fraction. The units 'seconds', 'minutes', 'hours' and 'days' replace the
-- GNU suffixes, and @+@ replaces giving GNU @sleep@ several arguments:
--
-- >>> _ = sleep 1.5                          -- sleep 1.5
-- >>> _ = sleep (minutes 2)                  -- sleep 2m
-- >>> _ = sleep (minutes 1 + seconds 30)     -- sleep 1m 30s
-- >>> _ = sleep (days 1)                     -- sleep 1d
--
-- GNU @sleep inf@ has no equivalent: 'NominalDiffTime' cannot represent
-- infinity. Use @forever (threadDelay maxBound)@ instead.

module Streamly.Coreutils.Sleep
    (
      sleep

    -- * Time units
    , seconds
    , minutes
    , hours
    , days
    )
where

import Control.Concurrent (threadDelay)
import Data.Time.Clock (NominalDiffTime)

import Streamly.Coreutils.Time (seconds, minutes, hours, days)

-- = Design notes
--
-- * The time is a 'NominalDiffTime' rather than a string in the GNU syntax
--   (@"1.5m"@): literals and arithmetic express fractions, units and sums,
--   and a typo is a compile error instead of a runtime parse error.
--
-- * 'threadDelay' takes an 'Int' number of microseconds, which overflows
--   after about 35 minutes on 32-bit platforms. The delay is split into
--   chunks of at most @maxBound :: Int@ microseconds.

-- | Wait for the given time, like GNU @sleep@. The time is rounded up to
-- whole microseconds, so the wait is never shorter than requested.
--
-- >>> sleep 0.01
--
-- Raises an 'IOError' with the GNU message "invalid time interval" if the
-- time is negative.
sleep :: NominalDiffTime -> IO ()
sleep t
    | t < 0 =
        ioError $ userError $ "sleep: invalid time interval: " ++ show t
    | otherwise = go (ceiling (t * 1_000_000))

    where

    maxChunk = toInteger (maxBound :: Int)

    go :: Integer -> IO ()
    go us
        | us <= 0 = return ()
        | otherwise = do
            let n = min us maxChunk
            threadDelay (fromInteger n)
            go (us - n)
