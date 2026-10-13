{-# LANGUAGE NumericUnderscores #-}

-- |
-- Module      : Streamly.Coreutils.Time
-- Copyright   : (c) 2021 Composewell Technologies
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Time duration units shared by the utilities, re-exported by the modules
-- that take durations.

module Streamly.Coreutils.Time
    (
      seconds
    , minutes
    , hours
    , days
    )
where

import Data.Time.Clock (NominalDiffTime)

-- XXX Use the streamly time module with improvements

-- NOTES: NominalDiffTime is actually time duration in seconds possibly
-- fractional. In contrast to DiffTime it ignores the leap seconds, so it is
-- the actual elapsed time duration. A more specific and intuitive name for
-- this would be NominalDiffSeconds or Duration or simply Seconds.
--
-- We could use (Integer -> NominalDiffTime) in the APIs below and that would
-- disallow nesting of units e.g. (seconds (minutes 5)). But that is unlikely
-- error and NominalDiffTime allows fractional seconds which is a good thing.

-- | Time duration in seconds.
--
-- >>> seconds 30
-- 30s
--
seconds :: NominalDiffTime -> NominalDiffTime
seconds = id

-- | Time duration in minutes.
--
-- >>> minutes 5
-- 300s
--
minutes :: NominalDiffTime -> NominalDiffTime
minutes n = n * 60

-- | Time duration in hours.
--
-- >>> hours 1.5
-- 5400s
--
hours :: NominalDiffTime -> NominalDiffTime
hours n = n * 3600

-- | Time duration in days.
--
-- >>> days 1
-- 86400s
--
days :: NominalDiffTime -> NominalDiffTime
days n = n * 86_400
