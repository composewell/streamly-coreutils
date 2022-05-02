-- |
-- Module      : Streamly.Coreutils.Directory
-- Copyright   : (c) 2022 Composewell Technologies
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Perform directory related operations.

module Streamly.Coreutils.Directory
    ( homeDir
    , withCd
    )
where

import System.Directory
    ( getHomeDirectory
    , withCurrentDirectory
    )

-- | Run an IO action with the given working directory and restore the
-- original working directory afterwards, even if the given action fails
-- due to an exception.
--
withCd :: FilePath -> IO () -> IO ()
withCd = withCurrentDirectory

homeDir :: IO FilePath
homeDir = getHomeDirectory
