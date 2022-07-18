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
    ( home
    , pwd
    , cd
    , withCd
    )
where

import System.Directory
    ( getHomeDirectory
    , withCurrentDirectory
    , getCurrentDirectory
    , setCurrentDirectory
    )

-- | Get home directory of the current user.
home :: IO FilePath
home = getHomeDirectory

-- XXX Support -L and -P options? Move this to its own module.

-- | Get the current working directory of the process.
pwd :: IO FilePath
pwd = getCurrentDirectory

-- XXX Set PWD env var? Move to its own module?
-- Support -L, -P options?

-- | Set the current working directory of the process.
cd :: FilePath -> IO ()
cd = setCurrentDirectory

-- | Run an IO action with the given working directory and restore the
-- original working directory afterwards, even if the given action fails
-- due to an exception.
--
withCd :: FilePath -> IO () -> IO ()
withCd = withCurrentDirectory
