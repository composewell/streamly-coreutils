-- |
-- Module      : Streamly.Coreutils.Find
-- Copyright   : (c) 2022 Composewell Technologies
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Find a file in a given search PATHs.

module Streamly.Coreutils.Find
    ( find
    , findFilesWith
    )
where

import Streamly.Coreutils.FileTest (FileTest, test)
import Streamly.Coreutils.Ls (ls, recursive)
import Streamly.Prelude (SerialT)

import qualified System.Directory as Directory
import qualified Streamly.Prelude as Stream
import Streamly.Coreutils.Common (Switch(On))

-- | Search through the given list of directories for the given file and
-- returns all paths where the given file exists.
--
find ::  [FilePath] -> String -> IO [FilePath]
find = Directory.findFiles

-- | Search recursively through the given directory for the given predicates
-- returns all paths where the given predicates are True.
--
findFilesWith :: FilePath -> FileTest -> SerialT IO (Either FilePath FilePath)
findFilesWith dir ft = do
    let strm = ls (recursive  On) dir
        fs = Stream.filterM pred0 strm
    fs

    where
    pred0 path =
        case path of
            Left _d -> return False
            Right f -> test f ft
