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

import Streamly.Coreutils.FileTest ( FileTest, test )
import Streamly.Prelude (SerialT)

import qualified System.Directory as Directory
import qualified Streamly.Prelude as Stream

-- | Search through the given list of directories for the given file and
-- returns all paths where the given file exists.
--
find ::  [FilePath] -> String -> IO [FilePath]
find = Directory.findFiles

-- | Search recursively through the given directory for the given predicates
-- returns all paths where the given predicates are True.
-- Print the file paths having the size of 292 bytes in current directory
-- @
--  do
--    ft <- compareFileSize (FileSize EQ 292)
--    findFilesWith "." ft & Stream.mapM_ print
-- @
findFilesWith :: FilePath -> FileTest -> SerialT IO (Either FilePath FilePath)
findFilesWith dir ft = do
    let strm = listDirRec dir
        fs = Stream.filterM pred0 strm
    fs

    where
    pred0 path =
        case path of
            Left _d -> return False
            Right f -> test f ft
