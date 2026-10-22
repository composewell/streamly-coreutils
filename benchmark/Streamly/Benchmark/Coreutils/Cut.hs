-- |
--
-- Module      : Streamly.Benchmark.Coreutils.Cut
-- Copyright   : (c) 2026 Composewell Technologies
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Benchmarks for 'Streamly.Coreutils.Cut'. Each benchmark runs 'cut' or
-- 'cutFields' over a generated file and drains the output.

module Streamly.Benchmark.Coreutils.Cut (main) where

import Data.List (intercalate)
import System.FilePath ((</>))
import System.IO (IOMode(WriteMode), hPutStr, hSetEncoding, utf8, withFile)
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty.Bench

import Streamly.Coreutils.Cut

import qualified Streamly.Data.Array as Array
import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Data.Stream as Stream
import qualified Streamly.FileSystem.Path as Path

-------------------------------------------------------------------------------
-- Input
-------------------------------------------------------------------------------

lineCount :: Int
lineCount = 100000

fieldCount :: Int
fieldCount = 10

-- | Write a file of 'lineCount' lines, each with 'fieldCount' fields
-- separated by the given delimiter.
writeInput :: FilePath -> Char -> IO ()
writeInput file d =
    withFile file WriteMode $ \h -> do
        hSetEncoding h utf8
        hPutStr h $ unlines $ map line [1 .. lineCount]

    where

    line i =
        intercalate [d]
            [ "field" ++ show j ++ "-" ++ show i
            | j <- [1 .. fieldCount]
            ]

-------------------------------------------------------------------------------
-- Benchmarks
-------------------------------------------------------------------------------

-- | Total number of bytes in the output of 'cut'.
benchCut :: String -> (CutOptions -> CutOptions) -> Path.Path -> Benchmark
benchCut name f path =
    bench name $ nfIO $ Stream.fold Fold.sum $ Array.length <$> cut f path

-- | Total number of bytes in the fields returned by 'cutFields'.
benchCutFields ::
    String -> (CutOptions -> CutOptions) -> Path.Path -> Benchmark
benchCutFields name f path =
    bench name
        $ nfIO
        $ Stream.fold Fold.sum
        $ sum . map Array.length <$> cutFields f path

-- | Benchmarks for an input file using the given delimiter.
benchDelimiter :: String -> Char -> Path.Path -> Benchmark
benchDelimiter name d path =
    bgroup name
        [ benchCut "cut -f 1" (fields [1] . delim) path
        , benchCut "cut -f 1,3,5" (fields [1, 3, 5] . delim) path
        , benchCut "cut -f 2-" (fieldsFrom 2 . delim) path
        , benchCutFields "cutFields -f 1,3,5" (fields [1, 3, 5] . delim) path
        ]

    where

    delim = delimiter d

main :: IO ()
main =
    withSystemTempDirectory "cut-bench-" $ \dir -> do
        let tabFile = dir </> "tab"
            multiByteFile = dir </> "multibyte"
        writeInput tabFile '\t'
        -- U+2502, three bytes in UTF-8
        writeInput multiByteFile '\x2502'
        tabPath <- Path.fromString tabFile
        multiBytePath <- Path.fromString multiByteFile
        defaultMain
            [ bgroup "cut"
                [ benchDelimiter "tab" '\t' tabPath
                , benchDelimiter "multibyte" '\x2502' multiBytePath
                ]
            ]
