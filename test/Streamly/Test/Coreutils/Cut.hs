-- |
--
-- Module      : Streamly.Test.Coreutils.Cut
-- Copyright   : (c) 2026 Composewell Technologies
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Tests for 'Streamly.Coreutils.Cut'. The expected outputs are those of GNU
-- @cut@.

module Streamly.Test.Coreutils.Cut (main) where

import Data.Char (chr)
import Data.Functor.Identity (runIdentity)
import Data.List (isInfixOf)
import Data.Word (Word8)
import System.FilePath ((</>))
import System.IO (IOMode(WriteMode), hPutStr, withBinaryFile)
import System.IO.Error (ioeGetErrorString)
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

import Streamly.Coreutils.Cut

import qualified Streamly.Data.Array as Array
import qualified Streamly.Data.Stream as Stream
import qualified Streamly.FileSystem.Path as Path
import qualified Streamly.Unicode.Stream as Unicode

-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

-- | Run the action on a file with the given contents. The file is written in
-- binary mode, so that "\n" is not translated on Windows.
withInput :: String -> (Path.Path -> IO a) -> IO a
withInput contents action =
    withSystemTempDirectory "cut-test-" $ \dir -> do
        let file = dir </> "input"
        withBinaryFile file WriteMode (`hPutStr` contents)
        Path.fromString file >>= action

toString :: Array.Array Word8 -> String
toString = map (chr . fromIntegral) . Array.toList

-- | Output lines of 'cut' on a file with the given contents.
cutS :: (CutOptions -> CutOptions) -> String -> IO [String]
cutS f contents =
    withInput contents $ \path ->
        map toString <$> Stream.toList (cut f path)

-- | Output of 'cutFields' on a file with the given contents.
cutFieldsS :: (CutOptions -> CutOptions) -> String -> IO [[String]]
cutFieldsS f contents =
    withInput contents $ \path ->
        map (map toString) <$> Stream.toList (cutFields f path)

comma :: Char
comma = ','

-- | The UTF-8 encoding of a string, one 'Char' per byte, as 'withInput'
-- writes and 'toString' reads.
utf8 :: String -> String
utf8 =
    map (chr . fromIntegral)
        . runIdentity
        . Stream.toList
        . Unicode.encodeUtf8
        . Stream.fromList

-- | The action raises an 'IOError' whose message contains the string.
shouldFailWith :: IO a -> String -> Expectation
shouldFailWith action msg =
    action `shouldThrow` (\e -> msg `isInfixOf` ioeGetErrorString e)

-------------------------------------------------------------------------------
-- Tests
-------------------------------------------------------------------------------

main :: IO ()
main = hspec $ do
    describe "field selection" $ do
        it "numbers fields from 1" $
            cutS (fields [1] . delimiter comma) "a,b,c\n"
                `shouldReturn` ["a"]

        it "outputs fields in input order, each once" $
            cutS (fields [3, 1, 3] . delimiter comma) "a,b,c\n"
                `shouldReturn` ["a,c"]

        it "selects a range given as a list" $
            cutS (fields [2 .. 3] . delimiter comma) "a,b,c,d\n"
                `shouldReturn` ["b,c"]

        it "selects an open-ended range with fieldsFrom" $
            cutS (fieldsFrom 3 . delimiter comma) "a,b,c,d,e\n"
                `shouldReturn` ["c,d,e"]

        it "accumulates fields and fieldsFrom" $
            cutS (fields [1] . fieldsFrom 4 . fields [2] . delimiter comma)
                "a,b,c,d,e\n"
                `shouldReturn` ["a,b,d,e"]

        it "uses the smallest of several fieldsFrom" $
            cutS (fieldsFrom 4 . fieldsFrom 2 . delimiter comma) "a,b,c,d\n"
                `shouldReturn` ["b,c,d"]

        it "outputs an empty line when no selected field exists" $
            cutS (fields [5] . delimiter comma) "a,b\n"
                `shouldReturn` [""]

        it "uses TAB as the default delimiter" $
            cutS (fields [2]) "a\tb,c\n"
                `shouldReturn` ["b,c"]

    describe "lines" $ do
        it "outputs a line without the delimiter whole" $
            cutS (fields [2] . delimiter comma) "abc\n"
                `shouldReturn` ["abc"]

        it "drops a line without the delimiter with onlyDelimited" $
            cutS (fields [2] . delimiter comma . onlyDelimited True)
                "abc\na,b\n"
                `shouldReturn` ["b"]

        it "keeps empty fields" $
            cutS (fields [2, 3] . delimiter comma) "a,,b\n"
                `shouldReturn` [",b"]

        it "keeps an empty trailing field" $
            cutS (fields [3] . delimiter comma) "a,b,\n"
                `shouldReturn` [""]

        it "outputs an empty line whole" $
            cutS (fields [1] . delimiter comma) "a\n\nb\n"
                `shouldReturn` ["a", "", "b"]

        it "does not output an extra line after the final newline" $
            cutS (fields [1] . delimiter comma) "a\nb\n"
                `shouldReturn` ["a", "b"]

        it "outputs a last line without a newline" $
            cutS (fields [1] . delimiter comma) "a\nb"
                `shouldReturn` ["a", "b"]

        it "keeps an empty trailing field with onlyDelimited" $
            cutS (fields [2] . delimiter comma . onlyDelimited True) "a,\n"
                `shouldReturn` [""]

        it "drops an empty line with onlyDelimited" $
            cutS (fields [1] . delimiter comma . onlyDelimited True)
                "\na,b\n"
                `shouldReturn` ["a"]

        it "outputs nothing for an empty file" $
            cutS (fields [1]) "" `shouldReturn` []

    describe "UTF-8" $ do
        it "keeps multi-byte characters in fields" $
            cutS (fields [2] . delimiter comma) (utf8 "\xe9,\x20ac\x4e2d\n")
                `shouldReturn` [utf8 "\x20ac\x4e2d"]

        it "splits on a multi-byte delimiter" $
            cutS (fields [1, 3] . delimiter '\x20ac')
                (utf8 "a\x20ac\xe9\x20ac\x4e2d\n")
                `shouldReturn` [utf8 "a\x20ac\x4e2d"]

        it "does not split inside a character on a partial match" $
            -- U+20AC is E2 82 AC; U+20AD is E2 82 AD
            cutS (fields [2] . delimiter '\x20ac') (utf8 "a\x20ad\&b\n")
                `shouldReturn` [utf8 "a\x20ad\&b"]

        it "keeps an empty field between multi-byte delimiters" $
            cutFieldsS (fields [1 .. 3] . delimiter '\x20ac')
                (utf8 "a\x20ac\x20ac\n")
                `shouldReturn` [["a", "", ""]]

        it "drops a line without a multi-byte delimiter with onlyDelimited" $
            cutS (fields [1] . delimiter '\x20ac' . onlyDelimited True)
                (utf8 "a,b\nc\x20ac\&d\n")
                `shouldReturn` ["c"]

    describe "cutFields" $ do
        it "returns the selected fields of each line" $
            cutFieldsS (fields [3, 1] . delimiter comma) "a,b,c\nd,e,f\n"
                `shouldReturn` [["a", "c"], ["d", "f"]]

        it "returns an empty field after a final delimiter" $
            cutFieldsS (fields [1, 2] . delimiter comma) "a,\n"
                `shouldReturn` [["a", ""]]

        it "returns a line without the delimiter as a single field" $
            cutFieldsS (fields [2] . delimiter comma) "abc\n"
                `shouldReturn` [["abc"]]

    describe "errors" $ do
        it "fails when no fields are given" $
            cutS id "a\n"
                `shouldFailWith`
                    "you must specify a list of bytes, characters, or fields"

        it "fails on field 0" $
            cutS (fields [0, 1]) "a\n"
                `shouldFailWith` "fields are numbered from 1"

        it "fails on fieldsFrom 0" $
            cutS (fieldsFrom 0) "a\n"
                `shouldFailWith` "fields are numbered from 1"

        it "fails on invalid options before opening the file" $
            withSystemTempDirectory "cut-test-" $ \dir -> do
                path <- Path.fromString (dir </> "missing")
                Stream.toList (cut id path)
                    `shouldFailWith` "you must specify a list"
