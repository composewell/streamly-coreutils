-- |
--
-- Module      : Streamly.Test.Coreutils.Sh
-- Copyright   : (c) 2026 Composewell Technologies
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Tests for 'Streamly.Coreutils.Sh'. The commands are POSIX shell commands;
-- on Windows they need @sh@ on the PATH.

module Streamly.Test.Coreutils.Sh (main) where

import Data.Char (chr)
import Data.Function ((&))
import Data.Word (Word8)
import Test.Hspec

import Streamly.Coreutils.Sh

import qualified Streamly.Data.Array as Array
import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Data.Stream as Stream
import qualified Streamly.Internal.System.Process as Process

-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

bytesToString :: [Word8] -> String
bytesToString = map (chr . fromIntegral)

-- | Input large enough to fill the pipe buffers, so that the writer and the
-- reader of the process run concurrently.
largeInput :: String
largeInput = concatMap (\i -> show i ++ "\n") [1 .. 50000 :: Int]

isProcessFailure :: Selector Process.ProcessFailure
isProcessFailure = const True

-------------------------------------------------------------------------------
-- Tests
-------------------------------------------------------------------------------

main :: IO ()
main = hspec $ do
    describe "generation" $ do
        it "toString returns the output" $
            toString "echo hello" `shouldReturn` "hello\n"

        it "toBytes returns the output bytes" $ do
            out <- toBytes "echo hello" & Stream.toList
            bytesToString out `shouldBe` "hello\n"

        it "toChunks returns the output in arrays" $ do
            out <- toChunks "echo hello" & Stream.toList
            bytesToString (concatMap Array.toList out) `shouldBe` "hello\n"

        it "toChars returns the output characters" $
            (toChars "echo hello" & Stream.toList) `shouldReturn` "hello\n"

        it "toLines splits the output into lines" $
            (toLines Fold.toList "printf 'a\\nb\\nc\\n'" & Stream.toList)
                `shouldReturn` ["a", "b", "c"]

        it "runs a pipeline inside the command" $
            toString "echo hello | tr a-z A-Z" `shouldReturn` "HELLO\n"

        it "passes quoted arguments to the shell" $
            toString "printf '%s|' 'a b' \"c d\" e\\ f"
                `shouldReturn` "a b|c d|e f|"

        it "returns empty output" $
            toString "true" `shouldReturn` ""

    describe "effects" $ do
        it "toNull runs the command" $
            toNull "true" `shouldReturn` ()

        it "toStdout runs the command" $
            toStdout "true" `shouldReturn` ()

    describe "failure" $ do
        it "toString raises ProcessFailure on a non-zero exit" $
            toString "exit 3" `shouldThrow` isProcessFailure

        it "toNull raises ProcessFailure on a non-zero exit" $
            toNull "false" `shouldThrow` isProcessFailure

        it "toBytes raises ProcessFailure on a non-zero exit" $
            (toBytes "exit 1" & Stream.toList) `shouldThrow` isProcessFailure

    describe "transformation" $ do
        it "pipeChars passes the input to the command" $
            (Stream.fromList "hello\n" & pipeChars "tr a-z A-Z" & Stream.toList)
                `shouldReturn` "HELLO\n"

        it "pipeBytes passes the input to the command" $ do
            out <-
                toBytes "echo hello"
                    & pipeBytes "tr a-z A-Z"
                    & Stream.toList
            bytesToString out `shouldBe` "HELLO\n"

        it "pipeChunks passes the input to the command" $ do
            out <-
                toChunks "echo hello"
                    & pipeChunks "tr a-z A-Z"
                    & Stream.toList
            bytesToString (concatMap Array.toList out) `shouldBe` "HELLO\n"

        it "pipes a large input through the command unchanged" $
            (Stream.fromList largeInput & pipeChars "cat" & Stream.toList)
                `shouldReturn` largeInput

        it "chains several commands" $
            (toChars "printf 'b\\na\\nc\\n'"
                & pipeChars "sort"
                & pipeChars "tr a-z A-Z"
                & Stream.toList)
                `shouldReturn` "A\nB\nC\n"

        it "raises ProcessFailure when the command fails" $
            (Stream.fromList "x" & pipeChars "cat > /dev/null; exit 2"
                & Stream.toList)
                `shouldThrow` isProcessFailure

    describe "helpers" $ do
        it "runWith runs a process function through the shell" $
            runWith Process.toString "echo hello" `shouldReturn` "hello\n"

        it "streamWith runs a stream function through the shell" $
            (streamWith Process.toChars "echo hello" & Stream.toList)
                `shouldReturn` "hello\n"

        it "pipeWith runs a pipe function through the shell" $
            (Stream.fromList "hello\n"
                & pipeWith Process.pipeChars "tr a-z A-Z"
                & Stream.toList)
                `shouldReturn` "HELLO\n"
