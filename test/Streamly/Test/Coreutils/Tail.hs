{-# LANGUAGE CPP #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
--
-- Module      : Streamly.Test.Coreutils.Tail
-- Copyright   : (c) 2026 Composewell Technologies
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Tests for 'Streamly.Coreutils.Tail'. The output is compared with a model
-- of GNU @tail@, which was checked against GNU @tail@ on random inputs.

module Streamly.Test.Coreutils.Tail (main) where

import Data.Char (chr)
import System.FilePath ((</>))
import System.IO (IOMode(..), hPutStr, withBinaryFile)
import System.IO.Error (isDoesNotExistError)
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec
import Test.Hspec.QuickCheck (modifyMaxSuccess)
import Test.QuickCheck
    (Gen, Property, choose, elements, forAll, ioProperty, listOf, oneof,
     vectorOf)
#if !defined(CABAL_OS_WINDOWS)
import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (SomeException, bracket, try)
import Control.Monad (void)
import System.Posix.Files
    (createNamedPipe, ownerReadMode, ownerWriteMode, unionFileModes)
import System.Posix.IO
    ( OpenFileFlags(append, trunc)
    , OpenMode(WriteOnly)
    , closeFd
    , defaultFileFlags
    , fdWrite
    , openFd
    )
import System.Timeout (timeout)
#endif

import Streamly.Coreutils.Tail
    ( TailOptions
    , bytes
#if !defined(CABAL_OS_WINDOWS)
    , follow
#endif
    , fromByte
    , fromLine
    , lines
    , tail
    )

import qualified Streamly.Data.Array as Array
import qualified Streamly.Data.Stream as Stream
import qualified Streamly.FileSystem.Path as Path

import Prelude hiding (lines, tail)

-------------------------------------------------------------------------------
-- Model of GNU tail
-------------------------------------------------------------------------------

-- | The option given to tail, with its count.
data Mode
    = LastLines Int     -- tail -n N
    | FromLine Int      -- tail -n +N
    | LastBytes Int     -- tail -c N
    | FromByte Int      -- tail -c +N
    deriving Show

-- | Split into lines, each keeping its newline. A last line without a newline
-- is a line too.
splitLines :: String -> [String]
splitLines "" = []
splitLines s =
    case break (== '\n') s of
        (a, []) -> [a]
        (a, _ : rest) -> (a ++ "\n") : splitLines rest

lastN :: Int -> [a] -> [a]
lastN n xs = drop (length xs - max 0 n) xs

model :: Mode -> String -> String
model mode s =
    case mode of
        LastLines n -> concat (lastN n (splitLines s))
        FromLine n -> concat (drop (n - 1) (splitLines s))
        LastBytes n -> lastN n s
        FromByte n -> drop (n - 1) s

type TailModifier = TailOptions -> TailOptions

modifier :: Mode -> TailModifier
modifier mode =
    case mode of
        LastLines n -> lines n
        FromLine n -> fromLine n
        LastBytes n -> bytes n
        FromByte n -> fromByte n

-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

withTempDir :: (FilePath -> IO a) -> IO a
withTempDir = withSystemTempDirectory "tail-test-"

-- | Write in binary mode, so that "\n" is not translated on Windows.
writeBinary :: FilePath -> String -> IO ()
writeBinary file s = withBinaryFile file WriteMode (`hPutStr` s)

tailS :: TailModifier -> FilePath -> IO String
tailS f file = do
    p <- Path.fromString file
    chunks <- Stream.toList (tail f p)
    return (map (chr . fromIntegral) (concatMap Array.toList chunks))

genMode :: Gen Mode
genMode = do
    n <- choose (-2, 40)
    elements [LastLines n, FromLine n, LastBytes n, FromByte n]

genContent :: Gen String
genContent = listOf (elements "ab\n")

-- | Content larger than the 64 KiB block that tail reads backwards.
genLargeContent :: Gen String
genLargeContent = do
    k <- choose (70_000, 200_000)
    vectorOf k (oneof [elements "abcdefgh", pure '\n'])

-- | tail on a regular file matches the model.
propRegular :: Gen String -> Property
propRegular gen =
    forAll gen $ \s ->
    forAll genMode $ \mode -> ioProperty $ withTempDir $ \dir -> do
        let file = dir </> "input"
        writeBinary file s
        out <- tailS (modifier mode) file
        return (out == model mode s)

#if !defined(CABAL_OS_WINDOWS)
#if defined(CABAL_OS_LINUX) || defined(CABAL_OS_DARWIN)
-- | A test of GNU behavior that the implementation does not match yet. It is
-- reported as pending while it fails, and fails once it passes, so that the
-- marker is removed when the bug is fixed.
knownBug :: String -> Expectation -> Expectation
knownBug reason action = do
    r <- try action
    case r of
        Left (_ :: SomeException) -> pendingWith reason
        Right () ->
            expectationFailure
                ("passes now, remove the knownBug marker: " ++ reason)
#endif

-- | Mark a test that needs tail to wait for a writer to open a FIFO. On Linux
-- and macOS, GHC opens the FIFO without blocking, so tail sees EOF before the
-- writer opens it. FreeBSD returns no EOF before the first writer, so the
-- test passes there.
fifoEofBug :: Expectation -> Expectation
#if defined(CABAL_OS_LINUX) || defined(CABAL_OS_DARWIN)
fifoEofBug =
    knownBug "GHC opens the FIFO without blocking, so tail sees EOF"
#else
fifoEofBug = id
#endif

makeFifo :: FilePath -> IO ()
makeFifo fifo =
    createNamedPipe fifo (unionFileModes ownerReadMode ownerWriteMode)

-- | Write the string to a FIFO, then run tail on it. An extra reader is kept
-- open so that the writer can open the FIFO before tail does; GHC opens a
-- FIFO without blocking, and fails to open the writing end of a FIFO that
-- has no reader.
tailFifo :: TailModifier -> FilePath -> String -> IO String
tailFifo f fifo s =
    withBinaryFile fifo ReadMode $ \_ -> do
        withBinaryFile fifo WriteMode (`hPutStr` s)
        tailS f fifo

-- | tail on a FIFO, which cannot be seeked, matches the model.
propFifo :: Property
propFifo =
    forAll genContent $ \s ->
    forAll genMode $ \mode -> ioProperty $ withTempDir $ \dir -> do
        let fifo = dir </> "fifo"
        makeFifo fifo
        out <- tailFifo (modifier mode) fifo s
        return (out == model mode s)

-- | Write to a file through a file descriptor. GHC refuses to open a Handle
-- for writing to a file that this process has open for reading, as tail does.
writeFd :: OpenFileFlags -> FilePath -> String -> IO ()
writeFd flags file s =
    bracket open closeFd (\fd -> void (fdWrite fd s))

    where

#if MIN_VERSION_unix(2,8,0)
    open = openFd file WriteOnly flags
#else
    open = openFd file WriteOnly Nothing flags
#endif

appendUnlocked :: FilePath -> String -> IO ()
appendUnlocked = writeFd defaultFileFlags {append = True}

truncateUnlocked :: FilePath -> String -> IO ()
truncateUnlocked = writeFd defaultFileFlags {trunc = True}

-- | Run the action after a short delay, in another thread.
later :: IO () -> IO ()
later action = void $ forkIO $ threadDelay 300_000 >> action

-- | The first @n@ bytes of the output of tail, failing after 10 seconds.
tailTake :: Int -> TailModifier -> FilePath -> IO (Maybe String)
tailTake n f file = do
    p <- Path.fromString file
    timeout 10_000_000 $ do
        out <-
            Stream.toList
                $ Stream.take n
                $ Stream.unfoldEach Array.reader
                $ tail f p
        return (map (chr . fromIntegral) out)
#endif

-------------------------------------------------------------------------------
-- Tests
-------------------------------------------------------------------------------

main :: IO ()
main = hspec $ do
    describe "model" $
        it "splits lines like GNU tail" $
            splitLines "a\n\nb" `shouldBe` ["a\n", "\n", "b"]

    describe "regular file" $ do
        it "outputs the last 10 lines by default" $ withTempDir $ \d -> do
            let file = d </> "f"
                s = concatMap (\i -> show i ++ "\n") [1 .. 15 :: Int]
            writeBinary file s
            tailS id file `shouldReturn` model (LastLines 10) s

        it "matches GNU tail on small files" $
            propRegular genContent

        modifyMaxSuccess (const 30) $
            it "matches GNU tail on files larger than a block" $
                propRegular genLargeContent

        it "uses the last of several count modifiers" $ withTempDir $ \d -> do
            let file = d </> "f"
            writeBinary file "a\nb\nc\n"
            tailS (lines 2 . bytes 1) file `shouldReturn` "b\nc\n"
            tailS (bytes 1 . lines 2) file `shouldReturn` "\n"

        it "fails on a missing file" $ withTempDir $ \d ->
            tailS id (d </> "missing") `shouldThrow` isDoesNotExistError

#if !defined(CABAL_OS_WINDOWS)
    describe "FIFO" $ do
        modifyMaxSuccess (const 50) $
            it "matches GNU tail" propFifo

        it "waits for a writer to open the FIFO" $ withTempDir $ \d ->
            fifoEofBug $ do
                let fifo = d </> "fifo"
                makeFifo fifo
                -- The writer fails once the test has ended; ignore that.
                later $ void
                    (try (withBinaryFile fifo WriteMode (`hPutStr` "a\n"))
                        :: IO (Either SomeException ()))
                timeout 10_000_000 (tailS id fifo) `shouldReturn` Just "a\n"

    describe "follow" $ do
        it "outputs data appended to the file" $ withTempDir $ \d -> do
            let file = d </> "f"
            writeBinary file "a\n"
            later $ appendUnlocked file "b\n"
            tailTake 4 (follow True) file `shouldReturn` Just "a\nb\n"

        it "starts again from the beginning after truncation"
            $ withTempDir $ \d -> do
                let file = d </> "f"
                writeBinary file "aaaa\n"
                later $ truncateUnlocked file "b\n"
                tailTake 7 (follow True) file `shouldReturn` Just "aaaa\nb\n"

        it "applies the count before following" $ withTempDir $ \d -> do
            let file = d </> "f"
            writeBinary file "a\nb\nc\n"
            later $ appendUnlocked file "d\n"
            tailTake 4 (follow True . lines 1) file
                `shouldReturn` Just "c\nd\n"
#endif
