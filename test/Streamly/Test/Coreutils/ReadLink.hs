-- |
--
-- Module      : Streamly.Test.Coreutils.ReadLink
-- Copyright   : (c) 2026 Composewell Technologies
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Tests for 'Streamly.Coreutils.ReadLink'. The expected behavior is that of
-- GNU @readlink@ without options.

module Streamly.Test.Coreutils.ReadLink (main) where

import System.Directory (createDirectory, createFileLink)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

import Streamly.Coreutils.ReadLink (readLink)

import qualified Streamly.FileSystem.Path as Path

-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

withTempDir :: (FilePath -> IO a) -> IO a
withTempDir = withSystemTempDirectory "readlink-test-"

readLinkS :: FilePath -> IO FilePath
readLinkS p = Path.toString <$> (Path.fromString p >>= readLink)

-------------------------------------------------------------------------------
-- Tests
-------------------------------------------------------------------------------

main :: IO ()
main = hspec $
    describe "readLink" $ do
        it "returns a relative target as stored" $ withTempDir $ \d -> do
            writeFile (d </> "target") ""
            createFileLink "target" (d </> "link")
            readLinkS (d </> "link") `shouldReturn` "target"

        it "returns an absolute target as stored" $ withTempDir $ \d -> do
            writeFile (d </> "target") ""
            createFileLink (d </> "target") (d </> "link")
            readLinkS (d </> "link") `shouldReturn` d </> "target"

        it "returns the target of a dangling link" $ withTempDir $ \d -> do
            createFileLink "nowhere" (d </> "link")
            readLinkS (d </> "link") `shouldReturn` "nowhere"

        it "returns one level of a chain of links" $ withTempDir $ \d -> do
            writeFile (d </> "target") ""
            createFileLink "target" (d </> "link1")
            createFileLink "link1" (d </> "link2")
            readLinkS (d </> "link2") `shouldReturn` "link1"

        it "fails on a regular file" $ withTempDir $ \d -> do
            writeFile (d </> "file") ""
            readLinkS (d </> "file") `shouldThrow` anyIOException

        it "fails on a directory" $ withTempDir $ \d -> do
            createDirectory (d </> "dir")
            readLinkS (d </> "dir") `shouldThrow` anyIOException

        it "fails on a missing path" $ withTempDir $ \d ->
            readLinkS (d </> "missing") `shouldThrow` anyIOException
