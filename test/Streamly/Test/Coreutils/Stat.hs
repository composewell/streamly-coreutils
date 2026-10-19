-- |
--
-- Module      : Streamly.Test.Coreutils.Stat
-- Copyright   : (c) 2026 Composewell Technologies
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Tests for 'Streamly.Coreutils.Stat'. The expected behavior is that of GNU
-- @stat@ and @stat -L@.

module Streamly.Test.Coreutils.Stat (main) where

import System.Directory (createDirectory, createFileLink)
import System.FilePath ((</>))
import System.IO.Error (isDoesNotExistError)
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

import Streamly.Coreutils.Stat (followSymlinks, stat)

import qualified Streamly.FileSystem.Path as Path
import qualified System.PosixCompat.Files as Files

-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

withTempDir :: (FilePath -> IO a) -> IO a
withTempDir = withSystemTempDirectory "stat-test-"

statS :: Bool -> FilePath -> IO Files.FileStatus
statS follow p = Path.fromString p >>= stat (followSymlinks follow)

-------------------------------------------------------------------------------
-- Tests
-------------------------------------------------------------------------------

main :: IO ()
main = hspec $ do
    describe "stat" $ do
        it "reports a regular file and its size" $ withTempDir $ \d -> do
            writeFile (d </> "f") "hello"
            st <- statS False (d </> "f")
            Files.isRegularFile st `shouldBe` True
            Files.fileSize st `shouldBe` 5

        it "reports a directory" $ withTempDir $ \d -> do
            createDirectory (d </> "dir")
            st <- statS False (d </> "dir")
            Files.isDirectory st `shouldBe` True

        it "reports a symlink itself, not its target" $ withTempDir $ \d -> do
            writeFile (d </> "f") "hello"
            createFileLink "f" (d </> "link")
            st <- statS False (d </> "link")
            Files.isSymbolicLink st `shouldBe` True

        it "reports a dangling symlink" $ withTempDir $ \d -> do
            createFileLink "nowhere" (d </> "link")
            st <- statS False (d </> "link")
            Files.isSymbolicLink st `shouldBe` True

        it "uses the defaults with id" $ withTempDir $ \d -> do
            createFileLink "nowhere" (d </> "link")
            st <- Path.fromString (d </> "link") >>= stat id
            Files.isSymbolicLink st `shouldBe` True

        it "fails on a missing path" $ withTempDir $ \d ->
            statS False (d </> "missing") `shouldThrow` isDoesNotExistError

    describe "stat (followSymlinks True)" $ do
        it "reports the target of a symlink" $ withTempDir $ \d -> do
            writeFile (d </> "f") "hello"
            createFileLink "f" (d </> "link")
            st <- statS True (d </> "link")
            Files.isRegularFile st `shouldBe` True
            Files.fileSize st `shouldBe` 5

        it "reports a regular file" $ withTempDir $ \d -> do
            writeFile (d </> "f") "hello"
            st <- statS True (d </> "f")
            Files.isRegularFile st `shouldBe` True

        it "fails on a dangling symlink" $ withTempDir $ \d -> do
            createFileLink "nowhere" (d </> "link")
            statS True (d </> "link") `shouldThrow` isDoesNotExistError
