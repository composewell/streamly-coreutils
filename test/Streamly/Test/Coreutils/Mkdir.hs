-- |
--
-- Module      : Streamly.Test.Coreutils.Mkdir
-- Copyright   : (c) 2026 Composewell Technologies
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Tests for 'Streamly.Coreutils.Mkdir'. The expected behavior is that of GNU
-- @mkdir@.

module Streamly.Test.Coreutils.Mkdir (main) where

import System.Directory (createDirectory, doesDirectoryExist)
import System.FilePath ((</>))
import System.IO.Error (isAlreadyExistsError, isDoesNotExistError)
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

import Streamly.Coreutils.Mkdir (makeParents, mkdir)

import qualified Streamly.FileSystem.Path as Path

-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

withTempDir :: (FilePath -> IO a) -> IO a
withTempDir = withSystemTempDirectory "mkdir-test-"

mkdirS :: Bool -> FilePath -> IO ()
mkdirS p dir = Path.fromString dir >>= mkdir (makeParents p)

-------------------------------------------------------------------------------
-- Tests
-------------------------------------------------------------------------------

main :: IO ()
main = hspec $ do
    describe "mkdir" $ do
        it "creates a directory" $ withTempDir $ \d -> do
            mkdirS False (d </> "a")
            doesDirectoryExist (d </> "a") `shouldReturn` True

        it "fails on an existing directory" $ withTempDir $ \d -> do
            createDirectory (d </> "a")
            mkdirS False (d </> "a") `shouldThrow` isAlreadyExistsError

        it "fails on an existing file" $ withTempDir $ \d -> do
            writeFile (d </> "a") ""
            mkdirS False (d </> "a") `shouldThrow` isAlreadyExistsError

        it "fails on a missing parent" $ withTempDir $ \d ->
            mkdirS False (d </> "a" </> "b") `shouldThrow` isDoesNotExistError

        it "uses the defaults with id" $ withTempDir $ \d -> do
            Path.fromString (d </> "a") >>= mkdir id
            doesDirectoryExist (d </> "a") `shouldReturn` True

    describe "mkdir (makeParents True)" $ do
        it "creates the missing parents" $ withTempDir $ \d -> do
            mkdirS True (d </> "a" </> "b" </> "c")
            doesDirectoryExist (d </> "a" </> "b" </> "c") `shouldReturn` True

        it "succeeds on an existing directory" $ withTempDir $ \d -> do
            createDirectory (d </> "a")
            mkdirS True (d </> "a")
            doesDirectoryExist (d </> "a") `shouldReturn` True

        it "fails on an existing file" $ withTempDir $ \d -> do
            writeFile (d </> "a") ""
            mkdirS True (d </> "a") `shouldThrow` isAlreadyExistsError

        it "fails when a parent is a file" $ withTempDir $ \d -> do
            writeFile (d </> "a") ""
            mkdirS True (d </> "a" </> "b") `shouldThrow` anyIOException
