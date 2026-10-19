-- |
--
-- Module      : Streamly.Test.Coreutils.Directory
-- Copyright   : (c) 2026 Composewell Technologies
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Tests for 'Streamly.Coreutils.Directory'.

module Streamly.Test.Coreutils.Directory (main) where

import Control.Exception (ErrorCall(..), bracket, throwIO, try)
import System.Directory
    ( canonicalizePath
    , createDirectory
    , getCurrentDirectory
    , getHomeDirectory
    , setCurrentDirectory
    )
import System.FilePath ((</>))
import System.IO.Error (isDoesNotExistError)
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

import Streamly.Coreutils.Directory

import qualified Streamly.FileSystem.Path as Path

-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

-- | Run the action in a fresh temporary directory, restoring the current
-- directory afterwards. The directory is canonicalized because the current
-- directory reported by the OS is canonical (e.g. /private/var on macOS).
withTempCwd :: (FilePath -> IO a) -> IO a
withTempCwd action =
    withSystemTempDirectory "directory-test-" $ \dir ->
        bracket getCurrentDirectory setCurrentDirectory $ \_ ->
            canonicalizePath dir >>= action

toPath :: FilePath -> IO Path.Path
toPath = Path.fromString

-------------------------------------------------------------------------------
-- Tests
-------------------------------------------------------------------------------

main :: IO ()
main = hspec $ do
    describe "home" $
        it "is the home directory of the current user" $ do
            expected <- getHomeDirectory
            fmap Path.toString home `shouldReturn` expected

    describe "pwd" $
        it "is the current directory" $ do
            expected <- getCurrentDirectory
            fmap Path.toString pwd `shouldReturn` expected

    describe "cd" $ do
        it "changes the current directory" $ withTempCwd $ \dir -> do
            toPath dir >>= cd
            fmap Path.toString pwd `shouldReturn` dir

        it "changes to a relative directory" $ withTempCwd $ \dir -> do
            createDirectory (dir </> "sub")
            toPath dir >>= cd
            toPath "sub" >>= cd
            fmap Path.toString pwd `shouldReturn` dir </> "sub"

        it "fails on a missing directory" $ withTempCwd $ \dir -> do
            p <- toPath (dir </> "missing")
            cd p `shouldThrow` isDoesNotExistError

    describe "withCd" $ do
        it "runs the action in the directory" $ withTempCwd $ \dir -> do
            p <- toPath dir
            withCd p (fmap Path.toString pwd `shouldReturn` dir)

        it "restores the current directory" $ withTempCwd $ \dir -> do
            original <- getCurrentDirectory
            p <- toPath dir
            withCd p (return ())
            getCurrentDirectory `shouldReturn` original

        it "restores the current directory when the action fails"
            $ withTempCwd $ \dir -> do
                original <- getCurrentDirectory
                p <- toPath dir
                r <- try (withCd p (throwIO (ErrorCall "boom")))
                r `shouldBe` Left (ErrorCall "boom")
                getCurrentDirectory `shouldReturn` original
