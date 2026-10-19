-- |
--
-- Module      : Streamly.Test.Coreutils.Which
-- Copyright   : (c) 2026 Composewell Technologies
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Tests for 'Streamly.Coreutils.Which'.

module Streamly.Test.Coreutils.Which (main) where

import Control.Exception (bracket)
import System.Directory
    ( createDirectory
    , exeExtension
    , getPermissions
    , setOwnerExecutable
    , setPermissions
    )
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.FilePath ((</>), (<.>), searchPathSeparator)
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

import Streamly.Coreutils.Which (which, whichAll, whichAllIn)

import qualified Streamly.FileSystem.Path as Path

-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

-- | Two directories, "d1" and "d2", in a fresh temporary directory.
withTwoDirs :: (FilePath -> FilePath -> IO a) -> IO a
withTwoDirs action =
    withSystemTempDirectory "which-test-" $ \d -> do
        let d1 = d </> "d1"
            d2 = d </> "d2"
        createDirectory d1
        createDirectory d2
        action d1 d2

-- | File name of an executable called @name@ on this platform.
exeName :: String -> FilePath
exeName name = name <.> exeExtension

-- | Create an executable file called @name@ in the directory.
makeExe :: FilePath -> String -> IO FilePath
makeExe dir name = do
    let file = dir </> exeName name
    writeFile file ""
    perms <- getPermissions file
    setPermissions file (setOwnerExecutable True perms)
    return file

-- | Run the action with PATH set to the given directories.
withPath :: [FilePath] -> IO a -> IO a
withPath dirs action =
    bracket (lookupEnv "PATH") restore $ \_ -> do
        setEnv "PATH" (foldr1 (\a b -> a ++ [searchPathSeparator] ++ b) dirs)
        action

    where

    restore = maybe (unsetEnv "PATH") (setEnv "PATH")

toStrings :: [Path.Path] -> [FilePath]
toStrings = map Path.toString

-------------------------------------------------------------------------------
-- Tests
-------------------------------------------------------------------------------

main :: IO ()
main = hspec $ do
    describe "whichAllIn" $ do
        it "finds the executable in every directory, in order"
            $ withTwoDirs $ \d1 d2 -> do
                f1 <- makeExe d1 "prog"
                f2 <- makeExe d2 "prog"
                dirs <- mapM Path.fromString [d2, d1]
                toStrings <$> whichAllIn dirs "prog" `shouldReturn` [f2, f1]

        it "returns an empty list for a missing name"
            $ withTwoDirs $ \d1 d2 -> do
                dirs <- mapM Path.fromString [d1, d2]
                toStrings <$> whichAllIn dirs "missing" `shouldReturn` []

        it "skips directories that do not contain the name"
            $ withTwoDirs $ \d1 d2 -> do
                f2 <- makeExe d2 "prog"
                dirs <- mapM Path.fromString [d1, d2]
                toStrings <$> whichAllIn dirs "prog" `shouldReturn` [f2]

        it "returns an empty list for no directories" $
            toStrings <$> whichAllIn [] "prog" `shouldReturn` []

    describe "which" $ do
        it "finds the first executable on the PATH"
            $ withTwoDirs $ \d1 d2 -> do
                _ <- makeExe d1 "prog"
                _ <- makeExe d2 "prog"
                r <- withPath [d1, d2] (which "prog")
                fmap Path.toString r `shouldBe` Just (d1 </> exeName "prog")

        it "returns Nothing for a missing name" $ withTwoDirs $ \d1 d2 -> do
            r <- withPath [d1, d2] (which "missing")
            fmap Path.toString r `shouldBe` Nothing

    describe "whichAll" $
        it "finds the executable in every PATH directory, in order"
            $ withTwoDirs $ \d1 d2 -> do
                f1 <- makeExe d1 "prog"
                f2 <- makeExe d2 "prog"
                r <- withPath [d1, d2] (whichAll "prog")
                toStrings r `shouldBe` [f1, f2]
