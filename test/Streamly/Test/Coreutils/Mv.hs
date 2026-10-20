{-# LANGUAGE CPP #-}
-- |
--
-- Module      : Streamly.Test.Coreutils.Mv
-- Copyright   : (c) 2026 Composewell Technologies
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Tests for 'Streamly.Coreutils.Mv'. Behavior mirrors GNU @mv@ unless noted
-- otherwise in the module documentation.

module Streamly.Test.Coreutils.Mv (main) where

import Control.Exception (IOException, bracket, try, SomeException)
import Control.Monad (void)
import Data.List (isInfixOf)
import Data.Time.Clock (addUTCTime, getCurrentTime)
import System.Directory
    ( createDirectory
    , createDirectoryIfMissing
    , createDirectoryLink
    , createFileLink
    , doesDirectoryExist
    , doesFileExist
    , doesPathExist
    , getTemporaryDirectory
    , pathIsSymbolicLink
    , removePathForcibly
    , setModificationTime
    , withCurrentDirectory
    )
import System.FilePath ((</>))
import System.IO.Error (isAlreadyExistsError)
import System.IO.Temp (createTempDirectory)
import Test.Hspec

import Streamly.Coreutils.Mv hiding (mv, mvMany)
import qualified Streamly.Coreutils.Mv as Mv
import qualified Streamly.FileSystem.Path as Path

-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

mv :: (MvOptions -> MvOptions) -> FilePath -> FilePath -> IO ()
mv f src dest = do
    srcP <- Path.fromString src
    destP <- Path.fromString dest
    Mv.mv f srcP destP

mvMany :: (MvOptions -> MvOptions) -> [FilePath] -> FilePath -> IO ()
mvMany f srcs dir = do
    srcsP <- mapM Path.fromString srcs
    dirP <- Path.fromString dir
    Mv.mvMany f srcsP dirP

-- | Run a test inside a fresh temporary directory, cleaning up afterwards.
withTempDir :: (FilePath -> IO ()) -> IO ()
withTempDir action = do
    tmpRoot <- getTemporaryDirectory
    bracket
        (createTempDirectory tmpRoot "mv-test-")
        removePathForcibly'
        action
  where
    -- Best-effort cleanup; we do not want cleanup failures to mask test
    -- failures.
    removePathForcibly' p =
        void (try (removePathForcibly p) :: IO (Either SomeException ()))

shouldNotExist :: FilePath -> IO ()
shouldNotExist path = doesPathExist path `shouldReturn` False

shouldContain' :: FilePath -> String -> IO ()
shouldContain' path content = readFile' path `shouldReturn` content

-- Strict read, so that the file is closed before it is moved or removed.
readFile' :: FilePath -> IO String
readFile' path = do
    s <- readFile path
    length s `seq` return s

shouldFail :: IO () -> IO ()
shouldFail action = do
    r <- try action :: IO (Either SomeException ())
    case r of
        Left _ -> return ()
        Right _ ->
            expectationFailure "Expected an exception but none was thrown"

-- | Assert that the action raises an 'IOException' whose message contains
-- each of the given strings.
shouldFailExists :: IO () -> IO ()
shouldFailExists action = do
    r <- try action
    case r of
        Left e -> e `shouldSatisfy` isAlreadyExistsError
        Right _ ->
            expectationFailure "Expected an exception but none was thrown"

shouldFailMentioning :: IO () -> [String] -> IO ()
shouldFailMentioning action names = do
    r <- try action :: IO (Either IOException ())
    case r of
        Left e -> mapM_ (\n -> show e `shouldSatisfy` isInfixOf n) names
        Right _ ->
            expectationFailure "Expected an exception but none was thrown"

-- | Set the modification time of a file to the given number of seconds from
-- now.
setAge :: FilePath -> Integer -> IO ()
setAge path secs = do
    now <- getCurrentTime
    setModificationTime path (addUTCTime (fromInteger secs) now)

-- | A file name that is not ASCII, to check the path encoding.
nonAscii :: FilePath
nonAscii = "f\252r-\1092\1072\1081\1083"

-- | A relative directory path of more than 260 characters, the Windows
-- MAX_PATH limit.
longDir :: FilePath
longDir = foldr1 (</>) (replicate 6 (replicate 50 'd'))

-------------------------------------------------------------------------------
-- Tests
-------------------------------------------------------------------------------

main :: IO ()
main = hspec $ do

    describe "mv (no options)" $ do

        it "renames a file to a new name" $ withTempDir $ \d -> do
            writeFile (d </> "a") "a"
            mv id (d </> "a") (d </> "b")
            shouldNotExist (d </> "a")
            (d </> "b") `shouldContain'` "a"

        it "renames a directory to a new name" $ withTempDir $ \d -> do
            createDirectory (d </> "x")
            writeFile (d </> "x" </> "f") "f"
            mv id (d </> "x") (d </> "y")
            shouldNotExist (d </> "x")
            (d </> "y" </> "f") `shouldContain'` "f"

        it "overwrites an existing file" $ withTempDir $ \d -> do
            writeFile (d </> "a") "a"
            writeFile (d </> "b") "b"
            mv id (d </> "a") (d </> "b")
            shouldNotExist (d </> "a")
            (d </> "b") `shouldContain'` "a"

        it "moves a file into an existing directory" $ withTempDir $ \d -> do
            writeFile (d </> "a") "a"
            createDirectory (d </> "dir")
            mv id (d </> "a") (d </> "dir")
            shouldNotExist (d </> "a")
            (d </> "dir" </> "a") `shouldContain'` "a"

        it "replaces a file of the same name in the directory"
            $ withTempDir $ \d -> do
            writeFile (d </> "a") "new"
            createDirectory (d </> "dir")
            writeFile (d </> "dir" </> "a") "old"
            mv id (d </> "a") (d </> "dir")
            (d </> "dir" </> "a") `shouldContain'` "new"

        it "moves a source with a trailing separator into a directory"
            $ withTempDir $ \d -> do
            createDirectory (d </> "x")
            createDirectory (d </> "dir")
            mv id (d </> "x" ++ "/") (d </> "dir")
            shouldNotExist (d </> "x")
            doesPathExist (d </> "dir" </> "x") `shouldReturn` True

        it "moves into the directory a symlink points to"
            $ withTempDir $ \d -> do
            writeFile (d </> "a") "a"
            createDirectory (d </> "dir")
            createDirectoryLink "dir" (d </> "link")
            mv id (d </> "a") (d </> "link")
            (d </> "dir" </> "a") `shouldContain'` "a"
            pathIsSymbolicLink (d </> "link") `shouldReturn` True

        it "fails on a missing source" $ withTempDir $ \d ->
            shouldFail (mv id (d </> "missing") (d </> "b"))

        it "moves a directory into an existing directory"
            $ withTempDir $ \d -> do
            createDirectory (d </> "x")
            writeFile (d </> "x" </> "f") "f"
            createDirectory (d </> "dir")
            mv id (d </> "x") (d </> "dir")
            shouldNotExist (d </> "x")
            (d </> "dir" </> "x" </> "f") `shouldContain'` "f"

        it "moves into a directory given with a trailing separator"
            $ withTempDir $ \d -> do
            writeFile (d </> "a") "a"
            createDirectory (d </> "dir")
            mv id (d </> "a") (d </> "dir" ++ "/")
            (d </> "dir" </> "a") `shouldContain'` "a"

        it "fails to replace a directory in the destination with a file"
            $ withTempDir $ \d -> do
            writeFile (d </> "a") "a"
            createDirectoryIfMissing True (d </> "dir" </> "a")
            shouldFail (mv id (d </> "a") (d </> "dir"))
            (d </> "a") `shouldContain'` "a"
            doesDirectoryExist (d </> "dir" </> "a") `shouldReturn` True

        it "works with relative paths" $ withTempDir $ \d -> do
            writeFile (d </> "a") "a"
            createDirectory (d </> "dir")
            withCurrentDirectory d $ mv id "a" "dir"
            (d </> "dir" </> "a") `shouldContain'` "a"

        it "works with non-ASCII names" $ withTempDir $ \d -> do
            writeFile (d </> nonAscii) "a"
            mv id (d </> nonAscii) (d </> nonAscii ++ "-2")
            shouldNotExist (d </> nonAscii)
            (d </> nonAscii ++ "-2") `shouldContain'` "a"

        it "works with paths longer than 260 characters"
            $ withTempDir $ \d -> do
            let deep = d </> longDir
            createDirectoryIfMissing True deep
            writeFile (deep </> "a") "a"
            mv id (deep </> "a") (deep </> "b")
            (deep </> "b") `shouldContain'` "a"

    describe "mv (overwrite OverwriteNever)" $ do

        it "keeps an existing destination" $ withTempDir $ \d -> do
            writeFile (d </> "a") "a"
            writeFile (d </> "b") "b"
            mv (overwrite OverwriteNever) (d </> "a") (d </> "b")
            (d </> "a") `shouldContain'` "a"
            (d </> "b") `shouldContain'` "b"

        it "moves to a missing destination" $ withTempDir $ \d -> do
            writeFile (d </> "a") "a"
            mv (overwrite OverwriteNever) (d </> "a") (d </> "b")
            shouldNotExist (d </> "a")
            (d </> "b") `shouldContain'` "a"

        it "keeps a dangling symlink at the destination"
            $ withTempDir $ \d -> do
            writeFile (d </> "a") "a"
            createFileLink "nowhere" (d </> "b")
            mv (overwrite OverwriteNever) (d </> "a") (d </> "b")
            (d </> "a") `shouldContain'` "a"
            pathIsSymbolicLink (d </> "b") `shouldReturn` True

        it "keeps an existing file in the destination directory"
            $ withTempDir $ \d -> do
            writeFile (d </> "a") "new"
            createDirectory (d </> "dir")
            writeFile (d </> "dir" </> "a") "old"
            mv (overwrite OverwriteNever) (d </> "a") (d </> "dir")
            (d </> "a") `shouldContain'` "new"
            (d </> "dir" </> "a") `shouldContain'` "old"

        it "keeps an existing directory" $ withTempDir $ \d -> do
            createDirectory (d </> "x")
            createDirectory (d </> "y")
            mv (overwrite OverwriteNever . intoDirectory False)
                (d </> "x") (d </> "y")
            doesDirectoryExist (d </> "x") `shouldReturn` True
            shouldNotExist (d </> "y" </> "x")

        it "fails on a missing source" $ withTempDir $ \d ->
            shouldFail
                (mv (overwrite OverwriteNever) (d </> "missing") (d </> "b"))

        it "works with relative paths" $ withTempDir $ \d -> do
            writeFile (d </> "a") "a"
            writeFile (d </> "b") "b"
            writeFile (d </> "c") "c"
            withCurrentDirectory d $ do
                mv (overwrite OverwriteNever) "a" "b"
                mv (overwrite OverwriteNever) "c" "e"
            (d </> "a") `shouldContain'` "a"
            (d </> "b") `shouldContain'` "b"
            (d </> "e") `shouldContain'` "c"

        it "works with non-ASCII names" $ withTempDir $ \d -> do
            writeFile (d </> nonAscii) "a"
            writeFile (d </> "b") "b"
            mv (overwrite OverwriteNever) (d </> nonAscii) (d </> "b")
            (d </> "b") `shouldContain'` "b"
            mv (overwrite OverwriteNever)
                (d </> nonAscii) (d </> nonAscii ++ "-2")
            shouldNotExist (d </> nonAscii)
            (d </> nonAscii ++ "-2") `shouldContain'` "a"

        it "works with paths longer than 260 characters"
            $ withTempDir $ \d -> do
            let deep = d </> longDir
            createDirectoryIfMissing True deep
            writeFile (deep </> "a") "a"
            writeFile (deep </> "b") "b"
            mv (overwrite OverwriteNever) (deep </> "a") (deep </> "b")
            (deep </> "b") `shouldContain'` "b"
            mv (overwrite OverwriteNever) (deep </> "a") (deep </> "c")
            shouldNotExist (deep </> "a")
            (deep </> "c") `shouldContain'` "a"

    describe "mv (overwrite OverwriteError)" $ do

        it "fails and keeps an existing destination" $ withTempDir $ \d -> do
            writeFile (d </> "a") "a"
            writeFile (d </> "b") "b"
            shouldFailExists
                (mv (overwrite OverwriteError) (d </> "a") (d </> "b"))
            (d </> "a") `shouldContain'` "a"
            (d </> "b") `shouldContain'` "b"

        it "moves to a missing destination" $ withTempDir $ \d -> do
            writeFile (d </> "a") "a"
            mv (overwrite OverwriteError) (d </> "a") (d </> "b")
            shouldNotExist (d </> "a")
            (d </> "b") `shouldContain'` "a"

        it "fails on a dangling symlink at the destination"
            $ withTempDir $ \d -> do
            writeFile (d </> "a") "a"
            createFileLink "nowhere" (d </> "b")
            shouldFailExists
                (mv (overwrite OverwriteError) (d </> "a") (d </> "b"))
            (d </> "a") `shouldContain'` "a"
            pathIsSymbolicLink (d </> "b") `shouldReturn` True

        it "fails on an existing file in the destination directory"
            $ withTempDir $ \d -> do
            writeFile (d </> "a") "new"
            createDirectory (d </> "dir")
            writeFile (d </> "dir" </> "a") "old"
            shouldFailExists
                (mv (overwrite OverwriteError) (d </> "a") (d </> "dir"))
            (d </> "a") `shouldContain'` "new"
            (d </> "dir" </> "a") `shouldContain'` "old"

    describe "mv (overwrite OverwriteOnly)" $ do

        it "does nothing if the destination is missing"
            $ withTempDir $ \d -> do
            writeFile (d </> "a") "a"
            mv (overwrite OverwriteOnly) (d </> "a") (d </> "b")
            (d </> "a") `shouldContain'` "a"
            shouldNotExist (d </> "b")

        it "replaces an existing destination" $ withTempDir $ \d -> do
            writeFile (d </> "a") "a"
            writeFile (d </> "b") "b"
            mv (overwrite OverwriteOnly) (d </> "a") (d </> "b")
            shouldNotExist (d </> "a")
            (d </> "b") `shouldContain'` "a"

    describe "mv (overwrite OverwriteUpdate)" $ do

        it "replaces an older destination" $ withTempDir $ \d -> do
            writeFile (d </> "a") "a"
            writeFile (d </> "b") "b"
            setAge (d </> "b") (-100)
            mv (overwrite OverwriteUpdate) (d </> "a") (d </> "b")
            shouldNotExist (d </> "a")
            (d </> "b") `shouldContain'` "a"

        it "keeps a newer destination" $ withTempDir $ \d -> do
            writeFile (d </> "a") "a"
            writeFile (d </> "b") "b"
            setAge (d </> "a") (-100)
            mv (overwrite OverwriteUpdate) (d </> "a") (d </> "b")
            (d </> "a") `shouldContain'` "a"
            (d </> "b") `shouldContain'` "b"

        it "moves to a missing destination" $ withTempDir $ \d -> do
            writeFile (d </> "a") "a"
            mv (overwrite OverwriteUpdate) (d </> "a") (d </> "b")
            shouldNotExist (d </> "a")
            (d </> "b") `shouldContain'` "a"

        it "compares with the file of the same name in the directory"
            $ withTempDir $ \d -> do
            writeFile (d </> "a") "a"
            createDirectory (d </> "dir")
            writeFile (d </> "dir" </> "a") "old"
            setAge (d </> "a") (-100)
            mv (overwrite OverwriteUpdate) (d </> "a") (d </> "dir")
            (d </> "a") `shouldContain'` "a"
            (d </> "dir" </> "a") `shouldContain'` "old"

    describe "mv (intoDirectory False)" $ do

        it "fails to replace a directory with a file" $ withTempDir $ \d -> do
            writeFile (d </> "a") "a"
            createDirectory (d </> "dir")
            shouldFail (mv (intoDirectory False) (d </> "a") (d </> "dir"))
            (d </> "a") `shouldContain'` "a"
            shouldNotExist (d </> "dir" </> "a")

        it "renames to a missing destination" $ withTempDir $ \d -> do
            writeFile (d </> "a") "a"
            mv (intoDirectory False) (d </> "a") (d </> "b")
            (d </> "b") `shouldContain'` "a"

#if !defined(CABAL_OS_WINDOWS)
        -- GNU mv -T replaces the symlink. MoveFileEx cannot replace a
        -- directory symlink with a file, so mv fails on Windows.
        it "replaces a symlink to a directory instead of moving into it"
            $ withTempDir $ \d -> do
            writeFile (d </> "a") "a"
            createDirectory (d </> "dir")
            createDirectoryLink "dir" (d </> "link")
            mv (intoDirectory False) (d </> "a") (d </> "link")
            pathIsSymbolicLink (d </> "link") `shouldReturn` False
            (d </> "link") `shouldContain'` "a"
            shouldNotExist (d </> "dir" </> "a")
#endif

#if !defined(CABAL_OS_WINDOWS)
        -- rename(2) replaces an empty directory; MoveFileEx does not.
        it "replaces an empty directory with a directory"
            $ withTempDir $ \d -> do
            createDirectory (d </> "x")
            writeFile (d </> "x" </> "f") "f"
            createDirectory (d </> "y")
            mv (intoDirectory False) (d </> "x") (d </> "y")
            shouldNotExist (d </> "x")
            (d </> "y" </> "f") `shouldContain'` "f"
#endif

        it "fails to replace a non-empty directory" $ withTempDir $ \d -> do
            createDirectory (d </> "x")
            createDirectory (d </> "y")
            writeFile (d </> "y" </> "f") "f"
            shouldFail (mv (intoDirectory False) (d </> "x") (d </> "y"))
            doesDirectoryExist (d </> "x") `shouldReturn` True
            (d </> "y" </> "f") `shouldContain'` "f"

    describe "mvMany" $ do

        it "moves several sources into a directory" $ withTempDir $ \d -> do
            writeFile (d </> "a") "a"
            writeFile (d </> "b") "b"
            createDirectory (d </> "dir")
            mvMany id [d </> "a", d </> "b"] (d </> "dir")
            shouldNotExist (d </> "a")
            shouldNotExist (d </> "b")
            (d </> "dir" </> "a") `shouldContain'` "a"
            (d </> "dir" </> "b") `shouldContain'` "b"

        it "fails without moving anything if the target is not a directory"
            $ withTempDir $ \d -> do
            writeFile (d </> "a") "a"
            writeFile (d </> "f") "f"
            shouldFail (mvMany id [d </> "a"] (d </> "f"))
            (d </> "a") `shouldContain'` "a"
            (d </> "f") `shouldContain'` "f"

        it "fails if the target is missing" $ withTempDir $ \d -> do
            writeFile (d </> "a") "a"
            shouldFail (mvMany id [d </> "a"] (d </> "missing"))
            (d </> "a") `shouldContain'` "a"

        it "moves the remaining sources after a failure, then fails"
            $ withTempDir $ \d -> do
            writeFile (d </> "a") "a"
            writeFile (d </> "c") "c"
            createDirectory (d </> "dir")
            shouldFail
                (mvMany id
                    [d </> "a", d </> "missing", d </> "c"] (d </> "dir"))
            (d </> "dir" </> "a") `shouldContain'` "a"
            (d </> "dir" </> "c") `shouldContain'` "c"

        it "applies the overwrite option to each source"
            $ withTempDir $ \d -> do
            writeFile (d </> "a") "new"
            writeFile (d </> "b") "b"
            createDirectory (d </> "dir")
            writeFile (d </> "dir" </> "a") "old"
            mvMany (overwrite OverwriteNever)
                [d </> "a", d </> "b"] (d </> "dir")
            (d </> "a") `shouldContain'` "new"
            (d </> "dir" </> "a") `shouldContain'` "old"
            (d </> "dir" </> "b") `shouldContain'` "b"

        it "moves the other sources with OverwriteError"
            $ withTempDir $ \d -> do
            writeFile (d </> "a") "new"
            writeFile (d </> "b") "b"
            createDirectory (d </> "dir")
            writeFile (d </> "dir" </> "a") "old"
            mvMany (overwrite OverwriteError)
                [d </> "a", d </> "b"] (d </> "dir")
                `shouldFailMentioning` [d </> "dir" </> "a"]
            (d </> "a") `shouldContain'` "new"
            (d </> "dir" </> "a") `shouldContain'` "old"
            (d </> "dir" </> "b") `shouldContain'` "b"

        it "names every failed source in the error" $ withTempDir $ \d -> do
            createDirectory (d </> "dir")
            mvMany id [d </> "missing1", d </> "missing2"] (d </> "dir")
                `shouldFailMentioning` ["missing1", "missing2"]

        it "does nothing for an empty source list" $ withTempDir $ \d -> do
            createDirectory (d </> "dir")
            mvMany id [] (d </> "dir")
            doesDirectoryExist (d </> "dir") `shouldReturn` True

        it "moves sources with trailing separators" $ withTempDir $ \d -> do
            createDirectory (d </> "x")
            createDirectory (d </> "y")
            createDirectory (d </> "dir")
            mvMany id [d </> "x" ++ "/", d </> "y" ++ "/"] (d </> "dir")
            doesDirectoryExist (d </> "dir" </> "x") `shouldReturn` True
            doesDirectoryExist (d </> "dir" </> "y") `shouldReturn` True

        it "moves into the directory a symlink points to"
            $ withTempDir $ \d -> do
            writeFile (d </> "a") "a"
            createDirectory (d </> "dir")
            createDirectoryLink "dir" (d </> "link")
            mvMany id [d </> "a"] (d </> "link")
            (d </> "dir" </> "a") `shouldContain'` "a"

        it "fails if the target is a symlink to a file"
            $ withTempDir $ \d -> do
            writeFile (d </> "a") "a"
            writeFile (d </> "f") "f"
            createFileLink "f" (d </> "link")
            shouldFail (mvMany id [d </> "a"] (d </> "link"))
            (d </> "a") `shouldContain'` "a"
            doesFileExist (d </> "f") `shouldReturn` True

        it "applies OverwriteUpdate to each source" $ withTempDir $ \d -> do
            writeFile (d </> "a") "a"
            writeFile (d </> "b") "b"
            createDirectory (d </> "dir")
            writeFile (d </> "dir" </> "a") "old"
            writeFile (d </> "dir" </> "b") "new"
            setAge (d </> "dir" </> "a") (-100)
            setAge (d </> "b") (-100)
            mvMany (overwrite OverwriteUpdate)
                [d </> "a", d </> "b"] (d </> "dir")
            (d </> "dir" </> "a") `shouldContain'` "a"
            (d </> "b") `shouldContain'` "b"
            (d </> "dir" </> "b") `shouldContain'` "new"
