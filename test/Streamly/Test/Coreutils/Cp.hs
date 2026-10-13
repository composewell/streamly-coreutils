{-# LANGUAGE CPP #-}
-- |
--
-- Module      : Streamly.Test.Coreutils.Cp
-- Copyright   : (c) 2026 Composewell Technologies
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Tests for 'Streamly.Coreutils.Cp'. Behavior mirrors GNU @cp@ unless noted
-- otherwise in the module documentation.

module Streamly.Test.Coreutils.Cp (main) where

import Control.Exception (bracket, try, SomeException)
import Control.Monad (void)
import Data.Time.Clock (addUTCTime, getCurrentTime)
import System.Directory
    ( createDirectory
    , createDirectoryLink
    , createFileLink
    , doesDirectoryExist
    , doesPathExist
    , getTemporaryDirectory
    , pathIsSymbolicLink
    , removePathForcibly
    , setModificationTime
    , withCurrentDirectory
    )
import System.FilePath ((</>))
import System.IO.Temp (createTempDirectory)
#if !defined(CABAL_OS_WINDOWS)
import System.PosixCompat.Files (fileMode, getFileStatus, setFileMode)
import System.Posix.Types (FileMode)
#endif
import Test.Hspec

import Streamly.Coreutils.Cp hiding (cp)
import qualified Streamly.Coreutils.Cp as Cp
import qualified Streamly.FileSystem.Path as Path

-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

cp :: (CpOptions -> CpOptions) -> FilePath -> FilePath -> IO ()
cp f src dest = do
    srcP <- Path.fromString src
    destP <- Path.fromString dest
    Cp.cp f srcP destP

-- | Run a test inside a fresh temporary directory, cleaning up afterwards.
withTempDir :: (FilePath -> IO ()) -> IO ()
withTempDir action = do
    tmpRoot <- getTemporaryDirectory
    bracket
        (createTempDirectory tmpRoot "cp-test-")
        removePathForcibly'
        action
  where
    -- Best-effort cleanup; we do not want cleanup failures to mask test
    -- failures.
    removePathForcibly' p =
        void (try (removePathForcibly p) :: IO (Either SomeException ()))

shouldNotExist :: FilePath -> IO ()
shouldNotExist path = doesPathExist path `shouldReturn` False

-- Strict read, so that the file is closed before it is written or removed.
readFile' :: FilePath -> IO String
readFile' path = do
    s <- readFile path
    length s `seq` return s

shouldContain' :: FilePath -> String -> IO ()
shouldContain' path content = readFile' path `shouldReturn` content

shouldFail :: IO () -> IO ()
shouldFail action = do
    r <- try action :: IO (Either SomeException ())
    case r of
        Left _ -> return ()
        Right _ ->
            expectationFailure "Expected an exception but none was thrown"

-- | Set the modification time of a file to the given number of seconds from
-- now.
setAge :: FilePath -> Integer -> IO ()
setAge path secs = do
    now <- getCurrentTime
    setModificationTime path (addUTCTime (fromInteger secs) now)

#if !defined(CABAL_OS_WINDOWS)
-- | The permission bits of a file.
modeOf :: FilePath -> IO FileMode
modeOf path = (`mod` 0o1000) . fileMode <$> getFileStatus path
#endif

-- | A file name that is not ASCII, to check the path encoding.
nonAscii :: FilePath
nonAscii = "f\252r-\1092\1072\1081\1083"

-------------------------------------------------------------------------------
-- Tests
-------------------------------------------------------------------------------

main :: IO ()
main = hspec $ do

    describe "cp (no options)" $ do

        it "copies a file to a new name" $ withTempDir $ \d -> do
            writeFile (d </> "a") "a"
            cp id (d </> "a") (d </> "b")
            (d </> "a") `shouldContain'` "a"
            (d </> "b") `shouldContain'` "a"

        it "overwrites an existing file" $ withTempDir $ \d -> do
            writeFile (d </> "a") "a"
            writeFile (d </> "b") "longer old content"
            cp id (d </> "a") (d </> "b")
            (d </> "b") `shouldContain'` "a"

        it "copies an empty file" $ withTempDir $ \d -> do
            writeFile (d </> "a") ""
            cp id (d </> "a") (d </> "b")
            (d </> "b") `shouldContain'` ""

        it "copies a file larger than one chunk" $ withTempDir $ \d -> do
            let content = concat (replicate 20000 "0123456789\n")
            writeFile (d </> "a") content
            cp id (d </> "a") (d </> "b")
            (d </> "b") `shouldContain'` content

        it "writes through a symlink at the destination"
            $ withTempDir $ \d -> do
            writeFile (d </> "a") "a"
            writeFile (d </> "target") "old"
            createFileLink "target" (d </> "link")
            cp id (d </> "a") (d </> "link")
            pathIsSymbolicLink (d </> "link") `shouldReturn` True
            (d </> "target") `shouldContain'` "a"

        it "fails on a dangling symlink at the destination"
            $ withTempDir $ \d -> do
            writeFile (d </> "a") "a"
            createFileLink "missing" (d </> "link")
            shouldFail (cp id (d </> "a") (d </> "link"))
            shouldFail (cp (overwrite OverwriteNever) (d </> "a") (d </> "link"))
            shouldFail
                (cp (overwrite OverwriteUpdate) (d </> "a") (d </> "link"))
            shouldNotExist (d </> "missing")
            pathIsSymbolicLink (d </> "link") `shouldReturn` True

        it "fails on a missing source" $ withTempDir $ \d -> do
            shouldFail (cp id (d </> "missing") (d </> "b"))
            shouldNotExist (d </> "b")

        it "fails on a directory source" $ withTempDir $ \d -> do
            createDirectory (d </> "dir")
            shouldFail (cp id (d </> "dir") (d </> "b"))
            shouldFail (cp (cpMethod HardLink) (d </> "dir") (d </> "b"))
            shouldNotExist (d </> "b")

        it "fails if source and destination are the same file"
            $ withTempDir $ \d -> do
            writeFile (d </> "a") "a"
            createFileLink "a" (d </> "link")
            shouldFail (cp id (d </> "a") (d </> "a"))
            shouldFail (cp id (d </> "a") (d </> "link"))
            (d </> "a") `shouldContain'` "a"

        it "works with non-ASCII names" $ withTempDir $ \d -> do
            writeFile (d </> nonAscii) "a"
            cp id (d </> nonAscii) (d </> nonAscii ++ "-2")
            (d </> nonAscii ++ "-2") `shouldContain'` "a"

#if !defined(CABAL_OS_WINDOWS)
        it "gives a new file the permission bits of the source"
            $ withTempDir $ \d -> do
            writeFile (d </> "a") "a"
            setFileMode (d </> "a") 0o600
            cp id (d </> "a") (d </> "b")
            modeOf (d </> "b") `shouldReturn` 0o600

        it "keeps the permission bits of an existing destination"
            $ withTempDir $ \d -> do
            writeFile (d </> "a") "a"
            setFileMode (d </> "a") 0o644
            writeFile (d </> "b") "b"
            setFileMode (d </> "b") 0o600
            cp id (d </> "a") (d </> "b")
            modeOf (d </> "b") `shouldReturn` 0o600
            (d </> "b") `shouldContain'` "a"
#endif

    describe "cp (symlink source)" $ do

        it "copies the contents of the target to a new file"
            $ withTempDir $ \d -> do
            writeFile (d </> "target") "a"
            createFileLink "target" (d </> "a")
            cp id (d </> "a") (d </> "b")
            pathIsSymbolicLink (d </> "b") `shouldReturn` False
            (d </> "b") `shouldContain'` "a"

#if !defined(CABAL_OS_WINDOWS)
        it "gives a new file the permission bits of the target"
            $ withTempDir $ \d -> do
            writeFile (d </> "target") "a"
            setFileMode (d </> "target") 0o640
            createFileLink "target" (d </> "a")
            cp id (d </> "a") (d </> "b")
            modeOf (d </> "b") `shouldReturn` 0o640
#endif

        it "overwrites an existing file with the contents of the target"
            $ withTempDir $ \d -> do
            writeFile (d </> "target") "a"
            createFileLink "target" (d </> "a")
            writeFile (d </> "b") "b"
            cp id (d </> "a") (d </> "b")
            (d </> "b") `shouldContain'` "a"

        it "fails on a dangling symlink" $ withTempDir $ \d -> do
            createFileLink "missing" (d </> "a")
            shouldFail (cp id (d </> "a") (d </> "b"))
            shouldNotExist (d </> "b")

        it "fails on a symlink to a directory" $ withTempDir $ \d -> do
            createDirectory (d </> "dir")
            createDirectoryLink "dir" (d </> "a")
            shouldFail (cp id (d </> "a") (d </> "b"))
            shouldNotExist (d </> "b")

        it "fails on a symlink to the destination" $ withTempDir $ \d -> do
            writeFile (d </> "b") "b"
            createFileLink "b" (d </> "a")
            shouldFail (cp id (d </> "a") (d </> "b"))
            (d </> "b") `shouldContain'` "b"

        it "uses the name of the symlink in a directory destination"
            $ withTempDir $ \d -> do
            writeFile (d </> "target") "a"
            createFileLink "target" (d </> "a")
            createDirectory (d </> "dir")
            cp id (d </> "a") (d </> "dir")
            pathIsSymbolicLink (d </> "dir" </> "a") `shouldReturn` False
            (d </> "dir" </> "a") `shouldContain'` "a"

        it "hard links the target with HardLink" $ withTempDir $ \d -> do
            writeFile (d </> "target") "a"
            createFileLink "target" (d </> "a")
            cp (cpMethod HardLink) (d </> "a") (d </> "b")
            pathIsSymbolicLink (d </> "b") `shouldReturn` False
            appendFile (d </> "target") "b"
            (d </> "b") `shouldContain'` "ab"

        it "fails on a dangling symlink with HardLink" $ withTempDir $ \d -> do
            createFileLink "missing" (d </> "a")
            shouldFail (cp (cpMethod HardLink) (d </> "a") (d </> "b"))
            shouldNotExist (d </> "b")

    describe "cp (directory destination)" $ do

        it "copies a file into an existing directory" $ withTempDir $ \d -> do
            writeFile (d </> "a") "a"
            createDirectory (d </> "dir")
            cp id (d </> "a") (d </> "dir")
            (d </> "dir" </> "a") `shouldContain'` "a"
            (d </> "a") `shouldContain'` "a"

        it "copies into a directory given with a trailing separator"
            $ withTempDir $ \d -> do
            writeFile (d </> "a") "a"
            createDirectory (d </> "dir")
            cp id (d </> "a") (d </> "dir/")
            (d </> "dir" </> "a") `shouldContain'` "a"

        it "copies into the directory a symlink points to"
            $ withTempDir $ \d -> do
            writeFile (d </> "a") "a"
            createDirectory (d </> "dir")
            createDirectoryLink "dir" (d </> "link")
            cp id (d </> "a") (d </> "link")
            (d </> "dir" </> "a") `shouldContain'` "a"

        it "uses the last component of a source path"
            $ withTempDir $ \d -> do
            createDirectory (d </> "src")
            writeFile (d </> "src" </> "a") "a"
            createDirectory (d </> "dir")
            cp id (d </> "src" </> "a") (d </> "dir")
            (d </> "dir" </> "a") `shouldContain'` "a"
            shouldNotExist (d </> "dir" </> "src")

        it "overwrites a file of the same name in the directory"
            $ withTempDir $ \d -> do
            writeFile (d </> "a") "a"
            createDirectory (d </> "dir")
            writeFile (d </> "dir" </> "a") "longer old content"
            cp id (d </> "a") (d </> "dir")
            (d </> "dir" </> "a") `shouldContain'` "a"

        it "keeps a file of the same name with OverwriteNever"
            $ withTempDir $ \d -> do
            writeFile (d </> "a") "a"
            createDirectory (d </> "dir")
            writeFile (d </> "dir" </> "a") "old"
            cp (overwrite OverwriteNever) (d </> "a") (d </> "dir")
            (d </> "dir" </> "a") `shouldContain'` "old"

        it "fails to replace a directory in the destination with a file"
            $ withTempDir $ \d -> do
            writeFile (d </> "a") "a"
            createDirectory (d </> "dir")
            createDirectory (d </> "dir" </> "a")
            shouldFail (cp id (d </> "a") (d </> "dir"))
            doesDirectoryExist (d </> "dir" </> "a") `shouldReturn` True

        it "fails if the source is already in the directory"
            $ withTempDir $ \d -> do
            writeFile (d </> "a") "a"
            shouldFail (cp id (d </> "a") d)
            (d </> "a") `shouldContain'` "a"

        it "hard links into the directory with HardLink"
            $ withTempDir $ \d -> do
            writeFile (d </> "a") "a"
            createDirectory (d </> "dir")
            cp (cpMethod HardLink) (d </> "a") (d </> "dir")
            appendFile (d </> "a") "b"
            (d </> "dir" </> "a") `shouldContain'` "ab"

        it "works with relative paths" $ withTempDir $ \d -> do
            writeFile (d </> "a") "a"
            createDirectory (d </> "dir")
            withCurrentDirectory d $ cp id "a" "dir"
            (d </> "dir" </> "a") `shouldContain'` "a"

        it "works with non-ASCII names" $ withTempDir $ \d -> do
            writeFile (d </> nonAscii) "a"
            createDirectory (d </> "dir")
            cp id (d </> nonAscii) (d </> "dir")
            (d </> "dir" </> nonAscii) `shouldContain'` "a"

    describe "cp (intoDirectory False)" $ do

        it "fails on an existing directory" $ withTempDir $ \d -> do
            writeFile (d </> "a") "a"
            createDirectory (d </> "dir")
            shouldFail (cp (intoDirectory False) (d </> "a") (d </> "dir"))
            shouldNotExist (d </> "dir" </> "a")

        it "copies a file to a new name" $ withTempDir $ \d -> do
            writeFile (d </> "a") "a"
            cp (intoDirectory False) (d </> "a") (d </> "b")
            (d </> "b") `shouldContain'` "a"

    describe "cp (overwrite OverwriteNever)" $ do

        it "keeps an existing destination" $ withTempDir $ \d -> do
            writeFile (d </> "a") "a"
            writeFile (d </> "b") "b"
            cp (overwrite OverwriteNever) (d </> "a") (d </> "b")
            (d </> "b") `shouldContain'` "b"

        it "copies to a missing destination" $ withTempDir $ \d -> do
            writeFile (d </> "a") "a"
            cp (overwrite OverwriteNever) (d </> "a") (d </> "b")
            (d </> "b") `shouldContain'` "a"

    describe "cp (overwrite OverwriteOnly)" $ do

        it "does nothing if the destination is missing"
            $ withTempDir $ \d -> do
            writeFile (d </> "a") "a"
            cp (overwrite OverwriteOnly) (d </> "a") (d </> "b")
            shouldNotExist (d </> "b")

        it "replaces an existing destination" $ withTempDir $ \d -> do
            writeFile (d </> "a") "a"
            writeFile (d </> "b") "b"
            cp (overwrite OverwriteOnly) (d </> "a") (d </> "b")
            (d </> "b") `shouldContain'` "a"

    describe "cp (overwrite OverwriteUpdate)" $ do

        it "replaces an older destination" $ withTempDir $ \d -> do
            writeFile (d </> "a") "a"
            writeFile (d </> "b") "b"
            setAge (d </> "b") (-100)
            cp (overwrite OverwriteUpdate) (d </> "a") (d </> "b")
            (d </> "b") `shouldContain'` "a"

        it "keeps a newer destination" $ withTempDir $ \d -> do
            writeFile (d </> "a") "a"
            writeFile (d </> "b") "b"
            setAge (d </> "a") (-100)
            cp (overwrite OverwriteUpdate) (d </> "a") (d </> "b")
            (d </> "b") `shouldContain'` "b"

        it "copies to a missing destination" $ withTempDir $ \d -> do
            writeFile (d </> "a") "a"
            cp (overwrite OverwriteUpdate) (d </> "a") (d </> "b")
            (d </> "b") `shouldContain'` "a"

        it "compares with the target of a symlink at the destination"
            $ withTempDir $ \d -> do
            writeFile (d </> "a") "a"
            writeFile (d </> "target") "old"
            setAge (d </> "target") (-200)
            setAge (d </> "a") (-100)
            createFileLink "target" (d </> "b")
            cp (overwrite OverwriteUpdate) (d </> "a") (d </> "b")
            pathIsSymbolicLink (d </> "b") `shouldReturn` True
            (d </> "target") `shouldContain'` "a"

        it "compares with the target of a symlink at the source"
            $ withTempDir $ \d -> do
            writeFile (d </> "target") "a"
            setAge (d </> "target") (-200)
            createFileLink "target" (d </> "a")
            writeFile (d </> "b") "b"
            setAge (d </> "b") (-100)
            cp (overwrite OverwriteUpdate) (d </> "a") (d </> "b")
            (d </> "b") `shouldContain'` "b"

    describe "cp (cpMethod HardLink)" $ do

        it "creates a hard link" $ withTempDir $ \d -> do
            writeFile (d </> "a") "a"
            cp (cpMethod HardLink) (d </> "a") (d </> "b")
            appendFile (d </> "a") "b"
            (d </> "b") `shouldContain'` "ab"

        it "fails if the destination exists" $ withTempDir $ \d -> do
            writeFile (d </> "a") "a"
            writeFile (d </> "b") "b"
            shouldFail (cp (cpMethod HardLink) (d </> "a") (d </> "b"))
            (d </> "b") `shouldContain'` "b"

        it "keeps an existing destination with OverwriteNever"
            $ withTempDir $ \d -> do
            writeFile (d </> "a") "a"
            writeFile (d </> "b") "b"
            cp (cpMethod HardLink . overwrite OverwriteNever)
                (d </> "a") (d </> "b")
            (d </> "b") `shouldContain'` "b"
