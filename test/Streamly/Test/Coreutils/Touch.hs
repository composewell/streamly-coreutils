{-# LANGUAGE CPP #-}
-- |
--
-- Module      : Streamly.Test.Coreutils.Touch
-- Copyright   : (c) 2026 Composewell Technologies
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Tests for 'Streamly.Coreutils.Touch'. Behavior mirrors GNU @touch@ unless
-- noted otherwise in the module documentation.

module Streamly.Test.Coreutils.Touch (main) where

import Control.Exception (bracket, try, SomeException)
import Control.Monad (void)
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import System.Directory
    ( createFileLink
    , doesPathExist
    , getAccessTime
    , getModificationTime
    , getTemporaryDirectory
    , removePathForcibly
    , setAccessTime
    , setModificationTime
    )
import System.FilePath ((</>))
import System.IO.Temp (createTempDirectory)
-- POSIXTime is needed only on non-Windows; merging the two imports would
-- make it a redundant import on Windows.
{- HLINT ignore "Use fewer imports" -}
#if !defined(CABAL_OS_WINDOWS)
import Data.Time.Clock.POSIX (POSIXTime)
import System.Posix.Files
    ( accessTimeHiRes
    , getSymbolicLinkStatus
    , modificationTimeHiRes
    , setSymbolicLinkTimesHiRes
    )
#endif
import Test.Hspec

import Streamly.Coreutils.Touch hiding (touch)
import qualified Streamly.Coreutils.Touch as Touch
import qualified Streamly.FileSystem.Path as Path

-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

touch :: (TouchOptions -> TouchOptions) -> FilePath -> IO ()
touch f path = Path.fromString path >>= Touch.touch f

-- | Run a test inside a fresh temporary directory, cleaning up afterwards.
withTempDir :: (FilePath -> IO ()) -> IO ()
withTempDir action = do
    tmpRoot <- getTemporaryDirectory
    bracket
        (createTempDirectory tmpRoot "touch-test-")
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

-- Strict read, so that the file is closed before it is removed.
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

-- | A time in the past, 2001-09-09, used to see whether touch changed the
-- times of a file.
oldSeconds :: Integer
oldSeconds = 1000000000

oldTime :: UTCTime
oldTime = posixSecondsToUTCTime (fromInteger oldSeconds)

-- | Create a file with the given content and set its access and modification
-- times to 'oldTime'.
writeOldFile :: FilePath -> String -> IO ()
writeOldFile path content = do
    writeFile path content
    setAccessTime path oldTime
    setModificationTime path oldTime

-- | Access and modification times of a file, following symlinks.
fileTimes :: FilePath -> IO (UTCTime, UTCTime)
fileTimes path = (,) <$> getAccessTime path <*> getModificationTime path

shouldBeOld :: FilePath -> IO ()
shouldBeOld path = fileTimes path `shouldReturn` (oldTime, oldTime)

shouldBeTouched :: FilePath -> IO ()
shouldBeTouched path = do
    (atime, mtime) <- fileTimes path
    atime `shouldSatisfy` (> oldTime)
    mtime `shouldSatisfy` (> oldTime)

#if !defined(CABAL_OS_WINDOWS)
oldPosixTime :: POSIXTime
oldPosixTime = fromInteger oldSeconds

-- | Create a symlink and set the times of the link itself to 'oldTime'.
createOldLink :: FilePath -> FilePath -> IO ()
createOldLink target link = do
    createFileLink target link
    setSymbolicLinkTimesHiRes link oldPosixTime oldPosixTime

-- | Access and modification times of a symlink itself.
linkTimes :: FilePath -> IO (POSIXTime, POSIXTime)
linkTimes path = do
    st <- getSymbolicLinkStatus path
    return (accessTimeHiRes st, modificationTimeHiRes st)

linkShouldBeTouched :: FilePath -> IO ()
linkShouldBeTouched path = do
    (atime, mtime) <- linkTimes path
    atime `shouldSatisfy` (> oldPosixTime)
    mtime `shouldSatisfy` (> oldPosixTime)
#endif

-------------------------------------------------------------------------------
-- Tests
-------------------------------------------------------------------------------

main :: IO ()
main = hspec $ do
    describe "touch id" $ do
        it "creates a missing file, empty" $ withTempDir $ \d -> do
            touch id (d </> "a")
            (d </> "a") `shouldContain'` ""

        it "updates the times of an existing file" $ withTempDir $ \d -> do
            writeOldFile (d </> "a") "content"
            touch id (d </> "a")
            shouldBeTouched (d </> "a")
            (d </> "a") `shouldContain'` "content"

        it "updates the target of a symlink" $ withTempDir $ \d -> do
            writeOldFile (d </> "target") "content"
            createFileLink "target" (d </> "link")
            touch id (d </> "link")
            shouldBeTouched (d </> "target")
            (d </> "target") `shouldContain'` "content"

        it "creates the target of a dangling symlink" $ withTempDir $ \d -> do
            createFileLink "target" (d </> "link")
            touch id (d </> "link")
            (d </> "target") `shouldContain'` ""

        it "fails if the parent directory is missing" $ withTempDir $ \d -> do
            shouldFail $ touch id (d </> "nodir" </> "a")
            shouldNotExist (d </> "nodir")

    describe "touch (create False)" $ do
        it "does nothing on a missing file" $ withTempDir $ \d -> do
            touch (create False) (d </> "a")
            shouldNotExist (d </> "a")

        it "does nothing if the parent directory is missing"
            $ withTempDir $ \d -> do
            touch (create False) (d </> "nodir" </> "a")
            shouldNotExist (d </> "nodir")

        it "updates the times of an existing file" $ withTempDir $ \d -> do
            writeOldFile (d </> "a") "content"
            touch (create False) (d </> "a")
            shouldBeTouched (d </> "a")
            (d </> "a") `shouldContain'` "content"

        it "updates the target of a symlink" $ withTempDir $ \d -> do
            writeOldFile (d </> "target") "content"
            createFileLink "target" (d </> "link")
            touch (create False) (d </> "link")
            shouldBeTouched (d </> "target")

        it "does not create the target of a dangling symlink"
            $ withTempDir $ \d -> do
            createFileLink "target" (d </> "link")
            touch (create False) (d </> "link")
            shouldNotExist (d </> "target")

#if !defined(CABAL_OS_WINDOWS)
    describe "touch (followSymlinks False)" $ do
        it "updates the symlink, not its target" $ withTempDir $ \d -> do
            writeOldFile (d </> "target") "content"
            createOldLink "target" (d </> "link")
            touch (followSymlinks False) (d </> "link")
            linkShouldBeTouched (d </> "link")
            shouldBeOld (d </> "target")

        it "updates a dangling symlink, without creating the target"
            $ withTempDir $ \d -> do
            createOldLink "target" (d </> "link")
            touch (followSymlinks False) (d </> "link")
            linkShouldBeTouched (d </> "link")
            shouldNotExist (d </> "target")

        it "updates the times of an existing file" $ withTempDir $ \d -> do
            writeOldFile (d </> "a") "content"
            touch (followSymlinks False) (d </> "a")
            shouldBeTouched (d </> "a")

        it "fails on a missing file" $ withTempDir $ \d -> do
            shouldFail $ touch (followSymlinks False) (d </> "a")
            shouldNotExist (d </> "a")

    describe "touch (create False . followSymlinks False)" $ do
        it "does nothing on a missing file" $ withTempDir $ \d -> do
            touch (create False . followSymlinks False) (d </> "a")
            shouldNotExist (d </> "a")

        it "updates the symlink, not its target" $ withTempDir $ \d -> do
            writeOldFile (d </> "target") "content"
            createOldLink "target" (d </> "link")
            touch (create False . followSymlinks False) (d </> "link")
            linkShouldBeTouched (d </> "link")
            shouldBeOld (d </> "target")
#else
    describe "touch (followSymlinks False)" $
        it "is not supported on Windows" $ withTempDir $ \d -> do
            writeOldFile (d </> "a") "content"
            shouldFail $ touch (followSymlinks False) (d </> "a")
            shouldBeOld (d </> "a")
#endif
