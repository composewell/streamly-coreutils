-- |
--
-- Module      : Streamly.Test.Coreutils.Ln
-- Copyright   : (c) 2026 Composewell Technologies
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Tests for 'Streamly.Coreutils.Ln'. The expected behavior is that of GNU
-- @ln@.

module Streamly.Test.Coreutils.Ln (main) where

import System.Directory
    ( createDirectory
    , createDirectoryLink
    , createFileLink
    , doesDirectoryExist
    , getSymbolicLinkTarget
    , pathIsSymbolicLink
    )
import GHC.IO.Exception (IOErrorType(InvalidArgument), IOException(..))
import System.FilePath ((</>))
import System.IO.Error (isAlreadyExistsError)
import System.IO.Temp (withSystemTempDirectory)
import System.PosixCompat.Files (createLink, fileID, getFileStatus)
import Test.Hspec

import Streamly.Coreutils.Ln (force, ln, symbolic)

import qualified Streamly.FileSystem.Path as Path

-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

withTempDir :: (FilePath -> IO a) -> IO a
withTempDir = withSystemTempDirectory "ln-test-"

-- | @lnS isForce isSymbolic src dst@
lnS :: Bool -> Bool -> FilePath -> FilePath -> IO ()
lnS f s src dst = do
    srcP <- Path.fromString src
    dstP <- Path.fromString dst
    ln (force f . symbolic s) srcP dstP

sameInode :: FilePath -> FilePath -> IO Bool
sameInode a b = do
    sa <- getFileStatus a
    sb <- getFileStatus b
    return (fileID sa == fileID sb)

isInvalidArgument :: IOException -> Bool
isInvalidArgument e = ioe_type e == InvalidArgument

-------------------------------------------------------------------------------
-- Tests
-------------------------------------------------------------------------------

main :: IO ()
main = hspec $ do
    describe "ln (hard link)" $ do
        it "creates a hard link" $ withTempDir $ \d -> do
            writeFile (d </> "a") "x"
            lnS False False (d </> "a") (d </> "b")
            sameInode (d </> "a") (d </> "b") `shouldReturn` True
            readFile (d </> "b") `shouldReturn` "x"

        it "uses the defaults with id" $ withTempDir $ \d -> do
            writeFile (d </> "a") "x"
            src <- Path.fromString (d </> "a")
            dst <- Path.fromString (d </> "b")
            ln id src dst
            sameInode (d </> "a") (d </> "b") `shouldReturn` True

        it "fails on a missing source" $ withTempDir $ \d ->
            lnS False False (d </> "missing") (d </> "b")
                `shouldThrow` anyIOException

        it "fails on an existing destination" $ withTempDir $ \d -> do
            writeFile (d </> "a") "x"
            writeFile (d </> "b") "y"
            lnS False False (d </> "a") (d </> "b")
                `shouldThrow` isAlreadyExistsError
            readFile (d </> "b") `shouldReturn` "y"

        it "fails on a dangling symlink at the destination"
            $ withTempDir $ \d -> do
                writeFile (d </> "a") "x"
                createFileLink "nowhere" (d </> "b")
                lnS False False (d </> "a") (d </> "b")
                    `shouldThrow` isAlreadyExistsError

    describe "ln (force True)" $ do
        it "replaces an existing destination" $ withTempDir $ \d -> do
            writeFile (d </> "a") "x"
            writeFile (d </> "b") "y"
            lnS True False (d </> "a") (d </> "b")
            sameInode (d </> "a") (d </> "b") `shouldReturn` True

        it "replaces a hard link to the source" $ withTempDir $ \d -> do
            writeFile (d </> "a") "x"
            createLink (d </> "a") (d </> "b")
            lnS True False (d </> "a") (d </> "b")
            sameInode (d </> "a") (d </> "b") `shouldReturn` True
            readFile (d </> "a") `shouldReturn` "x"

        it "replaces a dangling symlink" $ withTempDir $ \d -> do
            writeFile (d </> "a") "x"
            createFileLink "nowhere" (d </> "b")
            lnS True False (d </> "a") (d </> "b")
            sameInode (d </> "a") (d </> "b") `shouldReturn` True

        it "fails when the source and destination are the same file"
            $ withTempDir $ \d -> do
                writeFile (d </> "a") "x"
                lnS True False (d </> "a") (d </> "a")
                    `shouldThrow` isInvalidArgument
                readFile (d </> "a") `shouldReturn` "x"

        it "fails on the same file reached through another path"
            $ withTempDir $ \d -> do
                writeFile (d </> "a") "x"
                lnS True False (d </> "a") (d </> "." </> "a")
                    `shouldThrow` isInvalidArgument
                readFile (d </> "a") `shouldReturn` "x"

    describe "ln (symbolic True)" $ do
        it "creates a symlink with the source as the target"
            $ withTempDir $ \d -> do
                writeFile (d </> "a") "x"
                lnS False True "a" (d </> "b")
                pathIsSymbolicLink (d </> "b") `shouldReturn` True
                getSymbolicLinkTarget (d </> "b") `shouldReturn` "a"
                readFile (d </> "b") `shouldReturn` "x"

        it "creates a dangling symlink to a missing source"
            $ withTempDir $ \d -> do
                lnS False True "missing" (d </> "b")
                getSymbolicLinkTarget (d </> "b") `shouldReturn` "missing"

        it "fails on an existing destination" $ withTempDir $ \d -> do
            writeFile (d </> "b") "y"
            lnS False True "a" (d </> "b") `shouldThrow` isAlreadyExistsError

        it "replaces a symlink with force" $ withTempDir $ \d -> do
            createFileLink "a" (d </> "b")
            lnS True True "c" (d </> "b")
            getSymbolicLinkTarget (d </> "b") `shouldReturn` "c"

        it "replaces a symlink to the source with force"
            $ withTempDir $ \d -> do
                writeFile (d </> "a") "x"
                createFileLink "a" (d </> "b")
                lnS True True (d </> "a") (d </> "b")
                getSymbolicLinkTarget (d </> "b") `shouldReturn` d </> "a"
                readFile (d </> "a") `shouldReturn` "x"

        -- Like ln -sfn: the link is replaced, no link is created inside the
        -- directory it points to.
        it "replaces a symlink to a directory with force"
            $ withTempDir $ \d -> do
                createDirectory (d </> "dir")
                createDirectoryLink "dir" (d </> "b")
                lnS True True "c" (d </> "b")
                getSymbolicLinkTarget (d </> "b") `shouldReturn` "c"
                doesDirectoryExist (d </> "dir") `shouldReturn` True
