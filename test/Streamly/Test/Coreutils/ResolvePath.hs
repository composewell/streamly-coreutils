{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
--
-- Module      : Streamly.Test.Coreutils.ResolvePath
-- Copyright   : (c) 2026 Composewell Technologies
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Tests for 'Streamly.Coreutils.ResolvePath'. The expected results are those
-- of GNU @realpath@ with the corresponding options.

module Streamly.Test.Coreutils.ResolvePath (main) where

import Control.Exception (SomeException, try)
import System.Directory
    ( canonicalizePath
    , createDirectoryIfMissing
    , createDirectoryLink
    , createFileLink
    )
import System.FilePath ((</>))
import System.IO.Error (isDoesNotExistError)
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

import Streamly.Coreutils.ResolvePath

import qualified Streamly.FileSystem.Path as Path

-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

-- | A canonical temporary directory holding:
--
-- > x/y/       directory
-- > x/f        file
-- > ly ->      x/y
-- > dangling -> nowhere
withTree :: (FilePath -> IO a) -> IO a
withTree action =
    withSystemTempDirectory "resolvepath-test-" $ \tmp -> do
        d <- canonicalizePath tmp
        createDirectoryIfMissing True (d </> "x" </> "y")
        writeFile (d </> "x" </> "f") ""
        createDirectoryLink ("x" </> "y") (d </> "ly")
        createFileLink "nowhere" (d </> "dangling")
        action d

resolve :: (ResolvePathOptions -> ResolvePathOptions) -> FilePath -> IO FilePath
resolve f p = Path.toString <$> (Path.fromString p >>= resolvePath f)

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

-- | A 'knownBug' on POSIX only. Win32 removes @..@ lexically, so the bug
-- does not occur on Windows.
posixBug :: String -> Expectation -> Expectation
#if !defined(CABAL_OS_WINDOWS)
posixBug = knownBug
#else
posixBug _ action = action
#endif

-------------------------------------------------------------------------------
-- Tests
-------------------------------------------------------------------------------

main :: IO ()
main = hspec $ do
    describe "default (realpath)" $ do
        it "returns an existing file unchanged" $ withTree $ \d ->
            resolve id (d </> "x" </> "f") `shouldReturn` d </> "x" </> "f"

        it "resolves a symlink" $ withTree $ \d ->
            resolve id (d </> "ly") `shouldReturn` d </> "x" </> "y"

#if !defined(CABAL_OS_WINDOWS)
        it "resolves .. after the symlink target" $ withTree $ \d ->
            resolve id (d </> "ly" </> "..") `shouldReturn` d </> "x"
#else
        -- Win32 removes .. lexically before following symlinks.
        it "resolves .. before the symlink" $ withTree $ \d ->
            resolve id (d </> "ly" </> "..") `shouldReturn` d
#endif

        it "removes . and repeated separators" $ withTree $ \d ->
            resolve id (d </> "." </> "x" ++ "//y")
                `shouldReturn` d </> "x" </> "y"

        it "accepts a missing leaf" $ withTree $ \d ->
            resolve id (d </> "x" </> "missing")
                `shouldReturn` d </> "x" </> "missing"

        it "fails on a missing parent" $ withTree $ \d ->
            resolve id (d </> "missing" </> "a")
                `shouldThrow` isDoesNotExistError

    describe "existenceCheck RequirePath (realpath -e)" $ do
        it "returns an existing path" $ withTree $ \d ->
            resolve (existenceCheck RequirePath) (d </> "ly")
                `shouldReturn` d </> "x" </> "y"

        it "fails on a missing leaf" $ withTree $ \d ->
            resolve (existenceCheck RequirePath) (d </> "x" </> "missing")
                `shouldThrow` isDoesNotExistError

        it "fails on a dangling symlink" $ withTree $ \d ->
            resolve (existenceCheck RequirePath) (d </> "dangling")
                `shouldThrow` isDoesNotExistError

    describe "existenceCheck RequireParents (realpath -E)" $
        it "behaves like the default" $ withTree $ \d -> do
            resolve (existenceCheck RequireParents) (d </> "x" </> "missing")
                `shouldReturn` d </> "x" </> "missing"
            resolve (existenceCheck RequireParents) (d </> "missing" </> "a")
                `shouldThrow` isDoesNotExistError

    describe "existenceCheck RequireNone (realpath -m)" $ do
        it "accepts a missing parent" $ withTree $ \d ->
            resolve (existenceCheck RequireNone) (d </> "missing" </> "a")
                `shouldReturn` d </> "missing" </> "a"

        it "resolves .. after a missing component" $ withTree $ \d ->
            posixBug "canonicalizePath keeps .. after a missing component" $
                resolve (existenceCheck RequireNone)
                    (d </> "missing" </> "a" </> ".." </> "b")
                    `shouldReturn` d </> "missing" </> "b"

        it "resolves the existing prefix" $ withTree $ \d ->
            resolve (existenceCheck RequireNone) (d </> "ly" </> "missing")
                `shouldReturn` d </> "x" </> "y" </> "missing"

    describe "resolutionMode ParentThenResolve (realpath -L)" $ do
        it "resolves .. before the symlink" $ withTree $ \d ->
            resolve (resolutionMode ParentThenResolve) (d </> "ly" </> "..")
                `shouldReturn` d

        it "resolves the symlinks left after .." $ withTree $ \d ->
            resolve (resolutionMode ParentThenResolve)
                (d </> "x" </> ".." </> "ly")
                `shouldReturn` d </> "x" </> "y"

    describe "resolutionMode NoSymlinkResolution (realpath -s)" $ do
        it "keeps a symlink" $ withTree $ \d ->
            resolve (resolutionMode NoSymlinkResolution) (d </> "ly")
                `shouldReturn` d </> "ly"

        it "removes the component before .." $ withTree $ \d ->
            resolve
                (resolutionMode NoSymlinkResolution . existenceCheck RequireNone)
                (d </> "ly" </> ".." </> "x" </> "f")
                `shouldReturn` d </> "x" </> "f"

        it "checks existence on the lexical result" $ withTree $ \d ->
            posixBug "existence is checked on the physical path" $
                resolve
                    (resolutionMode NoSymlinkResolution . existenceCheck RequirePath)
                    (d </> "ly" </> ".." </> "x" </> "f")
                    `shouldReturn` d </> "x" </> "f"

        it "removes . and repeated separators" $ withTree $ \d ->
            resolve
                (resolutionMode NoSymlinkResolution . existenceCheck RequireNone)
                (d </> "." </> "x" ++ "//y")
                `shouldReturn` d </> "x" </> "y"

    describe "relativeTo (realpath --relative-to)" $ do
        it "gives a path below the base" $ withTree $ \d -> do
            base <- Path.fromString (d </> "x")
            resolve (relativeTo base) (d </> "x" </> "y") `shouldReturn` "y"

        it "gives . for the base itself" $ withTree $ \d -> do
            base <- Path.fromString d
            resolve (relativeTo base) d `shouldReturn` "."

        it "resolves symlinks in the base" $ withTree $ \d -> do
            base <- Path.fromString (d </> "ly")
            resolve (relativeTo base) (d </> "x" </> "y") `shouldReturn` "."

        it "gives .. for the parent of the base" $ withTree $ \d ->
            knownBug "makeRelative does not produce .. segments" $ do
                base <- Path.fromString (d </> "x" </> "y")
                resolve (relativeTo base) (d </> "x") `shouldReturn` ".."

        it "gives a path through a common ancestor" $ withTree $ \d ->
            knownBug "makeRelative does not produce .. segments" $ do
                base <- Path.fromString (d </> "x" </> "y")
                resolve (relativeTo base) (d </> "x" </> "f")
                    `shouldReturn` ".." </> "f"

    describe "relativeWithin (realpath --relative-base)" $ do
        it "relativizes a path inside the boundary" $ withTree $ \d -> do
            boundary <- Path.fromString (d </> "x")
            resolve (relativeWithin boundary) (d </> "x" </> "f")
                `shouldReturn` "f"

        it "keeps a path outside the boundary absolute" $ withTree $ \d -> do
            boundary <- Path.fromString (d </> "x")
            resolve (relativeWithin boundary) d `shouldReturn` d

        it "does not treat a name prefix as containment" $ withTree $ \d -> do
            createDirectoryIfMissing True (d </> "xa")
            boundary <- Path.fromString (d </> "x")
            resolve (relativeWithin boundary) (d </> "xa")
                `shouldReturn` d </> "xa"

        it "relativizes against relativeTo inside the boundary"
            $ withTree $ \d ->
                knownBug "makeRelative does not produce .. segments" $ do
                    boundary <- Path.fromString (d </> "x")
                    base <- Path.fromString (d </> "x" </> "y")
                    resolve (relativeTo base . relativeWithin boundary)
                        (d </> "x" </> "f")
                        `shouldReturn` ".." </> "f"

        it "keeps the path absolute when the base is outside the boundary"
            $ withTree $ \d ->
                knownBug "the base is not checked against the boundary" $ do
                    boundary <- Path.fromString (d </> "x")
                    base <- Path.fromString d
                    resolve (relativeTo base . relativeWithin boundary)
                        (d </> "x" </> "f")
                        `shouldReturn` d </> "x" </> "f"
