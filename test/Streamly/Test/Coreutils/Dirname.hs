{-# LANGUAGE CPP #-}
-- |
--
-- Module      : Streamly.Test.Coreutils.Dirname
-- Copyright   : (c) 2026 Composewell Technologies
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Tests for 'Streamly.Coreutils.Dirname'. The expected outputs are those of
-- GNU @dirname@.

module Streamly.Test.Coreutils.Dirname (main) where

import Control.Monad (forM_)
import Data.List (isPrefixOf)
import System.FilePath (isPathSeparator, pathSeparator)
import Test.Hspec
import Test.QuickCheck
    (Gen, Property, elements, forAll, listOf, listOf1, (==>))
import Test.QuickCheck.Monadic (assert, monadicIO, run)

import qualified Streamly.Coreutils.Dirname as Dirname
import qualified Streamly.FileSystem.Path as Path

-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

dirname :: String -> IO String
dirname p = Path.toString <$> (Path.fromString p >>= Dirname.dirname)

dropTrailingSeps :: String -> String
dropTrailingSeps = reverse . dropWhile isPathSeparator . reverse

-- | Check a table of (input, GNU dirname output) pairs.
checkCases :: [(String, String)] -> Spec
checkCases cases =
    forM_ cases $ \(input, expected) ->
        it (show input ++ " -> " ++ show expected) $
            dirname input `shouldReturn` expected

-- | Non-empty relative paths over a small alphabet, so that separators,
-- dot components and repeated separators are common.
genPath :: Gen String
genPath = listOf1 (elements "ab./")

-- | A single file name component, no separators.
genName :: Gen String
genName = listOf1 (elements "ab.")

-------------------------------------------------------------------------------
-- Properties
-------------------------------------------------------------------------------

-- | Trailing separators do not change the result.
propTrailingSeps :: Property
propTrailingSeps =
    forAll genPath $ \p ->
    forAll (listOf1 (elements "/")) $ \seps ->
        monadicIO $ do
            r1 <- run $ dirname p
            r2 <- run $ dirname (p ++ seps)
            assert (r1 == r2)

-- | Appending a component to a path that has a name in it gives back the
-- path without its trailing separators.
propAppendName :: Property
propAppendName =
    forAll genPath $ \p ->
    forAll (listOf1 (elements "/")) $ \seps ->
    forAll genName $ \n ->
        not (all isPathSeparator p) ==> monadicIO $ do
            r <- run $ dirname (p ++ seps ++ n)
            assert (r == dropTrailingSeps p)

-- | The result is ".", the root, or a prefix of the input that does not end
-- in a separator.
propShape :: Property
propShape =
    forAll ((++) <$> listOf (elements "/") <*> genPath) $ \p ->
        monadicIO $ do
            r <- run $ dirname p
            assert
                (  r == "."
                || r == [pathSeparator]
                || (r `isPrefixOf` p && not (isPathSeparator (last r)))
                )

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

main :: IO ()
main = hspec $ do
    describe "relative paths" $ checkCases
        [ ("a", ".")
        , ("a/b", "a")
        , ("a/b/c", "a/b")
        , ("a//b", "a")
        , ("a//b//c", "a//b")
        ]

    describe "trailing separators" $ checkCases
        [ ("a/", ".")
        , ("a//", ".")
        , ("a/b/", "a")
        , ("a/b//", "a")
        , ("a/b///", "a")
        , ("a//b//", "a")
        ]

    describe "dot components" $ checkCases
        [ (".", ".")
        , ("..", ".")
        , ("./", ".")
        , ("./a", ".")
        , ("../a", "..")
        , ("a/.", "a")
        , ("a/..", "a")
        , ("a/./b", "a/.")
        , ("a/../b", "a/..")
        , (".a", ".")
        , ("a/.b", "a")
        ]

#if !defined(CABAL_OS_WINDOWS)
    describe "absolute paths" $ checkCases
        [ ("/a", "/")
        , ("/a/", "/")
        , ("/a/b", "/a")
        , ("/a/b/", "/a")
        , ("/a/b//", "/a")
        , ("/a//b", "/a")
        , ("/a/b/c", "/a/b")
        ]

    describe "root" $ checkCases
        [ ("/", "/")
        , ("//", "/")
        , ("///", "/")
        , ("//a", "/")
        , ("///a", "/")
        , ("//a/", "/")
        ]

    describe "repeated leading separators kept before a directory" $
        checkCases
            [ ("//a//b", "//a")
            , ("////a///b///", "////a")
            , ("//a/b/c", "//a/b")
            ]
#else
    describe "drives" $ checkCases
        [ ("C:\\", "C:\\")
        , ("C:\\a", "C:\\")
        , ("C:\\a\\", "C:\\")
        , ("C:\\a\\b", "C:\\a")
        , ("C:\\a\\b\\", "C:\\a")
        , ("C:\\a\\\\b\\\\", "C:\\a")
        , ("C:a", "C:")
        , ("C:a\\b", "C:a")
        ]

    describe "backslash separators" $ checkCases
        [ ("a\\b", "a")
        , ("a\\b\\", "a")
        , ("a\\b\\\\", "a")
        , ("a\\", ".")
        ]

    describe "current drive root" $ checkCases
        [ ("\\", "\\")
        , ("\\a", "\\")
        , ("\\a\\", "\\")
        , ("\\a\\b", "\\a")
        ]

    describe "share roots" $ checkCases
        [ ("\\\\srv\\share", "\\\\srv\\share")
        , ("\\\\srv\\share\\", "\\\\srv\\share\\")
        , ("\\\\srv\\share\\a", "\\\\srv\\share\\")
        , ("\\\\srv\\share\\a\\", "\\\\srv\\share\\")
        , ("\\\\srv\\share\\a\\b", "\\\\srv\\share\\a")
        , ("\\\\?\\UNC\\srv\\share\\", "\\\\?\\UNC\\srv\\share\\")
        , ("\\\\?\\UNC\\srv\\share\\a", "\\\\?\\UNC\\srv\\share\\")
        , ("\\\\?\\UNC\\srv\\share\\a\\b", "\\\\?\\UNC\\srv\\share\\a")
        ]

    describe "verbatim drive paths" $ checkCases
        [ ("\\\\?\\C:\\", "\\\\?\\C:\\")
        , ("\\\\?\\C:\\a", "\\\\?\\C:\\")
        , ("\\\\?\\C:\\a\\b", "\\\\?\\C:\\a")
        ]
#endif

    describe "properties" $ do
        it "ignores trailing separators" propTrailingSeps
        it "strips an appended name" propAppendName
        it "returns \".\", the root or a prefix of the input" propShape
