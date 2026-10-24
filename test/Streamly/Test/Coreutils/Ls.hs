{-# LANGUAGE CPP #-}
-- |
--
-- Module      : Streamly.Test.Coreutils.Ls
-- Copyright   : (c) 2026 Composewell Technologies
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Tests for 'Streamly.Coreutils.Ls'. The order of the entries within a
-- directory is the order the operating system returns them in, so the
-- traversal tests check the structure of the output, not a fixed list.

module Streamly.Test.Coreutils.Ls (main) where

import Control.Monad (forM_)
import Data.Either (lefts, rights)
import Data.List (isPrefixOf, sort)
import System.Directory
    ( createDirectoryIfMissing
#if !defined(CABAL_OS_WINDOWS)
    , createDirectoryLink
#endif
    )
import System.FilePath (splitDirectories, (</>))
import System.IO.Error (ioeGetLocation, isDoesNotExistError)
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

import Streamly.Coreutils.Ls
    (LsOptions, Traversal(..), ls, recursive, traversal)

import qualified Streamly.Data.Stream as Stream
import qualified Streamly.FileSystem.Path as Path

-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

withTempDir :: (FilePath -> IO a) -> IO a
withTempDir = withSystemTempDirectory "ls-test-"

-- | Directories of the fixture tree, relative to the root.
fixtureDirs :: [FilePath]
fixtureDirs =
    [ "a", "a" </> "a1", "a" </> "a2", "b", "b" </> "b1"
    , "b" </> "b1" </> "b11", "c"
    ]

-- | Files of the fixture tree, relative to the root.
fixtureFiles :: [FilePath]
fixtureFiles =
    [ "f1", "f2", "a" </> "f", "a" </> "a1" </> "f", "b" </> "b1" </> "f" ]

makeFixture :: FilePath -> IO ()
makeFixture root = do
    forM_ fixtureDirs $ \d -> createDirectoryIfMissing True (root </> d)
    forM_ fixtureFiles $ \f -> writeFile (root </> f) ""

lsS :: (LsOptions -> LsOptions) -> FilePath -> IO [Either FilePath FilePath]
lsS f dir = do
    p <- Path.fromString dir
    xs <- Stream.toList (ls f p)
    pure (map (either (Left . Path.toString) (Right . Path.toString)) xs)

paths :: [Either FilePath FilePath] -> [FilePath]
paths = map (either id id)

depth :: FilePath -> Int
depth = length . splitDirectories

-- | A proper descendant of a directory.
isUnder :: FilePath -> FilePath -> Bool
isUnder d p =
    let ds = splitDirectories d
        ps = splitDirectories p
     in ds `isPrefixOf` ps && length ps > length ds

isChildOf :: FilePath -> FilePath -> Bool
isChildOf d p = isUnder d p && depth p == depth d + 1

-- | All elements that satisfy the predicate are at the head of the list.
prefix :: (a -> Bool) -> [a] -> Bool
prefix p = not . any p . dropWhile p

-- | All elements that satisfy the predicate are adjacent in the list.
contiguous :: (a -> Bool) -> [a] -> Bool
contiguous p = prefix p . dropWhile (not . p)

-- | Each directory in the list with the entries that follow it.
withRest :: [Either FilePath FilePath] -> [(FilePath, [FilePath])]
withRest xs = [(d, paths rest) | (Left d, rest) <- zip xs (tails' xs)]

    where

    tails' ys = drop 1 (scanr (:) [] ys)

-------------------------------------------------------------------------------
-- Tests
-------------------------------------------------------------------------------

-- | Every traversal lists the root first and then each entry of the tree
-- once, directories as Left and files as Right.
listsTree :: (LsOptions -> LsOptions) -> Spec
listsTree f =
    it "lists the root, then every entry once" $ withTempDir $ \root -> do
        makeFixture root
        xs <- lsS f root
        take 1 xs `shouldBe` [Left root]
        sort (lefts (drop 1 xs))
            `shouldBe` sort (map (root </>) fixtureDirs)
        sort (rights xs)
            `shouldBe` sort (map (root </>) fixtureFiles)

main :: IO ()
main = hspec $ do
    describe "ls id" $ do
        it "lists the entries of the directory only" $
            withTempDir $ \root -> do
                makeFixture root
                xs <- lsS id root
                sort xs `shouldBe` sort
                    [ Left (root </> "a"), Left (root </> "b")
                    , Left (root </> "c"), Right (root </> "f1")
                    , Right (root </> "f2")
                    ]

        it "lists nothing for an empty directory" $ withTempDir $ \root ->
            lsS id root `shouldReturn` []

        it "fails on a missing directory" $ withTempDir $ \root ->
            lsS id (root </> "missing") `shouldThrow` isDoesNotExistError

        it "reports a missing directory with the location ls" $
            withTempDir $ \root ->
                lsS id (root </> "missing")
                    `shouldThrow` ((== "ls") . ioeGetLocation)

        it "lists a file by itself" $ withTempDir $ \root -> do
            writeFile (root </> "file") ""
            lsS id (root </> "file") `shouldReturn` [Right (root </> "file")]
#if !defined(CABAL_OS_WINDOWS)
        it "lists the target of a symlink to a directory" $
            withTempDir $ \root -> do
                makeFixture root
                createDirectoryLink "a" (root </> "link")
                xs <- lsS id (root </> "link")
                sort xs `shouldBe` sort
                    [ Left (root </> "link" </> "a1")
                    , Left (root </> "link" </> "a2")
                    , Right (root </> "link" </> "f")
                    ]
#endif

    describe "ls (recursive True)" $ do
        listsTree (recursive True)

        it "uses WideDepthFirstReverse by default" $ withTempDir $ \root -> do
            makeFixture root
            xs <- lsS (recursive True) root
            ys <- lsS (recursive True . traversal WideDepthFirstReverse) root
            xs `shouldBe` ys

    describe "ls (recursive True . traversal WideDepthFirstReverse)" $ do
        let f = recursive True . traversal WideDepthFirstReverse
        listsTree f

        it "lists a directory's entries, then its subtrees" $
            withTempDir $ \root -> do
                makeFixture root
                xs <- lsS f root
                forM_ (withRest xs) $ \(d, rest) -> do
                    -- The subtree of d is one block; its entries come
                    -- first in it.
                    let sub = filter (isUnder d) rest
                    (d, contiguous (isUnder d) rest) `shouldBe` (d, True)
                    (d, prefix (isChildOf d) sub) `shouldBe` (d, True)

    describe "ls (recursive True . traversal DepthFirst)" $ do
        let f = recursive True . traversal DepthFirst
        listsTree f

        it "lists the subtree of a directory right after it" $
            withTempDir $ \root -> do
                makeFixture root
                xs <- lsS f root
                forM_ (withRest xs) $ \(d, rest) ->
                    (d, prefix (isUnder d) rest) `shouldBe` (d, True)

    describe "ls (recursive True . traversal BreadthFirst)" $ do
        let f = recursive True . traversal BreadthFirst
        listsTree f

        it "lists level by level" $ withTempDir $ \root -> do
            makeFixture root
            ds <- map depth . paths <$> lsS f root
            and (zipWith (<=) ds (drop 1 ds)) `shouldBe` True
