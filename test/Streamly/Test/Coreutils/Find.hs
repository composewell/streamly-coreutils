{-# LANGUAGE CPP #-}
-- |
--
-- Module      : Streamly.Test.Coreutils.Find
-- Copyright   : (c) 2026 Composewell Technologies
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Tests for 'Streamly.Coreutils.Find'. The expected output is that of GNU
-- @find@. The order of the entries within a directory is the order the
-- operating system returns them in, so the traversal tests check the
-- structure of the output, not a fixed list.

module Streamly.Test.Coreutils.Find (main) where

import Control.Monad (forM_)
import Data.Either (lefts, rights)
import Data.List (isPrefixOf, sort)
import System.Directory
    ( createDirectoryIfMissing
#if !defined(CABAL_OS_WINDOWS)
    , createDirectory
    , createDirectoryLink
    , createFileLink
#endif
    )
import System.FilePath (splitDirectories, (</>))
import System.IO.Error (isDoesNotExistError)
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

#if !defined(CABAL_OS_WINDOWS)
import Data.Char (chr)
#endif

import Streamly.Coreutils.Find
    ( FindOptions
    , Traversal(..)
    , find
    , findChunked
    , findEither
    , maxDepth
    , maxResults
    , minDepth
    , traversal
#if !defined(CABAL_OS_WINDOWS)
    , findByteChunked
    , followSymlinks
#endif
    )

import qualified Streamly.Data.Stream as Stream
import qualified Streamly.FileSystem.Path as Path
#if !defined(CABAL_OS_WINDOWS)
import qualified Streamly.Data.Array as Array
#endif

-------------------------------------------------------------------------------
-- Fixture
-------------------------------------------------------------------------------

withTempDir :: (FilePath -> IO a) -> IO a
withTempDir = withSystemTempDirectory "find-test-"

-- | Directories of the fixture tree, relative to the root. The root has more
-- subdirectories than the chunked reader puts in one chunk (4).
fixtureDirs :: [FilePath]
fixtureDirs =
    [ "a", "a" </> "a1", "a" </> "a2", "a" </> "a1" </> "deep"
    , "b", "b" </> "b1", "c", "d", "e", "f"
    ]

-- | Files of the fixture tree, relative to the root.
fixtureFiles :: [FilePath]
fixtureFiles =
    [ "f1", "f2", "a" </> "fa", "a" </> "a1" </> "fa1"
    , "a" </> "a1" </> "deep" </> "fdeep", "b" </> "b1" </> "fb1"
    ]

makeFixture :: FilePath -> IO ()
makeFixture root = do
    forM_ fixtureDirs $ \d -> createDirectoryIfMissing True (root </> d)
    forM_ fixtureFiles $ \f -> writeFile (root </> f) ""

-- | All paths of the fixture tree at depths from @lo@ to @hi@, the root
-- being at depth 0.
fixtureAt :: FilePath -> Int -> Int -> [FilePath]
fixtureAt root lo hi =
    sort
        [ root </> p
        | p <- "" : fixtureDirs ++ fixtureFiles
        , let n = if null p then 0 else depth p
        , n >= lo, n <= hi
        ]

-------------------------------------------------------------------------------
-- Running find
-------------------------------------------------------------------------------

type Options = FindOptions -> FindOptions

findS :: Options -> FilePath -> IO [FilePath]
findS f dir = do
    p <- Path.fromString dir
    map Path.toString <$> Stream.toList (find f p)

findEitherS :: Options -> FilePath -> IO [Either FilePath FilePath]
findEitherS f dir = do
    p <- Path.fromString dir
    xs <- Stream.toList (findEither f p)
    pure (map (either (Left . Path.toString) (Right . Path.toString)) xs)

findChunkedS :: Options -> FilePath -> IO [[FilePath]]
findChunkedS f dir = do
    p <- Path.fromString dir
    map (map Path.toString) <$> Stream.toList (findChunked f p)

#if !defined(CABAL_OS_WINDOWS)
findByteChunkedS :: Options -> FilePath -> IO [FilePath]
findByteChunkedS f dir = do
    p <- Path.fromString dir
    arrs <- Stream.toList (findByteChunked f p)
    pure $ lines $ map (chr . fromIntegral) $ concatMap Array.toList arrs
#endif

-- | The output of every find function, as a flat list of paths.
variants :: [(String, Options -> FilePath -> IO [FilePath])]
variants =
    [ ("find", findS)
    , ("findEither", \f d -> map (either id id) <$> findEitherS f d)
    , ("findChunked", \f d -> concat <$> findChunkedS f d)
#if !defined(CABAL_OS_WINDOWS)
    , ("findByteChunked", findByteChunkedS)
#endif
    ]

-------------------------------------------------------------------------------
-- Structure of the output
-------------------------------------------------------------------------------

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

-- | Each directory in the list with the paths that follow it.
withRest :: [Either FilePath FilePath] -> [(FilePath, [FilePath])]
withRest xs =
    [ (d, map (either id id) rest)
    | (Left d, rest) <- zip xs (drop 1 (scanr (:) [] xs))
    ]

-- | Each directory is listed before all paths under it.
parentsFirst :: [Either FilePath FilePath] -> Bool
parentsFirst xs =
    and
        [ not (any (isUnder d) earlier)
        | (Left d, earlier) <- zip xs (scanl (flip (:)) [] paths)
        ]

    where

    paths = map (either id id) xs

isDfs :: [Either FilePath FilePath] -> Bool
isDfs xs = and [prefix (isUnder d) rest | (d, rest) <- withRest xs]

isWideDfs :: [Either FilePath FilePath] -> Bool
isWideDfs xs =
    and
        [ contiguous (isUnder d) rest
            && prefix (isChildOf d) (filter (isUnder d) rest)
        | (d, rest) <- withRest xs
        ]

isBfs :: [Either FilePath FilePath] -> Bool
isBfs xs =
    let ds = map (depth . either id id) xs
     in and (zipWith (<=) ds (drop 1 ds))

-------------------------------------------------------------------------------
-- Tests
-------------------------------------------------------------------------------

-- | The order guarantee of each traversal, if any.
type Order = [Either FilePath FilePath] -> Bool

traversals :: [(Traversal, String, Maybe Order)]
traversals =
    [ (DepthFirst, "DepthFirst", Just isDfs)
    , (WideDepthFirstReverse, "WideDepthFirstReverse", Just isWideDfs)
    , (BreadthFirst, "BreadthFirst", Just isBfs)
    , (Interleaved, "Interleaved", Just parentsFirst)
    , (DepthFirstParallel, "DepthFirstParallel", Nothing)
    , ( DepthFirstParallelInterleaved
      , "DepthFirstParallelInterleaved"
      , Nothing
      )
    , (DepthFirstParallelOrdered, "DepthFirstParallelOrdered", Just isDfs)
    ]

traversalSpec :: (Traversal, String, Maybe Order) -> Spec
traversalSpec (t, name, order) =
    describe ("traversal " ++ name) $ do
        let f = traversal t

        it "findEither lists the tree, directories as Left" $
            withTempDir $ \root -> do
                makeFixture root
                xs <- findEitherS f root
                sort (lefts xs)
                    `shouldBe` sort (root : map (root </>) fixtureDirs)
                sort (rights xs) `shouldBe` sort (map (root </>) fixtureFiles)

        forM_ variants $ \(vname, run) ->
            it (vname ++ " lists every path once") $
                withTempDir $ \root -> do
                    makeFixture root
                    xs <- run f root
                    sort xs `shouldBe` fixtureAt root 0 maxBound

        case order of
            Nothing -> pure ()
            Just ok ->
                it "findEither lists the tree in its order" $
                    withTempDir $ \root -> do
                        makeFixture root
                        xs <- findEitherS f root
                        ok xs `shouldBe` True

depthSpec :: Spec
depthSpec =
    forM_ variants $ \(vname, run) ->
        describe vname $ do
            let check f lo hi = withTempDir $ \root -> do
                    makeFixture root
                    xs <- run f root
                    sort xs `shouldBe` fixtureAt root lo hi

            it "maxDepth 0 lists the root only" $ check (maxDepth 0) 0 0
            it "maxDepth 1 lists the root and its entries" $
                check (maxDepth 1) 0 1
            it "minDepth 1 . maxDepth 1 lists the entries of the root" $
                check (minDepth 1 . maxDepth 1) 1 1
            it "minDepth 2 lists depths 2 and more" $
                check (minDepth 2) 2 maxBound
            it "minDepth 2 . maxDepth 3 lists depths 2 to 3" $
                check (minDepth 2 . maxDepth 3) 2 3
            it "negative depths are 0" $
                check (minDepth (-1) . maxDepth (-1)) 0 0
            it "with DepthFirstParallel" $
                check
                    (minDepth 1 . maxDepth 2 . traversal DepthFirstParallel)
                    1
                    2

maxResultsSpec :: Spec
maxResultsSpec =
    forM_ variants $ \(vname, run) ->
        describe vname $ do
            it "maxResults 0 lists nothing" $ withTempDir $ \root -> do
                makeFixture root
                run (maxResults 0) root `shouldReturn` []

            it "maxResults n lists the first n paths" $
                withTempDir $ \root -> do
                    makeFixture root
                    xs <- run id root
                    forM_ [1, 3, 7, length xs, length xs + 5] $ \n ->
                        run (maxResults n) root
                            `shouldReturn` take n xs

            it "with a depth limit" $ withTempDir $ \root -> do
                makeFixture root
                xs <- run (maxResults 4 . minDepth 1) root
                length xs `shouldBe` 4
                all (`elem` fixtureAt root 1 maxBound) xs `shouldBe` True

rootSpec :: Spec
rootSpec =
    forM_ variants $ \(vname, run) ->
        describe vname $ do
            it "lists a file root by itself" $ withTempDir $ \root -> do
                writeFile (root </> "file") ""
                run id (root </> "file") `shouldReturn` [root </> "file"]

            it "lists an empty directory by itself" $ withTempDir $ \root ->
                run id root `shouldReturn` [root]

            it "fails on a missing root" $ withTempDir $ \root ->
                run id (root </> "missing")
                    `shouldThrow` isDoesNotExistError

            -- GNU find appends an entry name to a root that ends with a
            -- separator without adding another separator.
            it "keeps the trailing separator of the root" $
                withTempDir $ \root -> do
                    makeFixture root
                    let r = root ++ "/"
                    xs <- run (maxDepth 1) r
                    sort xs `shouldBe` sort
                        (r : map (r ++) (filter ((== 1) . depth)
                            (fixtureDirs ++ fixtureFiles)))

#if !defined(CABAL_OS_WINDOWS)
            it "does not follow a symlink by default" $
                withTempDir $ \root -> do
                    makeFixture root
                    createDirectoryLink "a" (root </> "link")
                    xs <- run id root
                    filter (isUnder (root </> "link")) xs `shouldBe` []
                    (root </> "link") `elem` xs `shouldBe` True

            it "lists a symlink root by itself by default" $
                withTempDir $ \root -> do
                    makeFixture root
                    createDirectoryLink "a" (root </> "link")
                    run id (root </> "link") `shouldReturn` [root </> "link"]

            it "follows symlinks with followSymlinks True" $
                withTempDir $ \root -> do
                    makeFixture root
                    createDirectoryLink "a" (root </> "link")
                    let a = root </> "a"
                        link = root </> "link"
                        underA = filter (\p -> p == a || isUnder a p)
                                    (fixtureAt root 0 maxBound)
                    xs <- run (followSymlinks True) link
                    sort xs `shouldBe`
                        sort (map ((link ++) . drop (length a)) underA)

            it "does not fail on a symlink loop with followSymlinks True" $
                withTempDir $ \root -> do
                    createFileLink "self" (root </> "self")
                    xs <- run (followSymlinks True) root
                    root `elem` xs `shouldBe` True

            -- Descends through the link until path resolution fails with
            -- ELOOP, which is ignored.
            it "stops at a symlink to an ancestor with followSymlinks True" $
                withTempDir $ \root -> do
                    createDirectory (root </> "d")
                    createDirectoryLink ".." (root </> "d" </> "up")
                    xs <- run (followSymlinks True) root
                    (root </> "d" </> "up") `elem` xs `shouldBe` True
#endif

main :: IO ()
main = hspec $ do
    describe "findEither" $ do
        it "lists the root as Left and a file as Right" $
            withTempDir $ \root -> do
                makeFixture root
                xs <- findEitherS id root
                Left root `elem` xs `shouldBe` True
                Right (root </> "f1") `elem` xs `shouldBe` True
#if !defined(CABAL_OS_WINDOWS)
        it "lists a symlink to a directory as Right" $
            withTempDir $ \root -> do
                makeFixture root
                createDirectoryLink "a" (root </> "link")
                xs <- findEitherS id root
                Right (root </> "link") `elem` xs `shouldBe` True
#endif
    describe "findChunked" $
        it "outputs no empty chunk" $ withTempDir $ \root -> do
            makeFixture root
            xs <- findChunkedS id root
            any null xs `shouldBe` False
    forM_ traversals traversalSpec
    describe "depth limits" depthSpec
    describe "maxResults" maxResultsSpec
    describe "root" rootSpec
