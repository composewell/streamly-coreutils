{-# LANGUAGE CPP #-}
-- |
-- Module      : Streamly.Coreutils.Find
-- Copyright   : (c) 2026 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Walk a directory tree, like GNU @find@ without an expression. The root of
-- the tree is the path given to a find function, what GNU @find@ calls the
-- starting point. It is not the file system root @/@. The output is the root
-- followed by every entry under it, each prefixed with the root. A root that
-- is not a directory is listed by itself.
--
-- Usage:
--
-- >> find id dir                            -- find dir
-- >> find (maxDepth 1) dir                  -- find dir -maxdepth 1
-- >> find (minDepth 1 . maxDepth 1) dir     -- find dir -mindepth 1
--                                           --   -maxdepth 1
-- >> find (followSymlinks True) dir         -- find -L dir
-- >> find (traversal DepthFirstParallel) dir
--                                           -- unordered, concurrent
-- >> findEither id dir                      -- directories as Left
--
-- Symbolic links are listed but not followed: a symlink to a directory is
-- reported as an entry and its target is not traversed, like the GNU find
-- default (@find -P@). Use 'followSymlinks' to follow them.
--
-- Caveats, compared to GNU @find@:
--
-- * Entries are not sorted. Within a directory they are in the order the
--   operating system returns them, as with GNU @find@.
-- * A directory that cannot be read, e.g. for lack of permission, ends the
--   stream with an exception. GNU @find@ reports it and continues.
--
-- The @hfd@ executable in this package is a command line driver for
-- 'findByteChunked'. To compare its speed with GNU @find@ and Rust @fd@:
--
-- @
-- time find > \/dev\/null    # GNU find
-- time fd -u > \/dev\/null   # Rust fd
-- time hfd > \/dev\/null     # this module
-- @

module Streamly.Coreutils.Find
    (
      find
    , findEither
    , findChunked
#if !defined(CABAL_OS_WINDOWS)
    , findByteChunked
#endif

    -- * Options
    , FindOptions
    , Traversal (..)
    , traversal
    , followSymlinks
    , minDepth
    , maxDepth
    , maxResults
    )
where

import Data.Bifunctor (bimap)
import Data.Maybe
    ( fromMaybe
#if !defined(CABAL_OS_WINDOWS)
    , isJust
#endif
    )
import Foreign.C.Error (eNOENT)
import Streamly.Data.Stream.Prelude (Stream)
import Streamly.FileSystem.DirIO (ReadOptions)
import Streamly.FileSystem.Path (Path)

import qualified Streamly.Data.Scanl as Scanl
import qualified Streamly.Data.Stream.Prelude as Stream
import qualified Streamly.Data.StreamK as StreamK
#if !defined(CABAL_OS_WINDOWS)
import qualified Streamly.FileSystem.DirIO as DirIO
import qualified Streamly.FileSystem.Path as Path
#endif
import qualified Streamly.Internal.Data.Stream as Stream
    ( altBfsConcatIterate
    , bfsConcatIterate
    , takeEndBy
#if !defined(CABAL_OS_WINDOWS)
    , postscanlMaybe
    , unfoldEachEndBy
#endif
    )
import qualified Streamly.Internal.Data.StreamK as StreamK
    (concatIterateWith, mergeIterateWith)
import qualified Streamly.Internal.FileSystem.DirIO as Dir
    (readEitherChunks, readEitherPaths)
import qualified Streamly.Internal.FileSystem.Path as Path
    (hasTrailingSeparator)

#if !defined(CABAL_OS_WINDOWS)
import Data.Word (Word8)
import Streamly.Data.Array (Array)
import Streamly.Internal.Data.Scanl (Scanl(..), Step(..))

import qualified Streamly.Data.Array as Array
import qualified Streamly.Internal.Data.Array as Array (unsafeSliceOffLen)
import qualified Streamly.Internal.Data.Fold as Fold (Fold, foldtM')
#if MIN_VERSION_streamly_core(0,4,0)
import qualified Streamly.Internal.Syscall.Posix.ReadDir as Dir
    (readEitherByteChunks)
#else
import qualified Streamly.Internal.FileSystem.Posix.ReadDir as Dir
    (readEitherByteChunks)
#endif
#endif

import Streamly.Coreutils.Error (errnoError)
import Streamly.Coreutils.FileTest (isDir, pathExists, test, testl)

-- = Design notes
--
-- * Timings of listing a sample tree with each traversal, from the
--   streamly-examples ListDir example. Rust fd (concurrent) took 150 ms.
--
--   > Traversal                       find   findChunked   findByteChunked
--   > DepthFirst                      204    164           154
--   > WideDepthFirstReverse           264    264           154
--   > BreadthFirst                    274    264           154
--   > Interleaved                     304    194           154
--   > DepthFirstParallel              174    124            94
--   > DepthFirstParallelInterleaved   224    134            94
--   > DepthFirstParallelOrdered       234    174           154
--
-- * DepthFirst appends StreamK streams (StreamK.concatIterateWith
--   StreamK.append). Stream.concatIterate gives the same order but took 274,
--   264 and 154 ms; it is kept commented out in iterateWith for benchmarking.
--
-- * A parallel traversal is named by the serial order it is built on,
--   followed by the concurrency mode: DepthFirstParallel,
--   DepthFirstParallelInterleaved and DepthFirstParallelOrdered use
--   parConcatIterate, a concurrent concatIterate. Parallel breadth first
--   traversals can be added later as BreadthFirstParallel and
--   BreadthFirstParallelOrdered. A separate modifier for the concurrency
--   mode was rejected because not every combination of order and mode has
--   an implementation, e.g. WideDepthFirstReverse has no parallel version.
--
-- * For the unordered DepthFirstParallel, "depth first" describes how
--   directories are scheduled, not the output order. Even the scheduling
--   depends on the queue of the parConcatIterate channel ("The channel q
--   should be FIFO for DFS, otherwise it is BFS" in streamly). The
--   alternative considered was to name it plain Parallel, because it
--   guarantees no order, and to use an order prefix only for the ordered
--   variants and for a parallel traversal that really schedules breadth
--   first. DepthFirstParallel was chosen so that a future
--   BreadthFirstParallel has a matching name.
--
-- * The serial traversals use the concat tree iterations, not the unfold
--   ones (unfoldIterateDfs, bfsUnfoldIterate, ...). In streamly-core 0.3 the
--   unfold iterations open every pending directory as soon as it is emitted,
--   so BFS can exceed the open file limit.
--
-- * Each element carries its depth, the root being 0, to implement minDepth
--   and maxDepth. A directory at maxDepth is listed but not read.
--   readEitherChunks reads only the directories it is given, so all paths in
--   one of its chunks have the same depth.
--
-- * readEitherByteChunks reads the subdirectories it finds in the same call
--   until its buffer is full, so a chunk can contain several depths, and the
--   traversal only schedules the directories it returns unread. With a depth
--   limit, findByteChunked is built from findChunked instead.
--
-- * The chunked readers emit directories only as Left. find and findChunked
--   output the Left elements too, so that directories are listed like GNU
--   find does. The byte chunks of readEitherByteChunks include directories.

-------------------------------------------------------------------------------
-- Options
-------------------------------------------------------------------------------

-- | The order in which the tree is visited. All serial traversals list a
-- directory before its contents. For a tree in which directory @1@ contains
-- @2@ and @3@, @2@ contains @4@, @3@ contains @5@, @4@ contains @7@ and @5@
-- contains @6@, with entries listed in that order:
--
-- +-----------------------------+---------------------+
-- | Traversal                   | Output              |
-- +=============================+=====================+
-- | 'DepthFirst'                | 1, 2, 4, 7, 3, 5, 6 |
-- +-----------------------------+---------------------+
-- | 'DepthFirstParallelOrdered' | 1, 2, 4, 7, 3, 5, 6 |
-- +-----------------------------+---------------------+
-- | 'WideDepthFirstReverse'     | 1, 2, 3, 5, 6, 4, 7 |
-- +-----------------------------+---------------------+
-- | 'BreadthFirst'              | 1, 2, 3, 4, 5, 7, 6 |
-- +-----------------------------+---------------------+
--
-- 'Interleaved', 'DepthFirstParallel' and 'DepthFirstParallelInterleaved' do
-- not guarantee an order.
data Traversal =
      DepthFirst
      -- ^ Pre-order depth first, the order of GNU @find@. Keeps the
      -- directories on the current path open; a very deep tree can exceed
      -- the open file limit.
    | WideDepthFirstReverse
      -- ^ List all entries of a directory, then each of its subdirectories
      -- in turn, recursively, the last listed first. Keeps one directory
      -- open, and the subdirectories not yet read along the current path in
      -- memory.
    | BreadthFirst
      -- ^ Level by level: all entries at depth one, then all at depth two,
      -- and so on. Keeps one directory open, and the directories of the next
      -- level in memory, which can be many for a wide tree.
    | Interleaved
      -- ^ Interleave the entries of sibling directories, so that one large
      -- directory does not delay the others.
    | DepthFirstParallel
      -- ^ Read directories concurrently, scheduled depth first, and emit
      -- entries as soon as they are read. The fastest; the output order is
      -- not deterministic.
    | DepthFirstParallelInterleaved
      -- ^ Like 'DepthFirstParallel' but schedules the directories fairly, so
      -- that one large subtree does not delay the others.
    | DepthFirstParallelOrdered
      -- ^ Read directories concurrently, but emit the entries in the
      -- 'DepthFirst' order.

-- | Options for the find functions. Modify the defaults with 'traversal',
-- 'followSymlinks', 'minDepth', 'maxDepth' and 'maxResults'.
data FindOptions = FindOptions
    { findTraversal :: Traversal
    , findFollowSymlinks :: Bool
    , findMinDepth :: Int
    , findMaxDepth :: Maybe Int
    , findMaxResults :: Maybe Int
    }

defaultOptions :: FindOptions
defaultOptions =
    FindOptions
        { findTraversal = DepthFirst
        , findFollowSymlinks = False
        , findMinDepth = 0
        , findMaxDepth = Nothing
        , findMaxResults = Nothing
        }

-- | The order in which the tree is visited.
--
-- Default (without this modifier): 'DepthFirst', the order of GNU @find@.
traversal :: Traversal -> FindOptions -> FindOptions
traversal t cfg = cfg {findTraversal = t}

-- | Descend into directories reached through symbolic links, like @find -L@.
-- This includes a symbolic link given as the root (starting point).
--
-- Default (without this modifier): symbolic links are listed but not
-- followed, like @find -P@.
--
-- There is no loop detection: through a symbolic link to an ancestor
-- directory the traversal lists the same entries again at each level, about
-- 40 levels deep, until resolving the path exceeds the system limit on
-- symbolic links (ELOOP). Broken symbolic links, symbolic link loops and
-- symbolic links whose target cannot be accessed are left out of the
-- output. GNU @find -L@ reports each of these and continues. Has no effect
-- on Windows.
followSymlinks :: Bool -> FindOptions -> FindOptions
followSymlinks x cfg = cfg {findFollowSymlinks = x}

-- | Do not output entries at depths less than @n@, like @find -mindepth n@.
-- The root (starting point) is at depth 0 and the entries in it at depth 1.
-- Directories at smaller depths are still read. A negative @n@ is treated as
-- 0.
--
-- Default (without this modifier): 0, the root is listed.
minDepth :: Int -> FindOptions -> FindOptions
minDepth n cfg = cfg {findMinDepth = max 0 n}

-- | Do not descend below depth @n@, like @find -maxdepth n@. The root
-- (starting point) is at depth 0, so @maxDepth 0@ lists only the root and
-- @maxDepth 1@ lists the root and its entries. A negative @n@ is treated as
-- 0.
--
-- Default (without this modifier): no limit.
maxDepth :: Int -> FindOptions -> FindOptions
maxDepth n cfg = cfg {findMaxDepth = Just (max 0 n)}

-- | Stop after @n@ paths have been output. A negative @n@ is treated as 0.
--
-- Default (without this modifier): no limit.
maxResults :: Int -> FindOptions -> FindOptions
maxResults n cfg = cfg {findMaxResults = Just (max 0 n)}

-------------------------------------------------------------------------------
-- Traversal
-------------------------------------------------------------------------------

-- The ignore options apply only to the stat of an entry, done to classify a
-- symlink when following symlinks, or an entry of unknown type. GNU find
-- reports these errors and continues, so we ignore them rather than end the
-- stream. They do not apply to opening a directory. The options have no
-- effect on Windows, and streamly-core 0.3 does not export them there.
{-# INLINE readOptions #-}
readOptions :: FindOptions -> ReadOptions -> ReadOptions
#if defined(CABAL_OS_WINDOWS)
readOptions _ = id
#else
readOptions opts =
      DirIO.followSymlinks (findFollowSymlinks opts)
    . DirIO.ignoreSymlinkLoops True
    . DirIO.ignoreMissing True
    . DirIO.ignoreInaccessible True
#endif

-- | Expand a tree from the given roots using @next@ to produce the children
-- of an element, 'Nothing' for a leaf.
{-# INLINE iterateWith #-}
iterateWith ::
       Traversal
    -> (a -> Maybe (Stream IO a))
    -> Stream IO a
    -> Stream IO a
iterateWith t next =
    case t of
        -- Slower than the StreamK append, see the design notes.
        -- DepthFirst -> Stream.concatIterate next
        DepthFirst -> viaK (StreamK.concatIterateWith StreamK.append)
        -- XXX Add WideDepthFirst using wideDfsConcatIterate from
        -- streamly-core 0.4.
        WideDepthFirstReverse -> Stream.altBfsConcatIterate next
        BreadthFirst -> Stream.bfsConcatIterate next
        Interleaved -> viaK (StreamK.mergeIterateWith StreamK.interleave)
        DepthFirstParallel -> Stream.parConcatIterate id parNext
        DepthFirstParallelInterleaved ->
            Stream.parConcatIterate (Stream.interleaved True) parNext
        DepthFirstParallelOrdered ->
            Stream.parConcatIterate (Stream.ordered True) parNext

    where

    nextK = maybe StreamK.nil StreamK.fromStream . next

    viaK iter = StreamK.toStream . iter nextK . StreamK.fromStream

    -- parConcatIterate takes a Maybe function since streamly 0.12.0
#if MIN_VERSION_streamly(0,12,0)
    parNext = next
#else
    parNext = fromMaybe Stream.nil . next
#endif

-- | Whether a directory at depth @d@ is read.
{-# INLINE descends #-}
descends :: FindOptions -> Int -> Bool
descends opts d = d < fromMaybe maxBound (findMaxDepth opts)

-- | The root as a directory ('Left') or another entry ('Right'). Raise an
-- ENOENT exception if it does not exist.
isDirRoot :: FindOptions -> Path -> IO Bool
isDirRoot opts root = do
    let testRoot = if findFollowSymlinks opts then test else testl
    r <- testRoot root isDir
    if r
    then pure True
    else do
        exists <- testl root pathExists
        if exists
        then pure False
        else ioError (errnoError "find" eNOENT root)

-- | Walk the tree from the root, tagging each element with its depth, then
-- drop the elements above minDepth. @dirRoot@ and @fileRoot@ are the root
-- element for a directory and for any other root, @readRoot@ reads the root
-- directory and @readDir@ any other directory.
{-# INLINE walk #-}
walk ::
       FindOptions
    -> a
    -> b
    -> Stream IO (Either a b)
    -> (a -> Stream IO (Either a b))
    -> Path
    -> Stream IO (Either a b)
walk opts dirRoot fileRoot readRoot readDir root =
      fmap snd
    $ Stream.filter ((>= findMinDepth opts) . fst)
    $ iterateWith (findTraversal opts) next
    $ Stream.fromEffect (tagRoot <$> isDirRoot opts root)

    where

    tagRoot isD = (0 :: Int, if isD then Left dirRoot else Right fileRoot)

    next (0, Left _)
        | descends opts 0 = Just (fmap (1,) readRoot)
    next (d, Left dir)
        | descends opts d = Just (fmap (d + 1,) (readDir dir))
    next _ = Nothing

-- The chunked directory readers always put a separator between the directory
-- and the entry name, giving dir//name for a root dir/, where GNU find gives
-- dir/name. readEitherPaths does not, so a root that ends with a separator is
-- read with readEitherPaths.


-- | Output at most @n@ elements in total from a stream of chunks, cutting the
-- last chunk.
{-# INLINE takeChunks #-}
takeChunks :: Int -> Stream IO [a] -> Stream IO [a]
takeChunks n
    | n <= 0 = const Stream.nil
    | otherwise =
          fmap snd
        . Stream.takeEndBy ((<= 0) . fst)
        . Stream.postscanl (Scanl.mkScanl step (n, []))

    where

    step (r, _) xs = (r - length xs, take r xs)

-------------------------------------------------------------------------------
-- Find
-------------------------------------------------------------------------------

-- | Like 'find', with directories as 'Left' and all other entries as
-- 'Right'. A symbolic link is 'Right' unless it is followed with
-- 'followSymlinks'.
--
-- Pass 'id' for the defaults; each modifier documents its own default.
--
-- Raises an exception if the root (starting point) does not exist or a
-- directory cannot be read.
findEither ::
    (FindOptions -> FindOptions) -> Path -> Stream IO (Either Path Path)
findEither f root =
      maybe id Stream.take (findMaxResults opts)
    $ walk opts root root (readDir root) readDir root

    where

    opts = f defaultOptions

    readDir = Dir.readEitherPaths (readOptions opts)

-- | List the root (starting point) and every entry under it, like
-- @find root@. The root is listed as given and each entry is prefixed with
-- it.
--
-- To print the paths one per line, like @find root@:
--
-- > Stream.fold (Handle.writeWith 32000 stdout)
-- >     $ Stream.unfoldEachEndBy 10 Array.reader
-- >     $ fmap Path.toArray
-- >     $ find id root
--
-- @unfoldEachEndBy@ is in "Streamly.Internal.Data.Stream". On Windows
-- 'Path.toArray' gives UTF-16LE @Word16@ units; insert
-- @Unicode.encodeUtf8 . Unicode.decodeUtf16le@ (from
-- "Streamly.Unicode.Stream") before the fold to write UTF-8.
--
-- Pass 'id' for the defaults; each modifier documents its own default.
--
-- Raises an exception if the root (starting point) does not exist or a
-- directory cannot be read.
find :: (FindOptions -> FindOptions) -> Path -> Stream IO Path
find f = fmap (either id id) . findEither f

-- | Like 'find' but outputs the paths in chunks, which is faster. A chunk
-- holds either directories only or other entries only. Chunks are never
-- empty.
--
-- To print the paths one per line, flatten the chunks first:
--
-- > Stream.fold (Handle.writeWith 32000 stdout)
-- >     $ Stream.unfoldEachEndBy 10 Array.reader
-- >     $ fmap Path.toArray
-- >     $ Stream.unfoldEach Unfold.fromList
-- >     $ findChunked id root
--
-- @unfoldEachEndBy@ is in "Streamly.Internal.Data.Stream". On Windows
-- 'Path.toArray' gives UTF-16LE @Word16@ units; insert
-- @Unicode.encodeUtf8 . Unicode.decodeUtf16le@ (from
-- "Streamly.Unicode.Stream") before the fold to write UTF-8.
--
-- Pass 'id' for the defaults; each modifier documents its own default.
--
-- Raises an exception if the root (starting point) does not exist or a
-- directory cannot be read.
findChunked :: (FindOptions -> FindOptions) -> Path -> Stream IO [Path]
findChunked f root =
      maybe id takeChunks (findMaxResults opts)
    $ either id id
    <$> walk opts [root] [root] readRoot (Dir.readEitherChunks ro) root

    where

    opts = f defaultOptions

    ro = readOptions opts

    readRoot
        | Path.hasTrailingSeparator root =
            fmap (bimap pure pure) (Dir.readEitherPaths ro root)
        | otherwise = Dir.readEitherChunks ro [root]

#if !defined(CABAL_OS_WINDOWS)
data Counts = Counts !Int !Int

-- | Count newlines; Left is the length of the prefix of an array that holds
-- the last line wanted, Right the number of lines still wanted after it.
{-# INLINE countLines #-}
countLines :: Monad m => Int -> Fold.Fold m Word8 (Either Int Int)
countLines n = Fold.foldtM' step (pure (Partial (Counts n 0))) extract

    where

    step (Counts l c) ch =
        let l1 = if ch == 10 then l - 1 else l
         in pure
                $ if l1 == 0
                  then Done (Left (c + 1))
                  else Partial (Counts l1 (c + 1))

    extract (Counts l _) = pure (Right l)

-- XXX Scanl is an awkward abstraction for the case when we are emitting every
-- element and just need to transform the elements using the state. We need a
-- smapM instead for this case. In the scan we are forced to use a Maybe and
-- then catMaybe unnecessarily to store the elements. Because only in the
-- initial state we do not have an element.

-- | Take the first @n@ newline terminated lines from a stream of byte arrays.
{-# INLINE takeLines #-}
takeLines :: Int -> Stream IO (Array Word8) -> Stream IO (Array Word8)
takeLines n
    | n <= 0 = const Stream.nil
    | otherwise =
        Stream.postscanlMaybe
            (Scanl step (pure (Partial (n, Nothing))) extract extract)

    where

    extract (_, arr) = pure arr

    step (l, _) arr = do
        r <- Stream.fold (countLines l) (Array.read arr)
        pure $ case r of
            Left len -> Done (Just (Array.unsafeSliceOffLen 0 len arr))
            Right l1 -> Partial (l1, Just arr)

-- | Newline terminated paths in one byte array.
toLines :: [Path] -> IO (Array Word8)
toLines =
      Stream.fold Array.create
    . Stream.unfoldEachEndBy 10 Array.reader
    . Stream.fromList
    . fmap Path.toArray

-- | Like 'find' but outputs the paths as bytes, each path followed by a
-- newline, in arrays of up to 32 KB. This is the fastest way to list a tree.
-- A path containing a newline cannot be told apart from two paths.
--
-- To fill its buffers, the directory reader reads the subdirectories it
-- finds before returning, so the output is not in the order of the chosen
-- 'traversal'; the traversal schedules only the directories the reader
-- returns unread. With 'minDepth' or 'maxDepth', the output is built from
-- 'findChunked' instead, which is slower.
--
-- The arrays can be written out unchanged:
--
-- > Stream.fold (Handle.writeChunks stdout) $ findByteChunked id root
--
-- Pass 'id' for the defaults; each modifier documents its own default.
--
-- Raises an exception if the root (starting point) does not exist or a
-- directory cannot be read. Not available on Windows.
findByteChunked ::
    (FindOptions -> FindOptions) -> Path -> Stream IO (Array Word8)
findByteChunked f root =
    maybe id takeLines (findMaxResults opts)
        $ if findMinDepth opts > 0 || isJust (findMaxDepth opts)
          then Stream.mapM toLines (findChunked (const unlimited) root)
          else Stream.concatEffect $ do
            isD <- isDirRoot opts root
            rootLine <- toLines [root]
            pure
                $ Stream.cons rootLine
                $ if isD then readTree else Stream.nil

    where

    opts = f defaultOptions

    -- maxResults is applied by takeLines
    unlimited = opts {findMaxResults = Nothing}

    ro = readOptions opts

    readTree =
          Stream.catRights
        $ iterateWith (findTraversal opts) next
        $ if Path.hasTrailingSeparator root
          then Stream.concatMapM listRoot (Dir.readEitherPaths ro root)
          else Stream.fromPure (Left [root])

    -- A directory is output as a line and read as a seed.
    listRoot (Left dir) = do
        line <- toLines [dir]
        pure $ Stream.fromList [Right line, Left [dir]]
    listRoot (Right file) = Stream.fromPure . Right <$> toLines [file]

    next = either (Just . Dir.readEitherByteChunks ro) (const Nothing)
#endif
