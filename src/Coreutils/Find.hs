{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-unused-binds #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
-- |
-- Module      : Coreutils.Find
-- Copyright   : (c) 2026 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Similar to GNU find. Not all options are implemented yet.
--
-- Examples:
-- List the current directory recursively using the internal traversal variants.
--
-- > main :: IO ()
-- > main = do
-- >     hSetBuffering stdout LineBuffering
-- >     let path = fromJust $ Path.fromString "."
-- > #if !defined(mingw32_HOST_OS) && !defined(__MINGW32__)
-- >     Stream.fold (Handle.writeChunks stdout)
-- >         $ findByteChunked id path
-- > #else
-- >     Stream.fold (Handle.writeWith 32000 stdout)
-- >         $ reEncode
-- >         $ Stream.unfoldEachEndBy 10 Array.reader
-- >         $ fmap Path.toArray
-- >         $ Stream.unfoldEach Unfold.fromList
-- >         $ findChunked id path
-- > #endif
--
-- > main :: IO ()
-- > main = do
-- >     hSetBuffering stdout LineBuffering
-- >     let path = fromJust $ Path.fromString "."
-- >     Stream.fold (Handle.writeWith 32000 stdout)
-- >         $ reEncode
-- >         $ Stream.unfoldEachEndBy 10 Array.reader
-- >         $ fmap Path.toArray
-- >         $ Stream.unfoldEach Unfold.fromList
-- >         $ findChunked id path
--
-- > main :: IO ()
-- > main = do
-- >     hSetBuffering stdout LineBuffering
-- >     let path = fromJust $ Path.fromString "."
-- >     Stream.fold (Handle.writeWith 32000 stdout)
-- >         $ reEncode
-- >         $ Stream.unfoldEachEndBy 10 Array.reader
-- >         $ fmap Path.toArray
-- >         $ find id path
--
-- Compare the above example with GNU @find@ or rust @fd@. To compare listing
-- the current directory recursively, use the following commands:
--
-- @
-- time find > /dev/null # GNU find
-- time fd -u > /dev/null # Rust fd
-- time hfind > /dev/null # This, Haskell implementation
-- @

module Coreutils.Find
    (
      find
    , findChunked
    , serialDfs
    , serialBfs
    , serialBfsRev
    , serialAppend
    , serialInterleaved
    , parallelUnordered
    , parallelInterleaved
    , parallelOrdered
#if !defined(mingw32_HOST_OS) && !defined(__MINGW32__)
    , findByteChunked
#endif

    -- * Options
    , FindOptions
    , maxResults
    )
where

import Data.Function ((&))
import Data.Functor.Identity (runIdentity)
import Data.Maybe (fromJust)
import Data.Word (Word8)
import Streamly.Data.Array (Array)
import Streamly.Data.Stream (Stream)
import Streamly.Data.Unfold (Unfold)
import Streamly.FileSystem.DirIO (ReadOptions)
import Streamly.FileSystem.Path (Path, OsWord)
import System.IO (stdout, hSetBuffering, BufferMode(LineBuffering))

import qualified Streamly.Data.Stream.Prelude as Stream
import qualified Streamly.Data.Array as Array
import qualified Streamly.FileSystem.DirIO as DirIO
import qualified Streamly.Internal.Data.Array as GArray
    ( compactMax'
    , read
    , unsafeSliceOffLen
    )
import qualified Streamly.Internal.Data.Stream as Stream
    ( unfoldEachEndBy
    , concatIterate
    , bfsConcatIterate
    , altBfsConcatIterate
    , postscanlMaybe
    )
import qualified Streamly.Data.StreamK as StreamK
import qualified Streamly.Internal.Data.StreamK as StreamK
    (concatIterateWith, mergeIterateWith)
import qualified Streamly.Data.Unfold as Unfold
import qualified Streamly.Internal.Data.Unfold as Unfold
    (either, nil)
import qualified Streamly.Internal.FileSystem.DirIO as Dir
    (readEitherChunks, readEitherPaths, eitherReaderPaths)
import qualified Streamly.FileSystem.Handle as Handle
import qualified Streamly.FileSystem.Path as Path
import qualified Streamly.Internal.FileSystem.Path as Path (toArray)
#if !defined(mingw32_HOST_OS) && !defined(__MINGW32__)
import qualified Streamly.Internal.FileSystem.Posix.ReadDir as Dir
    (readEitherByteChunks)
#else
import qualified Streamly.Unicode.Stream as Stream
#endif

import Streamly.Internal.Data.Scanl (Step(..), Scanl(..))
import qualified Streamly.Internal.Data.Scanl as Scanl
import qualified Streamly.Internal.Data.Fold as Fold
import qualified Streamly.Internal.Data.Array as Array

--
-- Running on a sample directory tree the concurrent rust "fd" tool took 150 ms
-- (real time). On the same tree the fastest variant using Haskell streamly
-- below took 94 ms. The time taken by other variants on the same tree is noted
-- in the comments. The fastest serial implementation using Haskell streamly
-- takes similar time as the concurrent rust "fd".
--
-- The code for directory traversal is just a few lines. This file is bigger
-- because we have implemented it in around 27 possible ways. To try other
-- variants just uncomment the relevant line and comment the currently enabled
-- line.

data FindTraversal
    = FindSerialDfs
    | FindSerialBfs
    | FindSerialBfsRev
    | FindSerialAppend
    | FindSerialInterleaved
    | FindParallelUnordered
    | FindParallelInterleaved
    | FindParallelOrdered

data FindOptions = FindOptions
    { findTraversal :: FindTraversal
    , findMaxResults :: Maybe Int
    }

defaultConfig :: FindOptions
defaultConfig =
    FindOptions
        { findTraversal = FindSerialDfs
        , findMaxResults = Nothing
        }

serialDfs :: FindOptions -> FindOptions
serialDfs cfg = cfg {findTraversal = FindSerialDfs}

serialBfs :: FindOptions -> FindOptions
serialBfs cfg = cfg {findTraversal = FindSerialBfs}

serialBfsRev :: FindOptions -> FindOptions
serialBfsRev cfg = cfg {findTraversal = FindSerialBfsRev}

serialAppend :: FindOptions -> FindOptions
serialAppend cfg = cfg {findTraversal = FindSerialAppend}

serialInterleaved :: FindOptions -> FindOptions
serialInterleaved cfg = cfg {findTraversal = FindSerialInterleaved}

parallelUnordered :: FindOptions -> FindOptions
parallelUnordered cfg = cfg {findTraversal = FindParallelUnordered}

parallelInterleaved :: FindOptions -> FindOptions
parallelInterleaved cfg = cfg {findTraversal = FindParallelInterleaved}

parallelOrdered :: FindOptions -> FindOptions
parallelOrdered cfg = cfg {findTraversal = FindParallelOrdered}

maxResults :: Int -> FindOptions -> FindOptions
maxResults n cfg = cfg {findMaxResults = Just (max 0 n)}

{-# INLINE recReadOpts #-}
recReadOpts :: ReadOptions -> ReadOptions
{-# INLINE reEncode #-}
reEncode :: Stream IO OsWord -> Stream IO Word8
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
recReadOpts = id

reEncode =
      Stream.encodeUtf8
    . Stream.decodeUtf16le
#else
recReadOpts =
      DirIO.followSymlinks True
    . DirIO.ignoreSymlinkLoops False
    . DirIO.ignoreMissing True
    . DirIO.ignoreInaccessible True

reEncode = id
#endif

data Counts = Counts !Int !Int deriving Show

{-# INLINE countStep #-}
countStep :: Monad m => Counts -> Word8 -> m (Step Counts (Either Int Int))
countStep (Counts l c) ch =
    let l1 = if ch == 10 then l - 1 else l
     in if l1 == 0
        then return $ Done $ Left (c + 1)
        else return $ Partial $ Counts l1 (c + 1)

{-# INLINE countExtract #-}
countExtract :: Monad m => Counts -> m (Either a Int)
countExtract (Counts l _) = return $ Right l

{-# INLINE count #-}
count :: Monad m => Int -> Fold.Fold m Word8 (Either Int Int)
count l = Fold.foldtM' countStep (return $ Partial (Counts l 0 )) countExtract

-- XXX Scanl is an awkward abstraction for the case when we are emitting every
-- element and just need to transform the elements using the state. We need a
-- smapM instead for this case. In the scan we are forced to use a Maybe and
-- then catMaybe unnecessarily to store the elements. Because only in the
-- initial state we do not have an element.
--
{-# INLINE scanStep #-}
scanStep :: Monad m =>
       (Int, Maybe (Array Word8))
    -> Array Word8
    -> m (Step (Int, Maybe (Array Word8)) (Maybe (Array Word8)))
scanStep (n, _) arr = do
    r <- Array.read arr & Stream.fold (count n)
    case r of
        Left len -> return $ Done $ Just (Array.unsafeSliceOffLen 0 len arr)
        Right cnt ->
            if cnt /= 0
            then return $ Partial (cnt, Just arr)
            else return $ Done (Just arr)

{-# INLINE scanExtract #-}
scanExtract :: Monad m => (Int, Maybe (Array Word8)) -> m (Maybe (Array Word8))
scanExtract (_, arr) = return arr

{-# INLINE scanFinal #-}
scanFinal :: Monad m => (Int, Maybe (Array Word8)) -> m (Maybe (Array Word8))
scanFinal (_, arr) = return arr

{-# INLINE takeN #-}
takeN :: Int -> Stream IO (Array Word8) -> Stream IO (Array Word8)
takeN n
    | n <= 0 = const Stream.nil
    | otherwise =
        Stream.postscanlMaybe
            (Scanl
                scanStep
                (return (Partial (n, Nothing)))
                scanExtract
                scanFinal)

#if !defined(mingw32_HOST_OS) && !defined(__MINGW32__)
-- Fastest implementation, only works for posix as of now.
findByteChunked :: (FindOptions -> FindOptions) -> Path -> Stream IO (Array Word8)
findByteChunked f path =
        transform $ Stream.catRights $
            case findTraversal opts of
                FindSerialDfs ->
                    Stream.concatIterate streamDirMaybe -- 154 ms
                    $ Stream.fromPure (Left [path])
                FindSerialBfs ->
                    Stream.bfsConcatIterate streamDirMaybe -- 154 ms
                    $ Stream.fromPure (Left [path])
                FindSerialBfsRev ->
                    Stream.altBfsConcatIterate streamDirMaybe -- 154 ms
                    $ Stream.fromPure (Left [path])
                FindSerialAppend ->
                    concatIterateWith StreamK.append -- 154 ms
                    $ Stream.fromPure (Left [path])
                FindSerialInterleaved ->
                    mergeIterateWith StreamK.interleave -- 154 ms
                    $ Stream.fromPure (Left [path])
                FindParallelUnordered ->
                    -- XXX To reduce concurrency overhead, perform buffering in
                    -- each worker and post the buffer or return [Path] and
                    -- then unfold it.
                    Stream.parConcatIterate id streamDir -- 94 ms
                    $ Stream.fromPure (Left [path])
                FindParallelInterleaved ->
                    Stream.parConcatIterate (Stream.interleaved True) streamDir -- 94 ms
                    $ Stream.fromPure (Left [path])
                FindParallelOrdered ->
                    Stream.parConcatIterate (Stream.ordered True) streamDir -- 154 ms
                    $ Stream.fromPure (Left [path])

    where

    {-# INLINE transform #-}
    transform s = maybe s (\n -> takeN n s) (findMaxResults opts)

    opts = f defaultConfig

    concatIterateWith combine =
          StreamK.toStream
        . StreamK.concatIterateWith combine (StreamK.fromStream . streamDir)
        . StreamK.fromStream

    mergeIterateWith combine =
          StreamK.toStream
        . StreamK.mergeIterateWith combine (StreamK.fromStream . streamDir)
        . StreamK.fromStream

    -- cfg = Stream.eager False . Stream.maxBuffer 2000 . Stream.maxThreads 2
    streamDir :: Either [Path] b -> Stream IO (Either [Path] (Array Word8))
    streamDir = either (Dir.readEitherByteChunks recReadOpts) (const Stream.nil)

    streamDirMaybe :: Either [Path] b -> Maybe (Stream IO (Either [Path] (Array Word8)))
    streamDirMaybe = either (Just . Dir.readEitherByteChunks recReadOpts) (const Nothing)
#endif

-- Faster than the find implementation below
findChunked :: (FindOptions -> FindOptions) -> Path -> Stream IO [Path]
findChunked f path =
        -- XXX implement maxResults
        Stream.catRights $
            case findTraversal (f defaultConfig) of
                FindSerialDfs ->
                    Stream.concatIterate streamDirMaybe -- 264 ms
                    $ Stream.fromPure (Left [path])
                FindSerialBfs ->
                    Stream.bfsConcatIterate streamDirMaybe -- 264 ms
                    $ Stream.fromPure (Left [path])
                FindSerialBfsRev ->
                    Stream.altBfsConcatIterate streamDirMaybe -- 264 ms
                    $ Stream.fromPure (Left [path])
                FindSerialAppend ->
                    concatIterateWith StreamK.append -- 164 ms
                    $ Stream.fromPure (Left [path])
                FindSerialInterleaved ->
                    mergeIterateWith StreamK.interleave -- 194 ms
                    $ Stream.fromPure (Left [path])
                FindParallelUnordered ->
                    Stream.parConcatIterate id streamDir -- 124 ms
                    $ Stream.fromPure (Left [path])
                FindParallelInterleaved ->
                    Stream.parConcatIterate (Stream.interleaved True) streamDir -- 134 ms
                    $ Stream.fromPure (Left [path])
                FindParallelOrdered ->
                    Stream.parConcatIterate (Stream.ordered True) streamDir -- 174 ms
                    $ Stream.fromPure (Left [path])

    where

    concatIterateWith combine =
          StreamK.toStream
        . StreamK.concatIterateWith combine (StreamK.fromStream . streamDir)
        . StreamK.fromStream

    mergeIterateWith combine =
          StreamK.toStream
        . StreamK.mergeIterateWith combine (StreamK.fromStream . streamDir)
        . StreamK.fromStream

    streamDir :: Either [Path] b -> Stream IO (Either [Path] [Path])
    streamDir = either (Dir.readEitherChunks recReadOpts) (const Stream.nil)

    streamDirMaybe :: Either [Path] b -> Maybe (Stream IO (Either [Path] [Path]))
    streamDirMaybe = either (Just . Dir.readEitherChunks recReadOpts) (const Nothing)

find :: (FindOptions -> FindOptions) -> Path -> Stream IO Path
find f path =
        -- XXX implement maxResults
        Stream.catRights $
            case findTraversal (f defaultConfig) of
                FindSerialDfs ->
                    --  Stream.unfoldIterateDfs unfoldDir -- 284 ms
                    Stream.concatIterate streamDirMaybe -- 274 ms
                    $ Stream.fromPure (Left path)
                FindSerialBfs ->
                    -- May fail with too many open files:
                    --  Stream.unfoldIterateBfs unfoldDir
                    Stream.bfsConcatIterate streamDirMaybe -- 274 ms
                    $ Stream.fromPure (Left path)
                FindSerialBfsRev ->
                    --  Stream.unfoldIterateBfsRev unfoldDir -- 344 ms
                    Stream.altBfsConcatIterate streamDirMaybe -- 264 ms
                    $ Stream.fromPure (Left path)
                FindSerialAppend ->
                    concatIterateWith StreamK.append -- 204 ms
                    $ Stream.fromPure (Left path)
                FindSerialInterleaved ->
                    mergeIterateWith StreamK.interleave -- 304 ms
                    $ Stream.fromPure (Left path)
                FindParallelUnordered ->
                    Stream.parConcatIterate id streamDir -- 174 ms
                    $ Stream.fromPure (Left path)
                FindParallelInterleaved ->
                    Stream.parConcatIterate (Stream.interleaved True) streamDir -- 224 ms
                    $ Stream.fromPure (Left path)
                FindParallelOrdered ->
                    Stream.parConcatIterate (Stream.ordered True) streamDir -- 234 ms
                    $ Stream.fromPure (Left path)

    where

    concatIterateWith combine =
          StreamK.toStream
        . StreamK.concatIterateWith combine (StreamK.fromStream . streamDir)
        . StreamK.fromStream

    mergeIterateWith combine =
          StreamK.toStream
        . StreamK.mergeIterateWith combine (StreamK.fromStream . streamDir)
        . StreamK.fromStream

    streamDir :: Either Path b -> Stream IO (Either Path Path)
    streamDir = either (Dir.readEitherPaths recReadOpts) (const Stream.nil)

    unfoldDir :: Unfold IO (Either Path b) (Either Path Path)
    unfoldDir = Unfold.either (Dir.eitherReaderPaths recReadOpts) Unfold.nil

    streamDirMaybe :: Either Path b -> Maybe (Stream IO (Either Path Path))
    streamDirMaybe = either (Just . Dir.readEitherPaths recReadOpts) (const Nothing)
