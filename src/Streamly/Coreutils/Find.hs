{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-unused-binds #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
-- |
-- Module      : Streamly.Coreutils.Find
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
-- >>> :{
-- main :: IO ()
-- main = do
--     hSetBuffering stdout LineBuffering
--     let path = fromJust $ Path.fromString "."
-- #if !defined(mingw32_HOST_OS) && !defined(__MINGW32__)
--     Stream.fold (Handle.writeChunks stdout)
--         $ findByteChunked path
-- #else
--     Stream.fold (Handle.writeWith 32000 stdout)
--         $ reEncode
--         $ Stream.unfoldEachEndBy 10 Array.reader
--         $ fmap Path.toArray
--         $ Stream.unfoldEach Unfold.fromList
--         $ findChunked id path
-- #endif
-- :}
--
-- >>> :{
-- main :: IO ()
-- main = do
--     hSetBuffering stdout LineBuffering
--     let path = fromJust $ Path.fromString "."
--     Stream.fold (Handle.writeWith 32000 stdout)
--         $ reEncode
--         $ Stream.unfoldEachEndBy 10 Array.reader
--         $ fmap Path.toArray
--         $ Stream.unfoldEach Unfold.fromList
--         $ findChunked id path
-- :}
--
-- >>> :{
-- main :: IO ()
-- main = do
--     hSetBuffering stdout LineBuffering
--     let path = fromJust $ Path.fromString "."
--     Stream.fold (Handle.writeWith 32000 stdout)
--         $ reEncode
--         $ Stream.unfoldEachEndBy 10 Array.reader
--         $ fmap Path.toArray
--         $ find id path
-- :}
--
-- Compare the above example with GNU @find@ or rust @fd@. To compare listing
-- the current directory recursively, use the following commands:
--
-- @
-- time find > /dev/null # GNU find
-- time fd -u > /dev/null # Rust fd
-- time hfind > /dev/null # This, Haskell implementation
-- @

module Streamly.Coreutils.Find
    (
      find
    , findChunked
#if !defined(mingw32_HOST_OS) && !defined(__MINGW32__)
    , findByteChunked
#endif

    -- * Options
    , FindOptions
    )
where

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
import qualified Streamly.Internal.Data.Array as GArray (compactMax')
import qualified Streamly.Internal.Data.Stream as Stream
    (unfoldEachEndBy, concatIterateDfs, concatIterateBfs, concatIterateBfsRev)
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

newtype FindOptions = FindOptions {findConcurrent :: Bool}

defaultConfig :: FindOptions
defaultConfig = FindOptions False

concurrent :: Bool -> FindOptions -> FindOptions
concurrent opt cfg = cfg {findConcurrent = opt}

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

#if !defined(mingw32_HOST_OS) && !defined(__MINGW32__)
-- Fastest implementation, only works for posix as of now.
findByteChunked :: Path -> Stream IO (Array Word8)
findByteChunked path =
        Stream.catRights
        -- Serial
        --  $ Stream.concatIterateDfs streamDirMaybe -- 154 ms
        --  $ Stream.concatIterateBfs streamDirMaybe -- 154 ms
        --  $ Stream.concatIterateBfsRev streamDirMaybe -- 154 ms

        -- Serial using stream append and interleave
        --  $ concatIterateWith StreamK.append -- 154 ms
        --  $ mergeIterateWith StreamK.interleave  -- 154 ms

        -- Concurrent
        -- XXX To reduce concurrency overhead, perform buffering in each worker
        -- and post the buffer or return [Path] and then unfold it.
        $ Stream.parConcatIterate id streamDir -- 94 ms
        --  $ Stream.parConcatIterate (Stream.interleaved True) streamDir -- 94 ms
        --  $ Stream.parConcatIterate (Stream.ordered True) streamDir -- 154 ms

        $ Stream.fromPure (Left [path])

    where

    concatIterateWith f =
          StreamK.toStream
        . StreamK.concatIterateWith f (StreamK.fromStream . streamDir)
        . StreamK.fromStream

    mergeIterateWith f =
          StreamK.toStream
        . StreamK.mergeIterateWith f (StreamK.fromStream . streamDir)
        . StreamK.fromStream

    -- cfg = Stream.eager False . Stream.maxBuffer 2000 . Stream.maxThreads 2
    streamDir :: Either [Path] b -> Stream IO (Either [Path] (Array Word8))
    streamDir = either (Dir.readEitherByteChunks recReadOpts) (const Stream.nil)

    streamDirMaybe :: Either [Path] b -> Maybe (Stream IO (Either [Path] (Array Word8)))
    streamDirMaybe = either (Just . Dir.readEitherByteChunks recReadOpts) (const Nothing)
#endif

-- Faster than the find implementation below
findChunked :: (FindOptions -> FindOptions) -> Path -> Stream IO [Path]
findChunked _opts path =
        Stream.catRights

        -- Serial using streams
        --  $ Stream.concatIterateDfs streamDirMaybe    -- 264 ms
        --  $ Stream.concatIterateBfs streamDirMaybe    -- 264 ms
        --  $ Stream.concatIterateBfsRev streamDirMaybe -- 264 ms

        -- Serial using stream append and interleave
        --  $ concatIterateWith StreamK.append     -- 164 ms
        --  $ mergeIterateWith StreamK.interleave  -- 194 ms

        -- Concurrent
        $ Stream.parConcatIterate id streamDir -- 124 ms
        --  $ Stream.parConcatIterate (Stream.interleaved True) streamDir -- 134 ms
        --  $ Stream.parConcatIterate (Stream.ordered True) streamDir -- 174 ms

        $ Stream.fromPure (Left [path])

    where

    concatIterateWith f =
          StreamK.toStream
        . StreamK.concatIterateWith f (StreamK.fromStream . streamDir)
        . StreamK.fromStream

    mergeIterateWith f =
          StreamK.toStream
        . StreamK.mergeIterateWith f (StreamK.fromStream . streamDir)
        . StreamK.fromStream

    streamDir :: Either [Path] b -> Stream IO (Either [Path] [Path])
    streamDir = either (Dir.readEitherChunks recReadOpts) (const Stream.nil)

    streamDirMaybe :: Either [Path] b -> Maybe (Stream IO (Either [Path] [Path]))
    streamDirMaybe = either (Just . Dir.readEitherChunks recReadOpts) (const Nothing)

find :: (FindOptions -> FindOptions) -> Path -> Stream IO Path
find _opts path =
        Stream.catRights

        -- Serial using unfolds
        --  $ Stream.unfoldIterateDfs unfoldDir -- 284 ms
        -- May fail with too many open files
        --  $ Stream.unfoldIterateBfs unfoldDir
        --  $ Stream.unfoldIterateBfsRev unfoldDir -- 344 ms

        -- Serial using streams
        --  $ Stream.concatIterateDfs streamDirMaybe -- 274 ms
        --  $ Stream.concatIterateBfs streamDirMaybe -- 274 ms
        --  $ Stream.concatIterateBfsRev streamDirMaybe -- 264 ms

        -- Serial using stream append and interleave
        --  $ concatIterateWith StreamK.append -- 204 ms
        --  $ mergeIterateWith StreamK.interleave  -- 304 ms

        -- Concurrent
        $ Stream.parConcatIterate id streamDir -- 174 ms
        --  $ Stream.parConcatIterate (Stream.interleaved True) streamDir -- 224 ms
        --  $ Stream.parConcatIterate (Stream.ordered True) streamDir -- 234 ms

        $ Stream.fromPure (Left path)

    where

    concatIterateWith f =
          StreamK.toStream
        . StreamK.concatIterateWith f (StreamK.fromStream . streamDir)
        . StreamK.fromStream

    mergeIterateWith f =
          StreamK.toStream
        . StreamK.mergeIterateWith f (StreamK.fromStream . streamDir)
        . StreamK.fromStream

    streamDir :: Either Path b -> Stream IO (Either Path Path)
    streamDir = either (Dir.readEitherPaths recReadOpts) (const Stream.nil)

    unfoldDir :: Unfold IO (Either Path b) (Either Path Path)
    unfoldDir = Unfold.either (Dir.eitherReaderPaths recReadOpts) Unfold.nil

    streamDirMaybe :: Either Path b -> Maybe (Stream IO (Either Path Path))
    streamDirMaybe = either (Just . Dir.readEitherPaths recReadOpts) (const Nothing)
