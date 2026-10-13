-- |
-- Module      : Streamly.Coreutils.Tail
-- Copyright   : (c) 2025 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Output the end of a file, like the GNU @tail@ command.
--
-- A regular file is read backwards from the end, so only the part of the
-- file that is output is read. A pipe or FIFO is read to the end, keeping in
-- memory only the data needed for the output.

-- TODO: we can add a tail1 or last to read the last line.
-- TODO: tail/head can be replaced with a single file reading utility which can
-- read lines or bytes in a range.
module Streamly.Coreutils.Tail
    (
      tail

    -- * Options
    , TailOptions
    , follow
    , lines
    , bytes
    , fromLine
    , fromByte
    )
where

import Control.Concurrent (threadDelay)
import Control.Monad (when)
import Data.Functor.Identity (runIdentity)
import Data.Word (Word8)
import Streamly.Data.Array (Array)
import Streamly.Data.Stream.Prelude (Stream)
import Streamly.FileSystem.Path (Path)
import Streamly.Internal.Data.Array
    (unsafeFreezeWithShrink, unsafeGetIndex, unsafeGetIndexRev)
import System.IO
    ( Handle
    , IOMode(ReadMode)
    , SeekMode(AbsoluteSeek)
    , hClose
    , hFileSize
    , hGetBuf
    , hIsSeekable
    , hSeek
    , hTell
    , openBinaryFile
    )

import qualified Streamly.Data.Array as Array
import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Data.Scanl as Scanl
import qualified Streamly.Data.Stream.Prelude as Stream
import qualified Streamly.FileSystem.Handle as Handle
import qualified Streamly.FileSystem.Path as Path
import qualified Streamly.Internal.Data.MutArray as MutArray

import Prelude hiding (tail, lines)

-- = Design notes
--
-- * The output is the bytes of the file in chunks, newlines included, like
--   the output of the @tail@ command. Splitting into lines is left to the
--   caller.
-- * 'follow' polls the file size once a second, like GNU @tail -f@ when
--   inotify is not available. On Linux streamly-fsevents (inotify) could be
--   used instead of polling.
-- * The last N lines of a regular file are found by reading blocks
--   backwards from the end and counting newlines. If the file ends with a
--   newline, that newline ends the last line and does not start a new one,
--   so we look for N + 1 newlines from the end in that case.

data TailOptions = TailOptions
    { _follow :: Bool
    , _offset :: Either Int Int -- Left = lines, Right = bytes
    , _reverse :: Bool
    }

defaultConfig :: TailOptions
defaultConfig = TailOptions
    { _follow = False
    , _offset = Left 10
    , _reverse = False -- False means default i.e. from the end
    }

-- | After the end of the file is reached, keep running and output data as it
-- is appended to the file, like @tail -f@. The file size is checked once a
-- second. If the file is truncated, output starts again from its beginning.
-- Has no effect on a pipe or FIFO, which is read until the writer closes it.
--
-- Default (without this modifier): 'False', stop at the end of the file.
follow :: Bool -> TailOptions -> TailOptions
follow opt cfg = cfg {_follow = opt}

-- Note we could have used negative offset to indicate from the end and
-- positive from the beginning. But in that case 0 becomes ambiguous, is it
-- from the end or from the beginning.

-- | Output the last @n@ lines, like @tail -n n@.
--
-- Default (without this modifier or 'bytes', 'fromLine', 'fromByte'): the
-- last 10 lines.
lines :: Int -> TailOptions -> TailOptions
lines x cfg = cfg {_offset = Left x, _reverse = False}

-- | Output the last @n@ bytes, like @tail -c n@.
bytes :: Int -> TailOptions -> TailOptions
bytes x cfg = cfg {_offset = Right x, _reverse = False}

-- | Output from line @n@ to the end, like @tail -n +n@. Lines are numbered
-- from 1.
fromLine :: Int -> TailOptions -> TailOptions
fromLine x cfg = cfg {_offset = Left x, _reverse = True}

-- | Output from byte @n@ to the end, like @tail -c +n@. Bytes are numbered
-- from 1.
fromByte :: Int -> TailOptions -> TailOptions
fromByte x cfg = cfg {_offset = Right x, _reverse = True}

-------------------------------------------------------------------------------
-- Array helpers
-------------------------------------------------------------------------------

newline :: Word8
newline = 10

countNewlines :: Array Word8 -> Int
countNewlines arr =
    runIdentity
        $ Stream.fold Fold.length
        $ Stream.filter (== newline)
        $ Array.read arr

-- | Index just after the @n@-th newline counted from the end of the array,
-- or the number of newlines still to be found before the array.
afterNewlineRev :: Int -> Array Word8 -> Either Int Int
afterNewlineRev n arr = go 0 n

    where

    len = Array.length arr

    -- i is the index counted from the end.
    go i k
        | i >= len = Left k
        | unsafeGetIndexRev i arr == newline =
            if k == 1 then Right (len - i) else go (i + 1) (k - 1)
        | otherwise = go (i + 1) k

-- | Index just after the @n@-th newline counted from the start of the array,
-- or the number of newlines still to be skipped after the array.
afterNewline :: Int -> Array Word8 -> Either Int Int
afterNewline n arr = go 0 n

    where

    len = Array.length arr

    go i k
        | i >= len = Left k
        | unsafeGetIndex i arr == newline =
            if k == 1 then Right (i + 1) else go (i + 1) (k - 1)
        | otherwise = go (i + 1) k

-- | The array from index @i@ to the end.
dropArray :: Int -> Array Word8 -> IO (Array Word8)
dropArray i arr = Stream.fold Array.create (Stream.drop i (Array.read arr))

endsWithNewline :: [Array Word8] -> Bool
endsWithNewline chunks =
    case reverse (filter ((> 0) . Array.length) chunks) of
        [] -> False
        (c : _) -> Array.getIndex (Array.length c - 1) c == Just newline

-------------------------------------------------------------------------------
-- Stream helpers
-------------------------------------------------------------------------------

-- XXX Should go in the Array stream module
-- | Drop a prefix of a stream of chunks. @split k arr@ is @Right i@ when the
-- remaining prefix of size @k@ ends at index @i@ of @arr@, or @Left k1@ when
-- @k1@ of the prefix is still to be dropped after @arr@.
{-# INLINE dropPrefix #-}
dropPrefix ::
       (Int -> Array Word8 -> Either Int Int)
    -> Int
    -> Stream IO (Array Word8)
    -> Stream IO (Array Word8)
dropPrefix split n s
    | n <= 0 = s
    | otherwise =
        Stream.mapMaybeM emit
            $ Stream.postscanl (Scanl.mkScanl step (n, Nothing)) s

    where

    -- The state is the prefix size still to be dropped and the chunk to
    -- emit with the index it starts from.
    step (k, _) arr
        | k <= 0 = (0, Just (0, arr))
        | otherwise =
            case split k arr of
                Right i -> (0, Just (i, arr))
                Left k1 -> (k1, Nothing)

    emit (_, Nothing) = pure Nothing
    emit (_, Just (0, arr)) = pure (Just arr)
    emit (_, Just (i, arr))
        | i >= Array.length arr = pure Nothing
        | otherwise = Just <$> dropArray i arr

-- XXX Should go in the Array stream module
-- | Drop the first @n@ lines.
{-# INLINE dropLines #-}
dropLines :: Int -> Stream IO (Array Word8) -> Stream IO (Array Word8)
dropLines = dropPrefix afterNewline

-- XXX Should go in the Array stream module
-- | Drop the first @n@ bytes.
{-# INLINE dropBytes #-}
dropBytes :: Int -> Stream IO (Array Word8) -> Stream IO (Array Word8)
dropBytes = dropPrefix split

    where

    split k arr =
        let len = Array.length arr
         in if k < len then Right k else Left (k - len)

-------------------------------------------------------------------------------
-- Non-seekable input
-------------------------------------------------------------------------------

-- A FIFO queue of chunks, each with its measure (newlines or bytes), and the
-- total measure. Chunks are pushed at the back and dropped from the front.
data Queue = Queue [(Array Word8, Int)] [(Array Word8, Int)] !Int

emptyQueue :: Queue
emptyQueue = Queue [] [] 0

-- | Add a chunk, then drop chunks from the front as long as the rest still
-- measure at least @need@.
pushQueue :: (Array Word8 -> Int) -> Int -> Queue -> Array Word8 -> Queue
pushQueue measure need (Queue front back total) arr =
    trim (Queue front ((arr, m) : back) (total + m))

    where

    m = measure arr

    trim q@(Queue [] [] _) = q
    trim (Queue [] b t) = trim (Queue (reverse b) [] t)
    trim q@(Queue ((_, m1) : f) b t)
        | t - m1 >= need = trim (Queue f b (t - m1))
        | otherwise = q

queueChunks :: Queue -> [Array Word8]
queueChunks (Queue front back _) = map fst (front ++ reverse back)

queueTotal :: Queue -> Int
queueTotal (Queue _ _ t) = t

-- XXX Should go in the Array stream module
-- | Last @n@ lines of a list of chunks.
lastLinesOf :: Int -> [Array Word8] -> IO [Array Word8]
lastLinesOf n chunks
    | n <= 0 = pure []
    | otherwise = go k [] (reverse chunks)

    where

    k = if endsWithNewline chunks then n + 1 else n

    go _ acc [] = pure acc
    go j acc (c : cs) =
        case afterNewlineRev j c of
            Right i -> do
                c1 <- dropArray i c
                pure (c1 : acc)
            Left j1 -> go j1 (c : acc) cs

-- XXX Should go in the Array stream module
-- | Output the last lines or bytes of a stream that cannot be seeked.
{-# INLINE tailStream #-}
tailStream ::
    Either Int Int -> Stream IO (Array Word8) -> Stream IO (Array Word8)
tailStream offset s = Stream.concatEffect $
    case offset of
        Left n -> do
            -- n + 1 newlines cover a final newline that ends the last line.
            q <- lastChunks countNewlines (n + 1)
            Stream.fromList <$> lastLinesOf n (queueChunks q)
        Right n -> do
            q <- lastChunks Array.length n
            pure
                $ dropBytes (queueTotal q - max 0 n)
                $ Stream.fromList (queueChunks q)

    where

    lastChunks measure need =
        Stream.fold (Fold.foldl' (pushQueue measure need) emptyQueue) s

-------------------------------------------------------------------------------
-- Seekable input
-------------------------------------------------------------------------------

blockSize :: Integer
blockSize = 64 * 1024

-- XXX Use Handle.getChunkOf when it is implemented in streamly-core.
readBlock :: Handle -> Integer -> Int -> IO (Array Word8)
readBlock h off len = do
    hSeek h AbsoluteSeek off
    arr <- MutArray.unsafeCreateWithPtr' len $ \p -> hGetBuf h p len
    pure $ unsafeFreezeWithShrink arr

-- XXX Should go in the Array stream module
-- | Offset where the last @n@ lines of the file start.
lastLinesOffset :: Handle -> Int -> IO Integer
lastLinesOffset h n = do
    size <- hFileSize h
    if size == 0 || n <= 0
    then pure size
    else do
        lastByte <- readBlock h (size - 1) 1
        let k = if Array.getIndex 0 lastByte == Just newline then n + 1 else n
        go k size

    where

    go k pos
        | pos <= 0 = pure 0
        | otherwise = do
            let len = min blockSize pos
                off = pos - len
            arr <- readBlock h off (fromIntegral len)
            case afterNewlineRev k arr of
                Right i -> pure (off + fromIntegral i)
                Left k1 -> go k1 off

-- | Output the requested part of a seekable file.
{-# INLINE tailSeekable #-}
tailSeekable :: TailOptions -> Handle -> IO (Stream IO (Array Word8))
tailSeekable cfg h =
    case (_reverse cfg, _offset cfg) of
        (False, Left n) -> lastLinesOffset h n >>= fromOffset
        (False, Right n) -> do
            size <- hFileSize h
            fromOffset (max 0 (size - fromIntegral (max 0 n)))
        (True, Left n) -> do
            hSeek h AbsoluteSeek 0
            pure $ dropLines (n - 1) (Handle.readChunks h)
        (True, Right n) -> do
            size <- hFileSize h
            fromOffset (min size (fromIntegral (max 0 (n - 1))))

    where

    fromOffset off = do
        hSeek h AbsoluteSeek off
        pure (Handle.readChunks h)

-- | Poll for data appended to the file, forever.
{-# INLINE followHandle #-}
followHandle :: Handle -> Stream IO (Array Word8)
followHandle h =
    Stream.concatMap (const (Handle.readChunks h)) (Stream.repeatM poll)

    where

    -- XXX use streamly-fsevents to watch the file, will need FreeBSD support
    -- in streamly-fsevents but we can still do it on Linux/macOS/Windows.
    poll = do
        threadDelay 1000000
        pos <- hTell h
        size <- hFileSize h
        when (size < pos) $ hSeek h AbsoluteSeek 0

-------------------------------------------------------------------------------
-- API
-------------------------------------------------------------------------------

-- | Output the end of a file as a stream of byte arrays, like the @tail@
-- command. Pass 'id' for the defaults; each modifier documents its own
-- default.
--
-- >> tail id path                -- tail path
-- >> tail (lines 20) path        -- tail -n 20 path
-- >> tail (fromLine 2) path      -- tail -n +2 path
-- >> tail (bytes 100) path       -- tail -c 100 path
-- >> tail (follow True) path     -- tail -f path
tail :: (TailOptions -> TailOptions) -> Path -> Stream IO (Array Word8)
tail modifier path =
    Stream.bracketIO
        (openBinaryFile (Path.toString path) ReadMode)
        hClose
        run

    where

    cfg = modifier defaultConfig

    run h = Stream.concatEffect $ do
        seekable <- hIsSeekable h
        if seekable
        then do
            s <- tailSeekable cfg h
            pure
                $ if _follow cfg
                  then Stream.append s (followHandle h)
                  else s
        else
            pure
                $ let s = Handle.readChunks h
                   in case (_reverse cfg, _offset cfg) of
                        (False, offset) -> tailStream offset s
                        (True, Left n) -> dropLines (n - 1) s
                        (True, Right n) -> dropBytes (n - 1) s
