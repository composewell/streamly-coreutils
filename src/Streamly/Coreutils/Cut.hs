-- |
-- Module      : Streamly.Coreutils.Cut
-- Copyright   : (c) 2025 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Functionality equivalent to the @cut@ command.

module Streamly.Coreutils.Cut
    ( foldIndicesBy
    , decodeLatin1
    , readLines
    , splitByWord8
    )
where

import qualified Data.List as List
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Catch (MonadCatch)
import Data.Functor.Identity (runIdentity)
import Data.Function ((&))
-- import Data.Char (ord, isSpace, chr)
import Data.Word (Word8)
import Streamly.Data.Stream (Stream)
import Streamly.Data.Fold (Fold)
import Streamly.Data.Array (Array)

import qualified  Streamly.Internal.Data.Array as Array
import qualified  Streamly.Internal.Data.Stream as Stream
import qualified  Streamly.Internal.Unicode.Stream as Unicode
import qualified Streamly.FileSystem.FileIO as File
import qualified Streamly.FileSystem.Path as Path

-- XXX should go in Unicode.Array.decodeLatin1?
decodeLatin1 :: Array Word8 -> [Char]
decodeLatin1 arr =
    Array.read arr
        & Unicode.decodeLatin1
        & Stream.toList
        & runIdentity

-- XXX this should go in unicode text file reading module - Unicode.FileIO?
-- e.g. readUtf8, writeUtf8, readLines, readWords, readWordsBy, readDSV,
-- readDSVSeq

-- | Return a stream of lines in a file.
readLines :: (MonadIO m, MonadCatch m) => Path.Path -> Stream m (Array Word8)
readLines p =
    File.readChunks p                 -- Stream IO (Array Word8)
        & Array.compactSepByByte_ 10  -- Stream IO (Array Word8)

-- Move these to Unicode.Array

-- | Split a line on a given separator field.
--
-- >> splitByWord8 x = Array.splitEndBy_ (== x)
--
-- For example, to generate arrays of all fields on each line of a file:
--
-- >> readFieldsBy w = Stream.mapM (Stream.fold GArray.create . splitByWord8 w) . readLines
--
splitByWord8 :: Monad m => Word8 -> Array Word8 -> Stream m (Array Word8)
-- XXX use Array.splitSepBy_ to get proper number of fields
splitByWord8 x = Array.splitEndBy_ (== x)

{-
-- | Unsafe if the supplied char is > 255
splitByLatin1 :: Monad m => Char -> Array Word8 -> Stream m (Array Word8)
splitByLatin1 x = splitByWord8 (fromIntegral (ord x))

-- splitCSV, splitDSV -- escape quoted comma, delimiter
splitByComma :: Monad m => Array Word8 -> Stream m (Array Word8)
splitByComma = splitByLatin1 ','

-- Strip ascii white space at the beginning and end
stripLatin1 :: Array Word8 -> Array Word8
stripLatin1 = Array.dropAround (isSpace . chr. fromIntegral)
-}

-- XXX Move to Data.Stream.

-- | Convert an input list of indices (minimum 0) to an output list of
-- booleans such that for each integer index present in the input list the
-- corresponding index in the output list is True and all other indices are
-- False. The size of the output list is determined by the max index in the
-- input list, which is the last index in the output list and has a True entry.
--
-- Negative integers in the input list are ignored.
makeIndexMask :: [Int] -> [Bool]
makeIndexMask indices =
    -- Sort would be O(n) if pre-sorted
    let xs1 = List.nub $ List.sort $ filter (>= 0) indices
     in go 0 xs1 id

    where

    go _ [] ys = ys []
    go i old@(x:xs) ys =
        if i == x
        then go (i + 1) xs (ys . (True :))
        else go (i + 1) old (ys . (False :))

-- | Zip the input stream with the supplied mask and keep only those elements
-- which are True in the mask stream.
filterByMask :: Monad m =>
    Stream m Bool -> Stream m a -> Stream m a
filterByMask xs =
    Stream.catMaybes
        . Stream.zipWith (\a b -> if a then Just b else Nothing) xs

-- | Assuming a stream having elements from 0 to n. Keep only those indices
-- which are present in the supplied list.
--
-- Performs better if the supplied indices are sorted in ascending order, or
-- even better use filterByMask instead.
filterIndices :: Monad m =>
    [Int] -> Stream m a -> Stream m a
filterIndices xs = filterByMask (Stream.fromList (makeIndexMask xs))

-- | Split the lines in a file using the given separator and then fold the
-- selected indices (indices start from 0) in each line using the supplied
-- fold.
--
-- For example to generate an array of the selected indices for each line:
--
-- >> selectIndicesBy = foldIndicesBy GArray.create
--
foldIndicesBy :: (MonadIO m, MonadCatch m) =>
    Fold m (Array Word8) b -> [Int] -> Word8 -> Path.Path -> Stream m b
foldIndicesBy f xs w =
    Stream.mapM (Stream.fold f . filterIndices xs . splitByWord8 w) . readLines
