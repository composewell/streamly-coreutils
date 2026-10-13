-- |
-- Module      : Streamly.Coreutils.Cut
-- Copyright   : (c) 2025 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Select fields from each line of a file, like GNU @cut -f@.
--
-- The fields to select are given with 'fields' and 'fieldsFrom'. Fields are
-- numbered from 1, as in GNU @cut@.
--
-- >>> _ = cut (fields [1, 3])                       -- cut -f 1,3
-- >>> _ = cut (fields [1 .. 3] . fieldsFrom 7)      -- cut -f 1-3,7-
-- >>> _ = cut (fields [2] . delimiter ',')          -- cut -d , -f 2
-- >>> _ = cut (fields [2] . onlyDelimited True)     -- cut -s -f 2
--
-- The input is treated as UTF-8 and the delimiter can be any character,
-- including a multi-byte UTF-8 character, which GNU @cut@ does not accept.
--
-- Not supported: @-b@ (bytes), @-c@ (characters), @--complement@,
-- @--output-delimiter@, @-z@ (NUL line terminator), and reading standard
-- input.

module Streamly.Coreutils.Cut
    (
      cut
    , cutFields

    -- * Options
    , CutOptions
    , fields
    , fieldsFrom
    , delimiter
    , onlyDelimited

    -- * Deprecated
    , foldIndicesBy
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
import Data.Maybe (isNothing)
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

-- = Design notes
--
-- * The field list is an option, not a positional argument, so that @-b@
--   and @-c@ can later be added as more options of the same kind. A missing
--   list, or more than one kind of list, is then a runtime error, raised
--   before the file is opened, with the GNU message. A typed argument
--   (a semigroup of ranges inside a fields / bytes / characters selection)
--   was rejected: it moves only these errors to compile time, while a field
--   number of 0 stays a runtime error anyway.
--
-- * 'fields' and 'fieldsFrom' accumulate (the selection is the union of all
--   of them), unlike other modifiers, which set a value. GNU rejects a
--   second @-f@; accumulating is a superset of that.
--
-- * Open-ended ranges are a separate modifier, 'fieldsFrom', and not an
--   infinite list: the list is sorted to output fields in input order, as
--   GNU does, and an infinite list cannot be sorted or validated.
--
-- * The delimiter is a 'Char', as for @cut -d@, and is always encoded as
--   UTF-8, whatever the locale. Lines are split on the encoded bytes,
--   without decoding the line: in UTF-8 the bytes of a whole character match
--   only at a character boundary, and invalid UTF-8 in the input is output
--   unchanged. GNU @cut@ accepts only a single byte delimiter; the
--   distribution "i18n" patch (Fedora, SUSE) accepts a multi-byte one.
--   Lost: splitting a non-UTF-8 file on a raw byte above 127, which a
--   @Word8@ delimiter allowed. A separate byte delimiter modifier can be
--   added if that is needed.
--
-- * 'foldIndicesBy' numbers fields from 0. It is deprecated, not changed,
--   so that existing callers do not silently select different fields.

-- XXX should go in Unicode.Array.decodeLatin1?
decodeLatin1 :: Array Word8 -> [Char]
decodeLatin1 arr =
    Array.read arr
        & Unicode.decodeLatin1
        & Stream.toList
        & runIdentity

-- XXX this should go in unicode text file reading module - Unicode.FileIO or
-- cat coreutil? Redirection ">" can have its own "pipeTo" or "redirectTo"
-- function.
-- e.g. readUtf8, writeUtf8, readLines, readWords, readWordsBy, readDSV,
-- readDSVSeq

-- | Return a stream of lines in a file.
--
-- A file ending in a newline yields an empty last line.
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

-- | @foldIndicesBy fold indices separator file@. Split the lines in a file
-- using the given separator and then fold the selected indices (indices start
-- from 0) in each line using the supplied fold.
--
-- For example to generate an array of the selected indices for each line:
--
-- >> selectIndicesBy = foldIndicesBy GArray.create
--
foldIndicesBy :: (MonadIO m, MonadCatch m) =>
    Fold m (Array Word8) b -> [Int] -> Word8 -> Path.Path -> Stream m b
foldIndicesBy f xs w =
    Stream.mapM (Stream.fold f . filterIndices xs . splitByWord8 w) . readLines
{-# DEPRECATED foldIndicesBy
    "Use cut or cutFields. Note that they number fields from 1." #-}

-------------------------------------------------------------------------------
-- cut
-------------------------------------------------------------------------------

-- | Options for 'cut' and 'cutFields'.
data CutOptions = CutOptions
    { cutFieldList :: [Int]
    , cutFieldsFrom :: Maybe Int
    , cutDelimiter :: Char
    , cutOnlyDelimited :: Bool
    }

defaultConfig :: CutOptions
defaultConfig =
    CutOptions
        { cutFieldList = []
        , cutFieldsFrom = Nothing
        , cutDelimiter = '\t'
        , cutOnlyDelimited = False
        }

-- | Select the given fields, numbered from 1, like @cut -f 1,3@. Use
-- @[m .. n]@ for the range @m-n@. The list must be finite; use 'fieldsFrom'
-- for an open-ended range.
--
-- Fields are output in the order they occur in the line, each at most once,
-- whatever the order of the list, like GNU @cut@.
--
-- Unlike other modifiers, 'fields' and 'fieldsFrom' accumulate: the
-- selection is the union of all of them, so @fields [1] . fields [3]@
-- selects fields 1 and 3.
--
-- Default (without this modifier and 'fieldsFrom'): no fields are selected,
-- and 'cut' raises an exception, like GNU @cut@ without @-f@.
fields :: [Int] -> CutOptions -> CutOptions
fields xs cfg = cfg {cutFieldList = xs ++ cutFieldList cfg}

-- | Select field @n@ and all fields after it, like @cut -f n-@. Accumulates
-- with 'fields' and other 'fieldsFrom' modifiers.
--
-- Default (without this modifier): no open-ended range is selected.
fieldsFrom :: Int -> CutOptions -> CutOptions
fieldsFrom n cfg = cfg {cutFieldsFrom = Just (maybe n (min n) (cutFieldsFrom cfg))}

-- | Use the given character as the field delimiter, like @cut -d@. The
-- delimiter is encoded as UTF-8, and can be a multi-byte character, which
-- GNU @cut@ does not accept.
--
-- Default (without this modifier): TAB, like GNU @cut@.
delimiter :: Char -> CutOptions -> CutOptions
delimiter d cfg = cfg {cutDelimiter = d}

-- | When 'True', drop the lines that do not contain the delimiter, like
-- @cut -s@.
--
-- Default (without this modifier): 'False'; a line without the delimiter is
-- output unchanged, whatever fields are selected, like GNU @cut@.
onlyDelimited :: Bool -> CutOptions -> CutOptions
onlyDelimited x cfg = cfg {cutOnlyDelimited = x}

-- | Raise the GNU @cut@ error for invalid options.
validateOptions :: CutOptions -> IO ()
validateOptions opts
    | null (cutFieldList opts) && isNothing (cutFieldsFrom opts) =
        failWith "you must specify a list of bytes, characters, or fields"
    | any (< 1) (cutFieldList opts) || maybe False (< 1) (cutFieldsFrom opts) =
        failWith "fields are numbered from 1"
    | otherwise = return ()

    where

    failWith msg = ioError $ userError $ "cut: " ++ msg

-- | Selection mask for fields numbered from 1: element @i@ is True if field
-- @i + 1@ is selected. Infinite when 'fieldsFrom' is set.
selectionMask :: CutOptions -> [Bool]
selectionMask opts =
    case cutFieldsFrom opts of
        Nothing -> listMask
        Just n ->
            zipWith (||) (listMask ++ repeat False) (map (>= n) [1 ..])

    where

    listMask = makeIndexMask (map (subtract 1) (cutFieldList opts))

-- | The UTF-8 encoding of a character.
encodeChar :: Char -> Array Word8
encodeChar = Array.fromPureStream . Stream.unfold Unicode.readCharUtf8

-- | Split a line into fields on the encoded delimiter. A line with @n@
-- delimiters has @n + 1@ fields; an empty line has one empty field.
splitLine :: Array Word8 -> Array Word8 -> IO [Array Word8]
splitLine delim line
    | Array.length line == 0 = return [line]
    | otherwise =
        case Array.toList delim of
            [d] -> do
                -- splitEndBy_ drops the empty field after a final delimiter
                flds <- Stream.toList $ Array.splitEndBy_ (== d) line
                let lastByte = Array.getIndex (Array.length line - 1) line
                return
                    $ if lastByte == Just d
                      then flds ++ [Array.empty]
                      else flds
            _ ->
                -- XXX This copies each field into a new array. We need
                -- Stream.indexEndBySeq_ / Stream.indexSepBySeq_ to yield
                -- the (offset, length) of each field, and corresponding
                -- Array operations that slice the line array using these
                -- indices, as Array.splitEndBy_ does with indexEndBy_.
                -- Another way to find the delimiter faster: search for the
                -- rarest byte of its UTF-8 encoding, and accept a match
                -- only where the adjacent bytes are the other bytes of the
                -- encoding.
                Stream.toList
                    $ Stream.splitSepBySeq_ delim Array.create
                    $ Array.read line

-- | The selected fields of a line, or 'Nothing' if the line is dropped. A
-- line without the delimiter is returned whole, as a single field.
cutLine ::
    CutOptions -> Array Word8 -> [Bool] -> Array Word8
    -> IO (Maybe [Array Word8])
cutLine opts delim mask line = do
    flds <- splitLine delim line
    return $ case flds of
        [_] | cutOnlyDelimited opts -> Nothing
            | otherwise -> Just [line]
        _ -> Just [f | (True, f) <- zip mask flds]

-- | Like 'cut' but returns the selected fields of each line as a list,
-- instead of joining them with the delimiter. A line without the delimiter
-- gives a single element list holding the whole line.
--
-- >> cutFields (fields [3, 1] . delimiter ',') path  -- line "a,b,c"
-- >> ["a","c"]
--
{-# INLINE cutFields #-}
cutFields ::
    (CutOptions -> CutOptions) -> Path.Path -> Stream IO [Array Word8]
cutFields f path =
    Stream.before (validateOptions opts)
        $ Stream.mapMaybeM (cutLine opts delim mask)
        $ Array.compactEndByLn_
        $ File.readChunks path

    where

    opts = f defaultConfig
    delim = encodeChar (cutDelimiter opts)
    mask = selectionMask opts

-- | Select fields from each line of a file, like GNU @cut -f@. Each output
-- element is one line, without the newline: the selected fields joined by
-- the delimiter.
--
-- Pass the field selection ('fields', 'fieldsFrom') and other modifiers
-- composed with @(.)@; each modifier documents its own default.
--
-- >> cut (fields [3, 1] . delimiter ',') path  -- line "a,b,c"
-- >> "a,c"
--
-- Raises an 'IOError' when the stream is run, before the file is opened, if
-- no fields are selected or a field number is less than 1, with the GNU
-- @cut@ message. Raises an 'IOError' if the file cannot be read.
cut :: (CutOptions -> CutOptions) -> Path.Path -> Stream IO (Array Word8)
cut f path =
    Stream.mapM join (cutFields f path)

    where

    delim = encodeChar (cutDelimiter (f defaultConfig))

    join flds =
        Stream.fold (Array.createOf (joinedLength flds))
            $ Array.concat
            $ Stream.intersperse delim
            $ Stream.fromList flds

    joinedLength flds =
        sum (map Array.length flds)
            + max 0 (length flds - 1) * Array.length delim
