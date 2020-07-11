module Streamly.Coreutils.Uniq (
        UniqOptions (..)
      , defaultUniqOptions
      , getRepetition
      , compareUsingOptions
      , onlyUnique
      , onlyDuplicate
      , onlyRepeated
      , uniq
      , uniqCount
   )
where
import qualified Streamly.Prelude as S
import qualified Streamly.Internal.Data.Fold as FL

import Data.Char (toLower, isSpace)
import Streamly.Internal.Data.Stream.StreamK.Type (IsStream)


-- |
-- Record to specify command line options like count,
-- skip-fields, check-chars used in GNU uniq
--
-- @since 0.1.0.0
data UniqOptions = UniqOptions {
                       count :: Bool
-- ^ generate a stream of (Int, String) where
-- the integer is the number of occurrences of the corresponding string
                     , duplicate :: Bool
-- ^ print only duplicate lines (which occur more than once)
                     , repeated :: Bool
-- ^ display only duplicate lines, once for each group
                     , skipFields :: Int
-- ^ skips first skipFields number of non-space strings
                     , skipChar :: Int
-- ^ skips first skipChar number of characters (after skipFields)
                     , checkChar :: Maybe Int
-- ^ take first checkChar number of characters for comparison (after skipChar)
-- If Nothing, takes the entire string
                     , ignoreCase :: Bool
-- ^ ignore case while comparison
                     , unique :: Bool
-- ^ print only unique lines (which occur exactly once)
                  }

-- |
-- Default options for uniq
--
-- @since 0.1.0.0
{-# INLINE defaultUniqOptions #-}

defaultUniqOptions :: UniqOptions
defaultUniqOptions = UniqOptions True False False 0 0 Nothing False True


-------------------------------------------------------------------------------
-- helper functions for uniq
-------------------------------------------------------------------------------


-- |
-- @slice n m str@ drops first @n@ characters
-- and then takes first @m@ characters from the string @str@
--
-- @since 0.1.0.0
{-# INLINE slice #-}

slice :: Int -> Int -> String -> String
slice off len = take len . drop off



-- |
-- Compares two strings ignoring case
--
-- @since 0.1.0.0
compareIgnCase :: String -> String -> Bool
compareIgnCase str1 str2 | length str1 /= length str2 = False
compareIgnCase (x1:xs) (y1:ys) =
    (toLower x1 == toLower y1) && compareIgnCase xs ys
compareIgnCase [] [] = True



-- |
-- Produces a comparison function of type @String -> String -> Bool@
-- using the options specified
--
-- @since 0.1.0.0
compareUsingOptions :: UniqOptions -> (String -> String -> Bool)
compareUsingOptions opt = compareXYZ (ignoreCase opt) (skipFields opt) (skipChar opt) (checkChar opt)
   where
      -- ^
      -- @compareXYZ ign x y z s1 s2@ does the following in order:
      -- skips first @x@ non   -space fields,
      -- skips first @y@ characters and takes first @z@ characters
      -- from the strings @s1@ and @s2@ and returns True only if they are equal
      -- after the above operations. Ignores case if
      -- @ign@ case is True
      --
      -- @since 0.1.0.0
      compareXYZ :: Bool -> Int -> Int -> Maybe Int -> String -> String -> Bool
      compareXYZ ignCase x y mz str1 str2 =
         let
            ix1 = indexAfterSkippingNWords x str1
            ix2 = indexAfterSkippingNWords x str2
            compareFunc =
               if ignCase
               then compareIgnCase
               else (==)
            compareWith :: (String -> String -> Bool) -> String -> String -> Bool
            compareWith eq s1 s2 = eq s1 s2
            -- ^
            -- Gives the number of characters to be deleted
            -- from the beginning of the string to skip @n@
            -- non-space fields\/strings
            --
            -- @since 0.1.0.0
            indexAfterSkippingNWords :: Int -> String -> Int
            indexAfterSkippingNWords n str = go 0 n
               where
                  len = length str
                  go i 0 = i
                  go i _ | i >= (len - 1) = len
                  go i xx = if isChar (str !! i) && isSpace (str !! (i + 1))
                           then go (i + 1) (xx - 1)
                           else go (i + 1) xx
                  isChar = not . isSpace
         in
            case mz of
               Just z -> compareWith
                         compareFunc
                         (slice (ix1 + y) z str1)
                         (slice (ix2 + y) z str2)
               _  -> compareWith
                     compareFunc
                     (drop (ix1 + y) str1)
                     (drop (ix2 + y) str2)


-- |
-- Applies the comparison function to adjacent strings in the stream
-- and returns a @(Int, String)@ stream where the integer
-- is the count of occurrences of the string
--
-- @since 0.1.0.0
getRepetition :: (IsStream t, Monad m) => (String -> String -> Bool) -> t m String -> t m (Int, String)
getRepetition comparator = S.groupsBy comparator
                              (FL.mkPureId (\(i, a) s -> if a == ""
                                                         then (i + 1, s)
                                                         else (i + 1, a)) (0, ""))


-- |
-- Processes the input stream from the beginning and
-- creates a @String@ stream as output,
-- inserting a string into the stream
-- once if its occurrence is exactly 1
--
-- @since 0.1.0.0
onlyUnique :: (IsStream t, Monad m) => (String -> String -> Bool) -> t m String -> t m String
onlyUnique comparator strm = S.map snd
   $ S.filter (\(i, _) -> i == 1)
   $ getRepetition comparator strm


-- |
-- Similar to @onlyUnique@ but inserts the string only
-- when the number of its occurrences is strictly greater than 1.
-- Inserts the string into the stream
-- equal to the number of times it occurs
--
-- @since 0.1.0.0
onlyDuplicate :: (IsStream t, Monad m) => (String -> String -> Bool) -> t m String -> t m String
onlyDuplicate comparator = S.concatMap (\(i, x) -> if i > 1
                                                   then S.replicate i x
                                                   else S.nil) . getRepetition comparator


-- |
-- Similar to @onlyDuplicate@ but inserts the string
-- in the output stream only once
--
-- @since 0.1.0.0
onlyRepeated :: (IsStream t, Monad m) => (String -> String -> Bool) -> t m String -> t m String
onlyRepeated comparator strm = S.map snd
   $ S.filter (\(i, _) -> i > 1)
   $ getRepetition comparator strm



-- |
-- Exactly one of @unique@, @duplicate@ and @repeated@ should be True
-- as the output stream can contain unique, duplicate (multiple repetitions)
-- or repeated (once for multiple repetitions) strings\/@(Int, String)@
-- tuples at a time
--
-- @since 0.1.0.0
validateOptions :: UniqOptions -> Bool
validateOptions opt =
    if u == True && not (d || r)
    then True
    else if d == True && not (r || u)
    then True
    else if r == True && not (u || d)
    then True
    else False

    where

    u = unique opt
    d = duplicate opt
    r = repeated opt


-- |
-- Generates a @String@ stream
-- applying uniq on the input stream
-- according to the the @UniqOptions@ specified
--
-- @since 0.1.0.0
uniq :: (IsStream t, Monad m) => UniqOptions -> t m String -> t m String
uniq opt strm = if validateOptions opt == False
                then S.nil
                else if (unique opt)
                then onlyUnique (compareUsingOptions opt) strm
                else if (duplicate opt)
                then onlyDuplicate (compareUsingOptions opt) strm
                else onlyRepeated (compareUsingOptions opt) strm


-- |
-- Generates a @(Int, String)@ stream
-- where each tuple @(n, str)@ represents that the string @str@
-- occurred @n@ times consecutively.
-- String comparison is performed using options provided
--
-- @since 0.1.0.0
uniqCount :: (IsStream t, Monad m) => UniqOptions -> t m String -> t m (Int, String)
uniqCount opt strm = if validateOptions opt == False
                     then S.nil
                     else if (unique opt)
                     then S.filter (\(i, _) -> i == 1) $ getRepetition (compareUsingOptions opt) strm
                     else S.filter (\(i, _) -> i >= 1) $ getRepetition (compareUsingOptions opt) strm
