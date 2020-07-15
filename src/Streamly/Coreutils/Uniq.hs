module Streamly.Coreutils.Uniq (
        Output (..)
      , UniqResult (..)
      , UniqOptions (..)
      , defaultUniqOptions
      , getRepetition
      , compareUsingOptions
      , uniq
      , uniqResultToString
   )
where
import qualified Streamly.Prelude as S
import qualified Streamly.Internal.Data.Fold as FL

import Data.Char (toLower, isSpace)
import Streamly.Internal.Data.Stream.StreamK.Type (IsStream)


-- |
-- Data type to capture the output of the stream -
-- the stream should either be composed of
-- unique, repeated, duplicate or all of the strings
--
-- @since 0.1.0.0
data Output = Unique
            | Repeated
            | Duplicate
            | All
            -- ^ Returns @UniqResult@ without filtering
            -- any strings based on counts


-- |
-- Data type of uniq's output.
-- @Count n s@ represents that the string @s@ occurs @n@
-- times in the output stream.
--
-- @since 0.1.0.0
data UniqResult = Count Int String


-- |
-- Show instance for @UniqResult@
--
-- @since 0.1.0.0
instance Show UniqResult where
    show (Count n s) = show n ++ " " ++ s


-- |
-- Record to specify command line options like count,
-- skip-fields, check-chars used in GNU uniq
--
-- @since 0.1.0.0
data UniqOptions = UniqOptions {
                       skipFields :: Int
-- ^ skips first skipFields number of non-space strings
                     , skipChar :: Int
-- ^ skips first skipChar number of characters (after skipFields)
                     , checkChar :: Maybe Int
-- ^ take first checkChar number of characters for comparison (after skipChar)
-- If Nothing, takes the entire string
                     , ignoreCase :: Bool
-- ^ ignore case while comparison
                     , output :: Output
-- ^ whether the output stream should be of unique, repeated or duplicate strings
                  }

-- |
-- Default options for uniq
--
-- @since 0.1.0.0
{-# INLINE defaultUniqOptions #-}

defaultUniqOptions :: UniqOptions
defaultUniqOptions = UniqOptions 0 0 Nothing False All


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
getRepetition :: (IsStream t, Monad m) => (String -> String -> Bool) -> t m String -> t m UniqResult
getRepetition comparator = S.groupsBy comparator
                              (FL.mkPureId (\(Count i a) s ->
                                  if a == ""
                                  then Count (i + 1) s
                                  else Count (i + 1) a) (Count 0 ""))


-- |
-- Generates a @UniqResult@ stream
-- applying uniq on the input stream
-- according to the the @UniqOptions@ specified.
-- When the output is @Repeated@, the @Int@ values in
-- @Count Int String@ are set to 1.
--
-- @since 0.1.0.0
uniq :: (IsStream t, Monad m) => UniqOptions -> t m String -> t m UniqResult
uniq opt strm =
    let
        eq n (Count i _) = i == n
        ge n (Count i _) = i > n
        extract predicate = S.filter predicate
            $ getRepetition (compareUsingOptions opt) strm
    in
        case (output opt) of
            Unique -> extract (eq 1)
            All -> extract (\_ -> True)
            Duplicate -> extract (ge 1)
            Repeated -> S.map (\(Count _ x) -> Count 1 x) $ extract (ge 1)


-- |
-- Converts a @UniqResult@ stream to a @String@ stream by
-- For each @Count n s@ in the input stream, it
-- concatenates the string @s n@ times in the output stream
--
-- @since 0.1.0.0
uniqResultToString :: (IsStream t, Monad m) => t m UniqResult -> t m String
uniqResultToString = S.concatMap (\(Count i x) -> S.replicate i x)
