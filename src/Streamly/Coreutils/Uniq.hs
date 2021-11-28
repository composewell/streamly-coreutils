module Streamly.Coreutils.Uniq
    ( Output(..)
    , UniqResult(..)
    , UniqOptions(..)
    , defaultUniqOptions
    , compareUsingOptions
    , getRepetition
    , uniq
    , uniqResultToString
    ) where

import Data.Char (isSpace, toLower)
import Streamly.Prelude (IsStream)

import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Prelude as Stream

-- | Data type to capture the output of the stream - the stream should either be
-- composed of unique, repeated, duplicate or all of the strings
--
-- @since 0.1.0.0
data Output
    = Unique
    -- ^
    -- Allows strings with count exactly equal to 1
    | Duplicate
    -- ^
    -- Allows strings with count strictly greater than 1
    | Repeated
    -- ^
    -- Similar to @Duplicate@ but
    -- in the output stream, only one string is present
    -- for a group of duplicate strings
    | All
    -- ^
    -- Allows all strings without filtering on the basis of counts

-- |
-- Data type of uniq's output
--
-- @since 0.1.0.0
data UniqResult = UniqResult Int String
                  -- ^
                  -- @UniqResult n s@ represents that
                  -- the string @s@ occurs @n@ times consecutively

-- |
-- Show instance for @UniqResult@
--
-- @since 0.1.0.0
instance Show UniqResult where
    show (UniqResult n s) = show n ++ " " ++ s


-- | Record to specify command line options like count, skip-fields, check-chars
-- used in GNU uniq
--
-- @since 0.1.0.0
data UniqOptions = UniqOptions
    { skipFields :: Int
    -- ^ skips first skipFields number of non-space strings
    , skipChar :: Int
    -- ^ skips first skipChar number of characters (after skipFields)
    , checkChar :: Maybe Int
    -- ^ take first checkChar number of characters for comparison (after
    -- skipChar).  If Nothing, takes the entire string
    , ignoreCase :: Bool
    -- ^ ignore case while comparison
    , output :: Output
    -- ^ whether the output stream should be of unique, repeated,
    -- duplicate or all of the strings
    }

-- | Default options for uniq are skipping zero non-space strings and
-- characters, taking all strings for comparison, case sensitive (@ignoreCase@
-- is set to @False@) and outputs all of the strings
--
-- @since 0.1.0.0
{-# INLINE defaultUniqOptions #-}
defaultUniqOptions :: UniqOptions
defaultUniqOptions = UniqOptions 0 0 Nothing False All

-------------------------------------------------------------------------------
-- Helper functions for uniq
-------------------------------------------------------------------------------

-- | @slice n m str@ drops first @n@ characters and then takes first @m@
-- characters from the string @str@
--
-- @since 0.1.0.0
{-# INLINE slice #-}
slice :: Int -> Int -> String -> String
slice off len = take len . drop off

-- |
-- Compares two strings ignoring case
--
-- @since 0.1.0.0
{-# INLINE compareIgnCase #-}
compareIgnCase :: String -> String -> Bool
compareIgnCase str1 str2 | length str1 /= length str2 = False
compareIgnCase (x1:xs) (y1:ys) =
    (toLower x1 == toLower y1) && compareIgnCase xs ys
compareIgnCase [] [] = True
compareIgnCase _ _ = False

-- |
-- Produces a comparison function of type @String -> String -> Bool@
-- using the options specified
--
-- @since 0.1.0.0
{-# INLINE compareUsingOptions #-}
compareUsingOptions :: UniqOptions -> (String -> String -> Bool)
compareUsingOptions opt =

    compareXYZ (ignoreCase opt) (skipFields opt) (skipChar opt) (checkChar opt)
    where

    compareWith :: (String -> String -> Bool) -> String -> String -> Bool
    compareWith eq s1 s2 = eq s1 s2

    -- Gives the number of characters to be deleted from the beginning of the
    -- string to skip @n@ non-space fields\/strings
    indexAfterSkippingNWords :: Int -> String -> Int
    indexAfterSkippingNWords n str = go 0 n
      where
        len = length str
        go i 0 = i
        go i _
            | i >= (len - 1) = len
        go i xx =
            if isChar (str !! i) && isSpace (str !! (i + 1))
                then go (i + 1) (xx - 1)
                else go (i + 1) xx
        isChar = not . isSpace

    -- @compareXYZ ign x y z s1 s2@ does the following in order:
    -- skips first @x@ non-space fields,
    -- skips first @y@ characters and takes first @z@ characters
    -- from the strings @s1@ and @s2@ and returns True only if they are equal
    -- after the above operations. Ignores case if
    -- @ign@ case is True
    compareXYZ :: Bool -> Int -> Int -> Maybe Int -> String -> String -> Bool
    compareXYZ ignCase x y mz str1 str2 =
        let ix1 = indexAfterSkippingNWords x str1
            ix2 = indexAfterSkippingNWords x str2
            compareFunc = if ignCase then compareIgnCase else (==)
         in case mz of
                Just z ->
                    compareWith
                        compareFunc
                        (slice (ix1 + y) z str1)
                        (slice (ix2 + y) z str2)
                _ ->
                    compareWith
                        compareFunc
                        (drop (ix1 + y) str1)
                        (drop (ix2 + y) str2)

-- | Applies the comparison function to adjacent strings in the stream and
-- returns a @UniqResult Int String@ stream where the integer is the count of
-- occurrences of the string
--
-- @since 0.1.0.0
{-# INLINE getRepetition #-}
getRepetition ::
       (IsStream t, Monad m)
    => (String -> String -> Bool)
    -> t m String
    -> t m UniqResult
getRepetition comparator =
    Stream.groupsBy
        comparator
        (Fold.foldl'
             (\(UniqResult i a) s ->
                  if i == 0
                  then UniqResult 1 s
                  else UniqResult (i + 1) a)
             (UniqResult 0 ""))

-- | Generates a @UniqResult@ stream applying uniq on the input stream according
-- to the the @UniqOptions@ specified.  When the output is @Repeated@, the @Int@
-- values in @UniqResult Int String@ are set to 1
--
-- @since 0.1.0.0
{-# INLINE uniq #-}
uniq :: (IsStream t, Monad m) => UniqOptions -> t m String -> t m UniqResult
uniq opt strm =
    case (output opt) of
        Unique -> extract (eq 1)
        All -> extract (\_ -> True)
        Duplicate -> extract (ge 1)
        Repeated -> Stream.map (\(UniqResult _ x) -> UniqResult 1 x) $ extract (ge 1)
  where
    eq n (UniqResult i _) = i == n
    ge n (UniqResult i _) = i > n
    extract predicate =
        Stream.filter predicate $ getRepetition (compareUsingOptions opt) strm

-- | Converts a @UniqResult@ stream to a @String@ stream.  For each @UniqResult
-- n s@ in the input stream, it concatenates the string @s n@ times in the
-- output stream
--
-- @since 0.1.0.0
{-# INLINE uniqResultToString #-}
uniqResultToString :: (IsStream t, Monad m) => t m UniqResult -> t m String
uniqResultToString = Stream.concatMap (\(UniqResult i x) -> Stream.replicate i x)
