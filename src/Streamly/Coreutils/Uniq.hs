module Streamly.Coreutils.Uniq (
        splitOnNewLine
      , getRepetition
      , defaultUniqOptions
      , UniqOptions (..)
   )
where
import qualified Streamly.Prelude as S
import qualified Streamly.Internal.Data.Fold as FL

import Data.Char (toLower, isSpace)
import Control.Monad.IO.Class (MonadIO)
import System.IO.Unsafe (unsafePerformIO)
import Streamly.Internal.Data.Stream.StreamK.Type (IsStream)

-------------------------------------------------------------------------------
-- Record for options used with uniq
-------------------------------------------------------------------------------

{-# INLINE intMax #-}

intMax :: Int
intMax = maxBound


data UniqOptions = UniqOptions {
                       count :: Bool
                     , repeated :: Bool       -- display only duplicate lines once for each group
                     , duplicate :: Bool      -- print all duplicate lines
                     , skipFields :: Int      -- skips first skipFields number of non-empty strings
                     , skipChar :: Int        -- skips first skipChar number of characters (after above op)
                     , checkChar :: Int       -- take first checkChar number of characters for comparison (after above op)
                     , ignoreCase :: Bool     -- whether to ignore case
                     , unique :: Bool         -- print only unique lines
                     , delimiter :: Char      -- to separate output lines
                  }

{-# INLINE defaultUniqOptions #-}

defaultUniqOptions :: UniqOptions
defaultUniqOptions = UniqOptions True False False 0 0 intMax False True '\n'


-------------------------------------------------------------------------------
-- helper functions for uniq
-------------------------------------------------------------------------------


splitOnNewLine :: (MonadIO m, IsStream t, Monad m) => t m Char -> t m String
splitOnNewLine strm = S.splitOnSuffix (== '\n') FL.toList strm


{-# INLINE xorCS #-}

xorCS :: Char -> Char -> Bool
xorCS s c = (isSpace s == True && isSpace c == True) || (isSpace s == False && isSpace c == False)


divide :: Int -> Char -> String -> String
divide 0 c s = c:s
divide n c (s:sl) = if xorCS c s == True then
                        divide n s sl
                     else
                        divide (n-1) s sl


skipFirstN :: Int -> String -> String
skipFirstN 0 str = str
skipFirstN n str = divide (2*n) ' ' str


proc :: Int -> Int -> Int -> String -> String
proc skpf skpc tkc = (take tkc . drop skpc . skipFirstN skpf)


-- skpFields, skipChars, takeChars
comp :: Bool -> Int -> Int -> Int -> String -> String -> Bool
comp ignCase skpf skpc tkc s1 s2 = if ignCase == True
                                   then
                                      (proc skpf skpc tkc (map toLower s1) == proc skpf skpc tkc (map toLower s2))
                                   else
                                      (proc skpf skpc tkc s1 == proc skpf skpc tkc s2)


getRepetition :: (IsStream t, Monad m) => Bool -> Int -> Int -> Int -> t m String -> t m (Int, String)
getRepetition ign skpf skpc tkc = S.groupsBy (comp ign skpf skpc tkc) (FL.mkPureId (\(i, a) s -> if a == ""
                                                                                         then (i + 1, s)
                                                                                         else (i + 1, a)) (0, ""))


onlyDuplicate :: (IsStream t, Monad m) => t m (Int, String) -> t m String
onlyDuplicate = S.concatMap (\(i, x) -> if i > 1
                                        then S.replicate i x
                                        else S.nil)


onlyRepeated :: (IsStream t, Monad m) => t m (Int, String) -> t m String
onlyRepeated = S.concatMap (\(i, x) -> if i > 1
                                       then S.replicate i x
                                       else S.nil)


onlyUnique :: (IsStream t, Monad m) => t m (Int, String) -> t m String
onlyUnique = S.concatMap (\(i, x) -> if i == 1
                                     then S.replicate i x
                                     else S.nil)
