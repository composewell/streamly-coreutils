module Streamly.Coreutils.Uniq (
      ignoreCase
    , splitOnNewLine
    , uniqCount
    , uniqRepeated
    , uniqDistinct
    , defaultUniqOptions
    , UniqOptions (..)
   )
where

import Streamly.Coreutils.Common

import qualified Streamly.Prelude as S
import qualified Streamly.Memory.Array as A
import qualified Streamly.Data.Unicode.Stream as U
import qualified Streamly.Internal.Data.Fold as FL

import Data.Word (Word8)
import Data.Char (toLower)
import System.IO (Handle, stdout)
import Foreign.Storable (Storable)
import System.Environment (getArgs)
import Control.Monad.IO.Class (MonadIO)
import Streamly.Data.Unicode.Stream (decodeLatin1)
import Streamly.Internal.Data.Stream.StreamK.Type (IsStream)

-------------------------------------------------------------------------------
-- Record for options used with uniq
-------------------------------------------------------------------------------


data UniqOptions = UniqOptions {
                     verbose :: Bool
                   , count :: Bool
--                     repeated :: Bool,      -- display only duplicate lines once for each group
--                     duplicate :: Bool,     -- print all duplicate lines
--                     skipFields :: Int,
--                     ignoreCase :: Bool,
--                     unique :: Bool,        -- print only unique lines
--                     zeroTerminated :: Bool,
--                     checkChar :: Int,
                  }

defaultUniqOptions :: UniqOptions
defaultUniqOptions = UniqOptions True True      -- add other options later


-------------------------------------------------------------------------------
-- helper functions for uniq
-------------------------------------------------------------------------------


-- convert stream of bytes to stream of characters - ignore case if bool is True
ignoreCase :: (IsStream t, Monad m) => Bool -> t m Word8 -> t m Char
ignoreCase flag strm = if flag == True
                       then S.map toLower (U.decodeLatin1 strm)
                       else U.decodeLatin1 strm


splitOnNewLine :: (MonadIO m, IsStream t, Monad m) => t m Char -> t m (A.Array Char)
splitOnNewLine strm = S.splitOnSuffix (== '\n') A.write strm


-- drops first n elements from a list

dropN :: Int -> [a] -> [a]
dropN n (x:xl) = if n == 0
                 then (x:xl)
                 else dropN (n - 1) xl
dropN _ [] = []


-- compares two Arrays ignoring first n entries

skipN :: (Storable a, Eq a) => Int -> A.Array a -> A.Array a -> Bool
skipN n a b = let
                  al = dropN n $ A.toList a
                  bl = dropN n $ A.toList b
              in
                  al == bl


-- to count occurences of each array of characters after splitOnNewLine
-- skips first n characters

uniqCount :: (IsStream t, Monad m) => Int -> t m (A.Array Char) -> t m (Int, String)
uniqCount n strm = S.groupsBy (skipN n)
                    (FL.mkPureId (\x -> \s -> (1 + fst x, if snd x == ""
                                                          then A.toList s
                                                          else snd x)) (0, "")) strm


-- filters Strings which are repeated (occ >= 2)

uniqRepeated :: (IsStream t, Monad m) => t m (Int, String) -> t m (Int, String)
uniqRepeated = S.filter (\x -> fst x >= 2)



-- filters Strings which are distinct (occ exactly = 1)

uniqDistinct :: (IsStream t, Monad m) => t m (Int, String) -> t m (Int, String)
uniqDistinct = S.filter (\x -> fst x == 1)


--uniq :: (IsStream t, Monad m) => UniqOptions -> SomeBase File -> Handle -> IO ()
      -- prints the output to the file or "/dev/stdout"
