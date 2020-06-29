module Streamly.Coreutils.Uniq (
      splitOnNewLine
    , uniqCount
    , uniqRepeated
    , uniqDistinct
   )
where

import Streamly.Coreutils.Types

import qualified Streamly.Prelude as S
import qualified Streamly.Memory.Array as A

import qualified Streamly.Data.Unicode.Stream as U
import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.Prelude as IP
import qualified Streamly.Internal.FileSystem.Dir as Dir
import qualified Streamly.Internal.FileSystem.File as File
import qualified Streamly.Internal.FileSystem.Handle as FH
import Streamly.Internal.Data.Stream.StreamK.Type (IsStream)

import Data.Word (Word8)
import System.IO (Handle, stdout)
import System.Environment (getArgs)
import Streamly.Data.Unicode.Stream (decodeLatin1)

import Control.Monad.IO.Class (MonadIO)

splitOnNewLine :: (MonadIO m, IsStream t, Monad m) => t m Word8 -> t m (A.Array Char)
splitOnNewLine strm = S.splitOnSuffix (== '\n') A.write
                    $ U.decodeLatin1 strm


-- to count occurences of each array of characters after splitOnNewLine
uniqCount :: (IsStream t, Monad m) => t m (A.Array Char) -> t m (Int, String)
uniqCount strm = S.groupsBy (\a -> \b -> a == b)
                (FL.mkPureId (\x -> \s -> (1 + fst x, A.toList s)) (0, "")) strm


-- filters Strings which are repeated (occ >= 2)
uniqRepeated :: (IsStream t, Monad m) => t m (Int, String) -> t m (Int, String)
uniqRepeated = S.filter (\x -> fst x >= 2)



-- filters Strings which are distinct (occ exactly = 1)
uniqDistinct :: (IsStream t, Monad m) => t m (Int, String) -> t m (Int, String)
uniqDistinct = S.filter (\x -> fst x == 1)


--uniq :: (IsStream t, Monad m) => UniqOptions -> SomeBase File -> Handle -> IO ()
      -- prints the output to the file or "/dev/stdout"
