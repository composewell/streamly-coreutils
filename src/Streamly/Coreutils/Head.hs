module Streamly.Coreutils.Head (
      extractFirst
    , exceptLast
    , firstLines
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

import Data.Word (Word8)
import System.Environment (getArgs)
import Control.Monad.IO.Class (MonadIO)
import System.IO (Handle, stdout, hPutStr)
import Streamly.Internal.Data.SVar (MonadAsync)
import Streamly.Internal.Data.Stream.Serial (SerialT(..))
import Streamly.Internal.Data.Stream.StreamK.Type (IsStream)


-- extracts first n bytes from a file as a stream of String
extractFirst :: IsStream t => Int -> SomeBase File -> t m String
extractFirst n fp = S.splitOnSuffix (== '\n') FL.toList
                  $ U.decodeLatin1
                  $ S.take n
                  $ File.toBytes
                  $ someFileToFP fp


-- except/leaves last n bytes from a file as a stream of String
exceptLast :: IsStream t => Int -> SomeBase File -> t m String
exceptLast n fp = let
                     strm = File.toBytes $ someFileToFP fp
                     len = S.length strm
                  in
                     S.splitOnSuffix (== '\n') FL.toList
                     $ U.decodeLatin1
                     $ S.take (len - n) strm


-- reads first n lines from file as a char stream
firstLines :: IsStream t => Int -> SomeBase File -> t m String
firstLines n fp = S.take n
                $ S.splitOnSuffix (== '\n') FL.toList
                $ U.decodeLatin1
                $ File.toBytes
                $ someFileToFP fp


-- interleave the name of the file
addFile :: (IsStream t, Monad m) => SomeBase File -> t m String -> t m String
addFile fp = S.cons $ someFileToFP fp


--head :: (IsStream t, Monad m) => HeadOptions -> t m (SomeBase File) -> t m String
