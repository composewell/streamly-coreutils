module Streamly.Coreutils.Echo (
      echo
    , trailingNewLine
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
import Streamly.Internal.Data.Stream.Serial (SerialT(..))
import Streamly.Internal.Data.Stream.StreamK.Type (IsStream)

import Data.Word (Word8)
import System.Environment (getArgs)
import System.IO (Handle, stdout, hPutStr)
import Streamly.Data.Unicode.Stream (decodeLatin1)

import Control.Monad.IO.Class (MonadIO)

trailingNewLine :: Bool -> Handle -> IO ()
trailingNewLine flag hd = if flag == True
                          then hPutStr hd "\n"
                          else hPutStr hd ""


echo :: (Monad m, MonadIO m) => EchoOptions -> Handle -> SerialT m Word8 -> m ()
echo opt hd strm = FH.fromBytes hd strm
