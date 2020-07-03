module Streamly.Coreutils.Tail (
      defaultTailOptions
    , TailOptions (..)
   )
where

import Streamly.Coreutils.Common

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

-------------------------------------------------------------------------------
-- Record for options used with tail
-------------------------------------------------------------------------------


data TailOptions = TailOptions {
                        lastNbytes :: Int          -- last n bytes
                      , fromNbytes :: Int          -- from n bytes from beginning to end
                      , lines :: Int               -- default 10
                      , verbose :: Bool            -- True for more than 1 file
                   }


defaultTailOptions :: TailOptions
defaultTailOptions = TailOptions 1000 1000 10 True


-------------------------------------------------------------------------------
-- helper functions for tail
-------------------------------------------------------------------------------
