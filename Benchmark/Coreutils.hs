module Main where

import qualified Streamly.Prelude as S
import qualified Streamly.Memory.Array as A
import qualified Streamly.Coreutils.Uniq as U
import qualified Streamly.Data.Unicode.Stream as Un
import qualified Streamly.Internal.FileSystem.File as File


import Data.Word (Word8)
import Control.Monad.IO.Class (MonadIO)
import System.IO (
         Handle
       , stdout
       , hPutStr
       , IOMode(..)
       , openFile)

import System.Directory (getHomeDirectory)
import Gauge (defaultMain)
import Control.Monad.Catch (MonadThrow, MonadCatch)
import Gauge.Benchmark (bench, bgroup, nfAppIO, nfIO)
import Streamly.Internal.Data.Stream.Serial (SerialT(..))
import Streamly.Internal.Data.Stream.StreamK.Type (IsStream)

srcFP ::IO FilePath
srcFP = do
            home <- getHomeDirectory
            return $ home ++ "/sorted.txt"

dstFP :: IO FilePath
dstFP = do
            home <- getHomeDirectory
            return $ home ++ "/copied.txt"



charStrm :: (IsStream t, Monad m, MonadIO m, MonadCatch m) => FilePath -> t m Char
charStrm fp = Un.decodeLatin1 $ File.toBytes fp


opt :: U.UniqOptions
opt = U.defaultUniqOptions {U.skipFields = 2}

main :: IO ()
main = do
         src <- srcFP
         dst <- dstFP
         defaultMain [
            bgroup "uniq" [
               bench "ignCase" $ nfAppIO (\strm -> S.drain $ U.ignCase True strm) (charStrm src),
               bench "splitOnNewLine" $ nfAppIO (\strm -> S.drain $ U.splitOnNewLine strm) (U.ignCase False $ charStrm src),
               bench "getRepetition" $ nfAppIO (\strm -> S.drain $ U.getRepetition opt strm) (U.splitOnNewLine $ U.ignCase False $ charStrm src),
               bench "filterStream" $ nfAppIO (\strm -> S.drain $ U.filterStream opt strm) (U.getRepetition opt $ U.splitOnNewLine $ U.ignCase False $ charStrm src),
               bench "uniqCount" $ nfAppIO (\strm -> S.drain $ U.uniqCount opt strm) (charStrm src),
               bench "merge" $ nfAppIO (\strm -> S.drain $ U.merge opt strm) (U.uniqCount opt $ charStrm src)
            ]
          ]
