module Main where

import qualified Streamly.Prelude as S
import qualified Streamly.Memory.Array as A
import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Data.Unicode.Stream as Un
import qualified Streamly.Internal.FileSystem.File as File

import Data.Word (Word8)
import Streamly.Coreutils.Uniq
import Control.Monad.IO.Class (MonadIO)
import System.IO (
         Handle
       , stdout
       , hPutStr
       , IOMode(..)
       , openFile)

import Gauge (defaultMain)
import System.Directory (getHomeDirectory)
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


splitOnNewLine :: (MonadIO m, IsStream t, Monad m) => t m Char -> t m String
splitOnNewLine strm = S.splitOnSuffix (== '\n') FL.toList strm


opt :: UniqOptions
opt = defaultUniqOptions { skipFields = 12
                         , skipChar = 10 }

main :: IO ()
main = do
         src <- srcFP
         dst <- dstFP
         let comp = compareUsingOptions opt
         defaultMain [
            bgroup "uniq" [
               bench "getRepetition" $ nfAppIO (\strm -> S.drain $ getRepetition comp strm) (splitOnNewLine $ charStrm src),
               bench "uniq" $ nfAppIO (\strm -> S.drain $ uniq opt strm) (splitOnNewLine $ charStrm src),
               bench "uniqResultToString" $ nfAppIO (\strm -> S.drain $ uniqResultToString $ uniq opt strm) (splitOnNewLine $ charStrm src)
            ]
          ]
