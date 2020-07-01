module Main where

import Streamly.Coreutils
import qualified Streamly.Coreutils.Cp as C
import qualified Streamly.Coreutils.Uniq as U

import qualified Streamly.Prelude as S
import qualified Streamly.Memory.Array as A

import qualified Streamly.Data.Unicode.Stream as Un
import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.Prelude as IP
import qualified Streamly.Internal.FileSystem.Dir as Dir
import qualified Streamly.Internal.FileSystem.File as File
import qualified Streamly.Internal.FileSystem.Handle as FH
import Streamly.Internal.Data.Stream.Serial (SerialT(..))
import Streamly.Internal.Data.Stream.StreamK.Type (IsStream)

import Data.Word (Word8)
import System.Environment (getArgs)
import Control.Monad.IO.Class (MonadIO)
import System.IO (Handle, stdout, hPutStr)
import Streamly.Data.Unicode.Stream (decodeLatin1)

import Gauge (defaultMain, benchmark, Benchmarkable (..))
import Gauge.Benchmark (nf, bench, bgroup, nfAppIO, nfIO, env, whnfAppIO, whnfIO)
import Control.Monad.Catch (MonadThrow, MonadCatch)

srcFP :: FilePath
srcFP = "/home/shruti/sorted.txt"

dstFP :: FilePath
dstFP = "/home/shruti/copied.txt"


source :: MonadThrow m => m (SomeBase File)
source = do
            fp <- parseAbsFile "/home/shruti/sorted.txt"
            return $ Abs fp

dest :: MonadThrow m => m (SomeBase File)
dest = do
            fp <- parseAbsFile "/home/shruti/copied.txt"
            return $ Abs fp


byteStrm :: (IsStream t, Monad m, MonadIO m, MonadCatch m) => FilePath -> t m Word8
byteStrm fp = File.toBytes fp


charStrm :: (IsStream t, Monad m, MonadIO m, MonadCatch m) => FilePath -> t m Char
charStrm fp = Un.decodeLatin1 $ File.toBytes fp


arrayChar :: (IsStream t, MonadCatch m, MonadIO m, Monad m) => FilePath -> t m (A.Array Char)
arrayChar fp = U.splitOnNewLine
             $ U.ignoreCase False
             $ File.toBytes fp


main :: IO ()
main = do
         src <- source
         dst <- dest
         defaultMain [
            bgroup "cp" [
               bench "cpFile" $ nfIO (C.cpFile C.defaultCpOptions src dst)
            ],
            bgroup "uniq" [
               bench "ignoreCase" $ nfAppIO (\strm -> S.drain $ U.ignoreCase True strm) (byteStrm srcFP),
               bench "splitOnNewLine" $ nfAppIO (\strm -> S.drain $ U.splitOnNewLine strm) (charStrm srcFP),
               bench "uniqCount skip 0" $ nfAppIO (\strm -> S.drain $ U.uniqCount 0 strm) (arrayChar "/home/shruti/sorted.txt"),
               bench "uniqCount skip 1000" $ nfAppIO (\strm -> S.drain $ U.uniqCount 1000 strm) (arrayChar "/home/shruti/sorted.txt")
            ]
          ]
