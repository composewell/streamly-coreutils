module Main where

import Streamly.Coreutils
import qualified Streamly.Coreutils.Cp as C
import qualified Streamly.Coreutils.Cat as Cat
import qualified Streamly.Coreutils.Echo as E
import qualified Streamly.Coreutils.Uniq as U

import qualified Streamly.Prelude as S
import qualified Streamly.Memory.Array as A

import qualified Streamly.Data.Unicode.Stream as Un
import qualified Streamly.Internal.FileSystem.File as File
import Streamly.Internal.Data.Stream.Serial (SerialT(..))
import Streamly.Internal.Data.Stream.StreamK.Type (IsStream)

import Data.Word (Word8)
import GHC.IO.Handle.FD (openFile)
import Control.Monad.IO.Class (MonadIO)
import System.IO (
         Handle
       , stdout
       , hPutStr
       , IOMode(..))

import Gauge (defaultMain)
import Gauge.Benchmark (bench, bgroup, nfAppIO, nfIO)
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


arrayChar :: (IsStream t, Monad m, MonadIO m, MonadCatch m) => FilePath -> t m (A.Array Char)
arrayChar fp = U.splitOnNewLine
             $ U.ignoreCase False
             $ File.toBytes fp


intStrStrm :: (IsStream t, Monad m, MonadIO m, MonadCatch m) => Int -> FilePath -> t m (Int, String)
intStrStrm n fp = U.uniqCount n
                $ U.splitOnNewLine
                $ Un.decodeLatin1
                $ File.toBytes fp


handleStrm :: IsStream t => [FilePath] -> t IO Handle
handleStrm fpl = S.mapM (\s -> openFile s ReadMode) $ S.fromList fpl


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
               bench "uniqCount skip 0" $ nfAppIO (\strm -> S.drain $ U.uniqCount 0 strm) (arrayChar srcFP),
               bench "uniqCount skip 1000" $ nfAppIO (\strm -> S.drain $ U.uniqCount 1000 strm) (arrayChar srcFP),
               bench "uniqRepeated skip 0" $ nfAppIO (\strm -> S.drain $ U.uniqRepeated strm) (intStrStrm 0 srcFP),
               bench "uniqRepeated skip 1000" $ nfAppIO (\strm -> S.drain $ U.uniqRepeated strm) (intStrStrm 1000 srcFP),
               bench "uniqDistinct skip 0" $ nfAppIO (\strm -> S.drain $ U.uniqDistinct strm) (intStrStrm 0 srcFP),
               bench "uniqDistinct skip 1000" $ nfAppIO (\strm -> S.drain $ U.uniqDistinct strm) (intStrStrm 1000 srcFP),
               bench "uniqDistinct skip 1000 - nfIO" $ nfIO (S.drain $ U.uniqDistinct $ intStrStrm 1000 srcFP)
            ],
            bgroup "cat" [
               bench "cat" $ nfIO (S.drain $ Cat.cat Cat.defaultCatOptions stdout (handleStrm [srcFP]))
            ],
            bgroup "echo" [
               bench "echo" $ nfIO (E.echo E.defaultEchoOptions stdout $ File.toBytes srcFP)
            ]
          ]
