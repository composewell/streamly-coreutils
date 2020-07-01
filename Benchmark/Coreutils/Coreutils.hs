module Main where

import Streamly.Coreutils
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
import Control.Monad.IO.Class (MonadIO)
import System.IO (Handle, stdout, hPutStr)
import Streamly.Data.Unicode.Stream (decodeLatin1)

import Gauge (defaultMain, benchmark, Benchmarkable (..))
import Gauge.Benchmark (nf, bench, bgroup, nfIO)
import Control.Monad.Catch (MonadThrow)

--fib :: Int -> Int
--fib 0 = 0
--fib 1 = 1
--fib n = fib (n - 1) + fib (n - 2)
--
--
--main = defaultMain [
--         bench "fib" (nf fib 20)
--       ]


source :: MonadThrow m => m (SomeBase File)
source = do
            fp <- parseAbsFile "/home/shruti/alice29.txt"
            return $ Abs fp

dest :: MonadThrow m => m (SomeBase File)
dest = do
            fp <- parseAbsFile "/home/shruti/copied.txt"
            return $ Abs fp

main :: IO ()
main = do
         src <- source
         dst <- dest
         defaultMain [
            bgroup "cp bench" [
                bench "cpFile" (nfIO (cpFile defaultCpOptions src dst))
                              ]
            bgroup "uniq bench" [
               bench "splitOnNewLine" (nfIO (splitOnNewLine $ U.decodeLatin1 $ File.toBytes "/home/shruti/thrice"))
                              ]
          ]
