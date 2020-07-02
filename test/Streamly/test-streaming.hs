module Main(main) where
import qualified Streamly.Prelude as S
import qualified Streamly.Data.Fold as FL
import qualified Streamly.Memory.Array as A

import Streamly.Coreutils
import Streamly.Coreutils.Cp as C
import Streamly.Coreutils.Uniq as U
import Streamly.Coreutils.Echo as E
import Streamly.Coreutils.Cat as Ca
import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.Prelude as IP
import qualified Streamly.Internal.FileSystem.File as File
import qualified Streamly.Internal.FileSystem.Dir as Dir

import Data.Word (Word8)
import Path (Path)
import System.IO (stdout, IOMode(ReadMode))
import GHC.IO.Handle.FD (openFile)
import Path.Posix (Abs, Rel, File, Dir, Path, parseAbsFile, parseRelFile, SomeBase(..), parseSomeFile, fromRelFile, fromAbsFile, fromRelFile, parseAbsDir, (</>), parseRelDir, fromRelDir, fromAbsDir)
import Data.Char (ord, chr, digitToInt)
import System.Environment (getArgs)
import Streamly.Data.Unicode.Stream (decodeLatin1)
import Streamly.Internal.Data.Stream.Serial (SerialT)

--import Control.Monad.Catch (MonadCatch)
--import Control.Monad.IO.Class (MonadIO)
--
--import Test.Hspec as H
--import Test.Hspec.QuickCheck
--import Test.QuickCheck (Property, forAll, Gen, vectorOf, arbitrary, choose)
--import Test.QuickCheck.Monadic (monadicIO, assert, run)

maxStreamLen :: Int
maxStreamLen = 1000

intMin :: Int
intMin = minBound

intMax :: Int
intMax = maxBound

{-# INLINE maxStreamLen #-}
{-# INLINE intMin #-}
{-# INLINE intMax #-}

safeHead (x:_) = Just x
safeHead _ = Nothing

second (x:y:_) = Just y
second _ = Nothing

a :: Word8
a = 97

main :: IO ()
main = do
--      hd <- getLine
--      sec <- getLine
--      src <- parseSomeFile hd
--      dst <- parseSomeFile sec
--      let n = defaultCpOptions {C.verbose = False}
--      cpFile n src dst

        S.drainM $ U.ignoreCase False $ S.repeat a
--      S.drain $ S.mapM (putStrLn . A.toList) (U.splitOnNewLine $ U.ignoreCase True (File.toBytes "/home/shruti/test-uniq.txt"))
--      -- S.mapM_
--      S.mapM_ print $ U.uniqCount 0 (U.splitOnNewLine $ U.ignoreCase True (File.toBytes "/home/shruti/test-uniq.txt"))
--      S.mapM_ print $ U.uniqRepeated $ U.uniqCount 4 (U.splitOnNewLine $ U.ignoreCase True (File.toBytes "/home/shruti/test-uniq.txt"))
--      S.mapM_ print $ U.uniqDistinct $ U.uniqCount 4 (U.splitOnNewLine $ U.ignoreCase True (File.toBytes "/home/shruti/test-uniq.txt"))
--      S.drain $ Ca.cat defaultCatOptions stdout (S.yieldM $ openFile "/home/shruti/test-cat.txt" ReadMode)
--      echo E.defaultEchoOptions stdout (File.toBytes "/home/shruti/test-uniq.txt")
--      E.trailingNewLine (E.trailingLine defaultEchoOptions) stdout
