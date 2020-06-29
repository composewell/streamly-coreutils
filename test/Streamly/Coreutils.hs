module Main where
import qualified Streamly.Prelude as S
import qualified Streamly.Data.Fold as FL
import qualified Streamly.Memory.Array as A

import Streamly.Coreutils
import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.Prelude as IP
import qualified Streamly.Internal.FileSystem.File as File
import qualified Streamly.Internal.FileSystem.Dir as Dir

import Path (Path)
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

main :: IO ()
main = do
--      hd <- getLine
--      sec <- getLine
--      src <- parseSomeFile hd
--      dst <- parseSomeFile sec
--      cpFile defaultCpOptions src dst

      S.drain $ S.mapM (putStrLn . A.toList) (splitOnNewLine (File.toBytes "/home/shruti/test-uniq.txt"))
      -- S.mapM_
      S.mapM_ print $ uniqCount (splitOnNewLine (File.toBytes "/home/shruti/test-uniq.txt"))

