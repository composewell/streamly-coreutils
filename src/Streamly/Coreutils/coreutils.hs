import qualified Streamly.Prelude as S
import qualified Streamly.Data.Fold as FL
import qualified Streamly.Memory.Array as A

import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.Prelude as IP
import qualified Streamly.Internal.FileSystem.File as File
import qualified Streamly.Internal.FileSystem.Dir as Dir

import Path.Posix (Abs, Rel, File, Dir, Path, parseAbsFile, parseRelFile, SomeBase(..), parseSomeFile, fromRelFile, fromAbsFile, fromRelFile)
import GHC.Word (Word8)
import Data.Char (ord, chr, digitToInt)
import System.Environment (getArgs)
import Streamly.Data.Unicode.Stream (decodeLatin1)
import Streamly.Internal.Data.Stream.Serial (SerialT)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO)

data OptsDict = OptsDict {
                  verbose :: Bool
                }

defaultOptsDict :: OptsDict
defaultOptsDict = OptsDict True         -- set to False later

cpVerbose :: Bool -> SomeBase File -> SomeBase File -> IO ()
cpVerbose opt src dest = if opt == True then
                           print src >> print " -> " >> print dest
                       else
                           print ""

baseToFilePath :: SomeBase File -> FilePath
baseToFilePath some = case some of
                           Abs x -> fromAbsFile x
                           Rel x -> fromRelFile x


cpFile :: OptsDict -> SomeBase File -> SomeBase File -> IO ()
cpFile opt src dest = (File.fromChunks (baseToFilePath dest)
                      $ File.toChunksWithBufferOf (256*1024) $ baseToFilePath src)
                      >> cpVerbose (verbose opt) src dest


safeHead (x:_) = Just x
safeHead _ = Nothing

secd (x:y:_) = Just y
secd _ = Nothing

main :: IO ()
main = do
         lst <- getArgs
         case safeHead lst of
               Just hd -> case secd lst of
                           Just sec -> do
                                          src <- parseSomeFile hd
                                          dst <- parseSomeFile sec

                                          cpFile defaultOptsDict src dst
                           _        -> putStrLn "error"
               _       -> putStrLn "error"
