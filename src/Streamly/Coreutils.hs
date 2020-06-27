--module Streamly.Coreutils (
--      cpFile
--    , defaultOptsDict
--    , OptsDict
--   )
--where

--import Streamly.Coreutils.Types
import qualified Streamly.Prelude as S
import qualified Streamly.Data.Fold as FL
import qualified Streamly.Memory.Array as A

import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.Prelude as IP
import qualified Streamly.Internal.FileSystem.Dir as Dir
import qualified Streamly.Internal.FileSystem.File as File
import qualified Streamly.Internal.FileSystem.Handle as FH

import Streamly.Internal.Data.Stream.StreamK.Type (IsStream)

import Path
import Path.Posix
      (Path
      , File
      , Dir
      , Abs
      , Rel
      , parseAbsFile
      , parseRelFile
      , parseSomeFile
      , fromRelFile
      , fromAbsFile
      , fromRelFile
      , parseAbsDir
      , (</>)
      , parseRelDir
      , fromRelDir
      , fromAbsDir)

import System.IO (Handle, stdout)
import System.Environment (getArgs)
import Streamly.Data.Unicode.Stream (decodeLatin1)

-------------------------------------------------------------------------------
-- helper functions - NOT useful at the moment (not using SomeBase type)
-------------------------------------------------------------------------------

someFileToFP :: SomeBase File -> FilePath
someFileToFP some =
                  case some of
                     Abs x -> fromAbsFile x
                     Rel x -> fromRelFile x


dirToFilePath :: SomeBase Dir -> FilePath
dirToFilePath some =
                   case some of
                     Abs x -> fromAbsDir x
                     Rel x -> fromRelDir x


data EchoOptions = EchoOptions {
                     verbose :: Bool
                  }
-------------------------------------------------------------------------------
-- cp and helper functions for options
-------------------------------------------------------------------------------

cpVerbose :: Bool -> FilePath -> FilePath -> IO ()
cpVerbose opt src dest = if opt == True
                         then
                           putStrLn $ src ++ " -> " ++ dest
                         else
                           putStr ""

echo :: EchoOptions -> Handle -> IO ()
echo opt handle = FH.fromBytes handle $ FH.getBytes

--cpFile :: OptsDict -> Path Abs File -> Path Abs File -> IO ()
--cpFile opt src dest = do
--                        let srcFP = fromAbsFile src
--                        let dstFP = fromAbsFile dest
--                        File.fromChunks dstFP $ File.toChunksWithBufferOf (256*1024) srcFP
--                        cpVerbose (verbose opt) srcFP dstFP
--

main :: IO ()
main = do
      let out = stdout
      echo (EchoOptions True) out
