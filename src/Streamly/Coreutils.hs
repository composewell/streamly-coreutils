import qualified Streamly.Prelude as S
import qualified Streamly.Data.Fold as FL
import qualified Streamly.Memory.Array as A

import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.Prelude as IP
import qualified Streamly.Internal.FileSystem.File as File
import qualified Streamly.Internal.FileSystem.Dir as Dir

import Path (Path)
import Path.Posix
      (Abs
      , Rel
      , File
      , Dir
      , Path
      , parseAbsFile
      , parseRelFile
      , SomeBase(..)
      , parseSomeFile
      , fromRelFile
      , fromAbsFile
      , fromRelFile
      , parseAbsDir
      , (</>)
      , parseRelDir
      , fromRelDir
      , fromAbsDir)
import System.Environment (getArgs)
import Streamly.Data.Unicode.Stream (decodeLatin1)


data OptsDict = OptsDict {
                  verbose :: Bool
                }


defaultOptsDict :: OptsDict
defaultOptsDict = OptsDict True         -- set to False later


cpVerbose :: Bool -> FilePath -> FilePath -> IO ()
cpVerbose opt src dest = if opt == True
                         then
                           putStrLn $ src ++ " -> " ++ dest
                         else
                           putStr ""


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


cpFile :: OptsDict -> SomeBase File -> SomeBase File -> IO ()
cpFile opt src dest = do
                        let srcFP = someFileToFP src
                        let dstFP = someFileToFP dest
                        File.fromChunks dstFP $ File.toChunksWithBufferOf (256*1024) srcFP
                        cpVerbose (verbose opt) srcFP dstFP
