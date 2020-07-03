module Streamly.Coreutils.Cp (
      cpFile
    , defaultCpOptions
    , CpOptions (..)
   )
where

import Streamly.Coreutils.Common
import qualified Streamly.Prelude as S
import qualified Streamly.Data.Fold as FL
import qualified Streamly.Memory.Array as A

import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.Prelude as IP
import qualified Streamly.Internal.FileSystem.Dir as Dir
import qualified Streamly.Internal.FileSystem.File as File
import qualified Streamly.Internal.FileSystem.Handle as FH

import Streamly.Internal.Data.Stream.StreamK.Type (IsStream)

import System.IO (Handle, stdout)
import System.Environment (getArgs)
import Streamly.Data.Unicode.Stream (decodeLatin1)

-------------------------------------------------------------------------------
-- Record for options used with cp
-------------------------------------------------------------------------------


data CpOptions = CpOptions {
                  verbose :: Bool
                }


defaultCpOptions :: CpOptions
defaultCpOptions = CpOptions True        -- set to False later


-------------------------------------------------------------------------------
-- cp and helper functions for options
-------------------------------------------------------------------------------

cpPrint :: Bool -> FilePath -> FilePath -> IO ()
cpPrint opt src dest = if opt == True
                         then
                           putStrLn $ src ++ " -> " ++ dest
                         else
                           putStr ""


cpFile :: CpOptions -> SomeBase File -> SomeBase File -> IO ()
cpFile opt src dest = do
                        let srcFP = someFileToFP src
                        let dstFP = someFileToFP dest
                        File.fromChunks dstFP $ File.toChunksWithBufferOf (256*1024) srcFP
                        cpPrint (verbose opt) srcFP dstFP
