module Streamly.Coreutils.Cp (
      copyVerbose
    , copy
    , copyFileList
    , append
    , copyDirToDir
    , fileToFile
    , fileToDir
    , dirToDir
    , extractFileName
    , CpOptions (..)
    , defaultCpOptions
   )
where

import Path.Posix
import Streamly.Coreutils.Common
import qualified Streamly.Prelude as S
import qualified Streamly.Data.Fold as FL
import qualified Streamly.Memory.Array as A

import qualified Streamly.Internal.Prelude as IP
import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.FileSystem.Dir as Dir
import qualified Streamly.Internal.FileSystem.File as File
import qualified Streamly.Internal.FileSystem.Handle as FH

--import System.IO (Handle, stdout)
--import System.Environment (getArgs)
--import Control.Monad.Trans.Class (lift)
import System.Directory (listDirectory, doesFileExist, createDirectoryIfMissing)
import Streamly.Data.Unicode.Stream (decodeLatin1)
import Streamly.Internal.Data.Stream.StreamK.Type (IsStream)

-------------------------------------------------------------------------------
-- Record for options used with cp
-------------------------------------------------------------------------------


data CpOptions = CpOptions {
                  verbose :: Bool
                }


defaultCpOptions :: CpOptions
defaultCpOptions = CpOptions True


-------------------------------------------------------------------------------
-- cp and helper functions for options
-------------------------------------------------------------------------------

-- cp
-- File -> File (newfile name as given in dest)
-- (preserves file name)- these 3 coming below
-- File -> Dir
-- [File] -> Dir
-- Dir -> Dir

-- inline
-- FilePath -> FilePath
-- t m FilePath -> FilePath
-- SomeBase File -> SomeBase Dir
-- SomeBase Dir -> SomeBase Dir

-------------------------------------------------------------------------------


-- |
-- to print source, destination file paths when verbose is True
{-# INLINE copyVerbose #-}
copyVerbose :: FilePath -> FilePath -> IO ()
copyVerbose src dest = putStrLn $ src ++ " -> " ++ dest


-- |
-- Basic file copying with arg types @FilePath@
{-# INLINE copy #-}
copy :: FilePath -> FilePath -> IO ()
copy src dst = File.fromChunks dst
    $ File.toChunksWithBufferOf (256*1024) src


copyFileList :: [FilePath] -> FilePath -> IO ()
copyFileList ls dst = mapM_ (\f -> copy f dst) ls


-- |
-- to extract the file part of a @SomeBase File@ path
extractFileName :: SomeBase File -> FilePath
extractFileName src =
    case src of
        Abs fpath -> fromRelFile $ filename fpath
        Rel fpath -> fromRelFile $ filename fpath


-- unsafe => first arg dir shouldn't be empty
append :: FilePath -> FilePath -> FilePath
append dir file =
    if last dir == '/'
    then
       dir ++ file
    else
       dir ++ "/" ++ file


-- copy dir to dir (FilePath)
copyDirToDir :: FilePath -> FilePath -> IO ()
copyDirToDir src dst = do
    fileList <- listDirectory src
    createDirectoryIfMissing True dst
    mapM_ (\f -> do
             flag <- doesFileExist $ append src f
             if flag == True
             then do
                 copyVerbose (append src f) $ append dst f
                 copy (append src f) $ append dst f
             else do
                 createDirectoryIfMissing True $ append dst f
                 copyVerbose (append src f) $ append dst f
                 copyDirToDir (append src f) $ append dst f) fileList


-------------------------------------------------------------------------------

-- SomeBase File -> SomeBase File
cpFileWithRename :: SomeBase File -> SomeBase File -> IO ()
cpFileWithRename src dst = do
    let srcFp = someFileToFP src
    let dstFp = someFileToFP dst
    copy srcFp dstFp


-- SomeBase File -> SomeBase Dir
-- preserves src file name
cpFile :: SomeBase File -> SomeBase Dir -> IO ()
cpFile src dest = do
    let srcFp = someFileToFP src
    let dirFp = someDirToFP dest
    let fileName = extractFileName src
    case src of
        Abs fpath -> copy srcFp $ dirFp ++ fileName
        Rel fpath -> copy srcFp $ dirFp ++ fileName


traverseDir :: (IsStream t, Monad m) => SomeBase Dir -> t m (SomeBase File)
traverseDir dir = do
    dirFp <- someDirToFP dir
    S.dirFplistDirectory dirFp

-- SomeBase Dir -> SomeBase Dir
cpDir :: SomeBase Dir -> SomeBase Dir -> IO ()
cpDir src dst = do
    let srcFP = someDirToFP src
    let dstFP = someDirToFP dst
    copyDirToDir srcFP dstFP
