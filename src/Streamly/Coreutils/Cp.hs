{-# LANGUAGE BlockArguments #-}
module Streamly.Coreutils.Cp
    ( CpOptions (..)
    , defaultCpOptions
    , cpFileWithRename
    , cpFiles
    , cpDir
    , traverseDir
   )
where

import Path
import Streamly
import Streamly.Coreutils.Common

import qualified Streamly.Prelude as S
import qualified Streamly.Internal.Prelude as IP
import qualified Streamly.Internal.FileSystem.File as File

import System.Info (os)
import System.IO.Unsafe (unsafePerformIO)
import System.Directory (listDirectory, doesFileExist, createDirectoryIfMissing)

-------------------------------------------------------------------------------
-- Record for options used with cp
-------------------------------------------------------------------------------

-- |
data CpOptions = CpOptions
    { verbose :: Bool
    -- ^ Prints the source and destination files being copied
    , force :: Bool
    -- ^ if the destination file
    -- cannot be opened, removes it and continues copying the rest
    , overwriteExisting :: Bool
    -- ^ If @True@, an existing destination file will be overwritten
    -- due to copying
    }


-- | Default options for @cp@ are @verbose@ and @overwriteExisting@ to
-- @True@ and @force@ set to @False@
{-# INLINE defaultCpOptions #-}
defaultCpOptions :: CpOptions
defaultCpOptions = CpOptions True False True


-------------------------------------------------------------------------------
-- cp and helper functions for options
-------------------------------------------------------------------------------


-- | Basic file copying with @FilePath@ as argument types.
-- Prints paths of source, destination files when @verbose@ is @True@.
-- Returns @True@ if copying operation was successful.
-- @False@ is returned in cases where a file by the same name
-- exists in the destination path and @overwriteExisting@ is set to
-- @False@ or if the source file does not exist.
{-# INLINE copy #-}
copy
    :: CpOptions
    -> FilePath
    -> FilePath
    -> IO Bool
copy opt src dst = do
    existDst <- doesFileExist dst
    existSrc <- doesFileExist src
    if not existSrc
    then do
        putStrLn $ "Source file " ++ src ++ " does not exist"
        return False
    else if existDst && not (overwriteExisting opt)
    then do
        putStrLn $ "overwriteExisting : False " ++ dst ++ " already exists"
        return False
    else if verbose opt == True
    then do
        File.fromChunks dst $ File.toChunksWithBufferOf (256*1024) src
        putStrLn $ src ++ " -> " ++ dst
        return True
    else do
        File.fromChunks dst $ File.toChunksWithBufferOf (256*1024) src
        return True


-- | Append two paths - first one should be a directory
-- Assumes second path is relative and without a leading '/'
{-# INLINE append #-}
append
    :: FilePath
    -> FilePath
    -> FilePath
append dir file =
    let end = last dir
        osName = os
        pathSep = if osName == "windows" then '\\' else '/'
     in
         if dir == ""
         then file
         else if end == pathSep
         then dir ++ file
         else dir ++ [pathSep] ++ file


-- | Removes a trailing slash from a file path
{-# INLINE removeTrailingSlash #-}
removeTrailingSlash
    :: FilePath
    -> FilePath
removeTrailingSlash "" = ""
removeTrailingSlash path =
    let end = last path
        osName = os
        pathSep = if osName == "windows" then '\\' else '/'
     in
         if end == pathSep
         then init path
         else path


-- | Lists the contents (both files and directories) of the directory
-- relative to the argument directory.
-- @Left path@ represents that @path@ is a directory while @Right path@
-- represents that it is a file.
traverseDir
    :: IsStream t
    => FilePath
    -> t IO (Either FilePath FilePath)
traverseDir baseDir = do
    IP.concatMapTreeWith ahead listContents $ S.yield $ Left ""

    where

    listContents :: IsStream t => FilePath -> t IO (Either FilePath FilePath)
    listContents dir = do
        S.fromListM $ map (identify dir) $ contents dir

    identify :: FilePath -> FilePath -> IO (Either FilePath FilePath)
    identify dir path = do
       let actualPath = append baseDir $ append dir path
       isFile <- doesFileExist actualPath
       if isFile == True
       then
           return $ Right $ append dir path     -- RELATIVE File
       else
           return $ Left $ append dir path      -- RELATIVE Dir

    contents :: FilePath -> [FilePath]
    contents dir = unsafePerformIO $ listDirectory $ append baseDir dir



-- | Recursively copies the contents of the source directory
-- to the destination directory.
-- Arguments are of type @FilePath@.
copyDirToDir
    :: IsStream t
    => CpOptions
    -> FilePath
    -> FilePath
    -> t IO ()
copyDirToDir opt src dest = do
    if force opt == True
    then
      S.mapM handleForce $ traverseDir src
    else
      S.mapM (\_ -> return ()) $ S.takeWhileM handleNonForce $ traverseDir src
    -- Create destination directory if it does not exist
    --S.mapM createDir $ S.filter isDir $ traverseDir src
    --S.mapM  S.findIndices isDir $ traverseDir src
    --S.mapM createDir $ S.filter isFile $ traverseDir src
    where

    createDir :: Either FilePath FilePath -> IO ()
    createDir (Left dir) = createDirectoryIfMissing True $ append dest dir
    createDir _ = return ()

    isDir :: Either FilePath FilePath -> Bool
    isDir (Left _) = True
    isDir _ = False

    isFile :: Either FilePath FilePath -> Bool
    isFile (Right _) = True
    isFile _ = False

    handleForce :: Either FilePath FilePath -> IO ()
    handleForce (Left dir) = createDirectoryIfMissing True $ append dest dir
    handleForce (Right file) = do
        copy opt (append src file) (append dest file)
        return ()

    handleNonForce :: Either FilePath FilePath -> IO Bool
    handleNonForce (Left _) = return True
    handleNonForce (Right file) =
        copy opt (append src file) (append dest file)


-- Copies the source file to the destination path with a possible rename
cpFileWithRename
    :: CpOptions
    -> SomeBase File
    -> SomeBase File
    -> IO Bool
cpFileWithRename opt src dst = do
    let srcFp = removeTrailingSlash $ someFileToFP src
    let dstFp = removeTrailingSlash $ someFileToFP dst
    copy opt srcFp dstFp


-- | Copies the source file preserving the source file name
-- in the destination directory.
-- Returns @True@ in the @IO@ monad if copying was successful,
-- @False@ otherwise.
cpFile
    :: CpOptions
    -> SomeBase File
    -> SomeBase Dir
    -> IO Bool
cpFile option src dest = do
    copy option srcFp destFp

    where

    srcFp = removeTrailingSlash $ someFileToFP src
    dirFp = someDirToFP dest
    destFp = append dirFp fileName
    fileName = extractFileName src

    -- | To extract the file part of a @SomeBase File@ path
    extractFileName :: SomeBase File -> FilePath
    extractFileName srcFile =
        case srcFile of
            Abs fpath -> fromRelFile $ filename fpath
            Rel fpath -> fromRelFile $ filename fpath


-- | Copies a stream of files to the destination directory.
-- The stream can be drained with the appropriate combinator
-- to achieve concurrency.
-- Fails if the destination directory does not exist.
cpFiles
    :: IsStream t
    => CpOptions
    -> t IO (SomeBase File)
    -> SomeBase Dir
    -> t IO (SomeBase File)
    -- ^ Stream of files for which copying was successful
cpFiles opt strm dir = do
    if force opt == True
    then
        S.filterM (\f -> cpFile opt f dir) strm
    else
        S.takeWhileM (\f -> cpFile opt f dir) strm


-- | Recursively copies the source directory's contents
-- to the destination directory
cpDir
    :: IsStream t
    => CpOptions
    -> SomeBase Dir
    -> SomeBase Dir
    -> t IO ()
cpDir opt src dst = do
    let srcFp = someDirToFP src
    let dstFp = someDirToFP dst
    copyDirToDir opt srcFp dstFp
