module Streamly.Coreutils.Cp
    ( CpOptions (..)
    , defaultCpOptions
    , copy
    , traverseDir
    , cpFile
    , cpFiles
    , cpDirSerial
    , cpDirAsync
    , cpDirParallel
   )
where

import Path
import Streamly

import qualified Streamly.Prelude as S
import qualified Streamly.Internal.Prelude as IP
import qualified Streamly.Internal.FileSystem.File as File

import System.IO.Unsafe (unsafePerformIO)
import System.Directory (listDirectory, doesFileExist, createDirectoryIfMissing)

-------------------------------------------------------------------------------
-- Record for options used with cp
-------------------------------------------------------------------------------

-- | Data type to represent flags\/options in GNU @cp@
--
-- @since 0.1.0.0
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
--
-- @since 0.1.0.0
{-# INLINE defaultCpOptions #-}
defaultCpOptions :: CpOptions
defaultCpOptions = CpOptions True False True


-------------------------------------------------------------------------------
-- cp and helper functions for options
-------------------------------------------------------------------------------


-- | Prints paths of source, destination files when @verbose@ is @True@.
-- Returns @True@ if copying operation was successful.
-- @False@ is returned in cases where a file by the same name
-- exists in the destination path and @overwriteExisting@ is set to
-- @False@ or if the source file does not exist.
--
-- @since 0.1.0.0
{-# INLINE copy #-}
copy
    :: CpOptions
    -> Path Abs File
    -> Path Abs File
    -> IO Bool
copy opt src dst = do
    let srcFp = fromAbsFile src
    let dstFp = fromAbsFile dst
    existDst <- doesFileExist dstFp
    existSrc <- doesFileExist srcFp
    if not existSrc
    then do
        putStrLn $ "Source file " ++ srcFp ++ " does not exist"
        return False
    else if existDst && not (overwriteExisting opt)
    then do
        putStrLn $ "overwriteExisting : False " ++ dstFp ++ " already exists"
        return False
    else if verbose opt == True
    then do
        File.fromChunks dstFp $ File.toChunksWithBufferOf (256*1024) srcFp
        putStrLn $ srcFp ++ " -> " ++ dstFp
        return True
    else do
        File.fromChunks dstFp $ File.toChunksWithBufferOf (256*1024) srcFp
        return True


-- | Lists the contents (both files and directories) of the directory
-- as absolute paths.
-- @Left path@ represents that @path@ is a directory while @Right path@
-- represents that it is a file.
--
-- @since 0.1.0.0
traverseDir
    :: IsStream t
    => Path Abs Dir
    -> t IO (Either (Path Abs Dir) (Path Abs File))
traverseDir baseDir = do
    IP.concatMapTreeWith ahead listContents $ S.yield $ Left baseDir

    where

    listContents :: IsStream t => Path Abs Dir -> t IO (Either (Path Abs Dir) (Path Abs File))
    listContents dir = do
        S.fromListM $ map (identify dir) $ contents $ fromAbsDir dir

    identify :: Path Abs Dir -> FilePath -> IO (Either (Path Abs Dir) (Path Abs File))
    identify dir path = do
       asFile <- parseRelFile path
       let filePath = dir </> asFile
       isFile <- doesFileExist $ fromAbsFile filePath
       if isFile == True
       then do
           return $ Right filePath
       else do
           asDir <- parseRelDir path
           return $ Left $ dir </> asDir

    contents :: FilePath -> [FilePath]
    contents dir = unsafePerformIO $ listDirectory dir



-- | Recursively copies the contents of the source directory
-- to the destination directory.
--
-- @since 0.1.0.0
copyDirToDir
    :: IsStream t
    => CpOptions
    -> Path Abs Dir
    -> Path Abs Dir
    -> t IO ()
copyDirToDir opt src dest = do
    if force opt == True
    then
        S.mapM handleForce
        $ traverseDir src
    else
        S.mapM (\_ -> return ())
        $ S.takeWhileM handleNonForce
        $ traverseDir src

    where

    handleForce :: Either (Path Abs Dir) (Path Abs File) -> IO ()
    handleForce (Left dir) = do
        if dir == src
        then createDirectoryIfMissing True $ fromAbsDir dest
        else do
            actualPath <- stripAndPrepend dir src dest
            createDirectoryIfMissing True $ fromAbsDir actualPath
    handleForce (Right file) = do
        actualPath <- stripAndPrepend file src dest
        _ <- copy opt file actualPath
        return ()


    handleNonForce :: Either (Path Abs Dir) (Path Abs File) -> IO Bool
    handleNonForce (Left dir) = do
        if dir == src
        then do
            createDirectoryIfMissing True $ fromAbsDir dest
            return True
        else do
            actualPath <- stripAndPrepend dir src dest
            createDirectoryIfMissing True $ fromAbsDir actualPath
            return True
    handleNonForce (Right file) = do
        actualPath <- stripAndPrepend file src dest
        copy opt file actualPath

    stripAndPrepend :: Path Abs t -> Path Abs Dir -> Path Abs Dir  -> IO (Path Abs t)
    stripAndPrepend path strip pref = do
        suffix <- stripProperPrefix strip path
        return $ pref </> suffix




-- | Copies the source file preserving the source file name
-- in the destination directory.
-- Returns @True@ in the @IO@ monad if copying was successful,
-- @False@ otherwise.
--
-- @since 0.1.0.0
cpFile
    :: CpOptions
    -> Path Abs File
    -> Path Abs Dir
    -> IO Bool
cpFile option src dest =
    copy option src $ dest </> filename src


-- | Copies a stream of files to the destination directory.
-- The stream can be drained with the appropriate combinator
-- to achieve concurrency.
-- Fails if the destination directory does not exist.
--
-- @since 0.1.0.0
cpFiles
    :: IsStream t
    => CpOptions
    -> t IO (Path Abs File)
    -> Path Abs Dir
    -> t IO (Path Abs File)
    -- ^ Stream of files for which copying was successful
cpFiles opt strm dir = do
    if force opt == True
    then
        S.filterM (\f -> cpFile opt f dir) strm
    else
        S.takeWhileM (\f -> cpFile opt f dir) strm


-- | Recursively and @serially@ copies the source directory's contents
-- to the destination directory
--
-- @since 0.1.0.0
cpDirSerial
    :: CpOptions
    -> Path Abs Dir
    -> Path Abs Dir
    -> IO ()
cpDirSerial opt src dst = do
    S.drain $ serially $ copyDirToDir opt src dst


-- | Recursively and @asyncly@ copies the source directory's contents
-- to the destination directory
--
-- @since 0.1.0.0
cpDirAsync
    :: CpOptions
    -> Path Abs Dir
    -> Path Abs Dir
    -> IO ()
cpDirAsync opt src dst = do
    S.drain $ asyncly $ copyDirToDir opt src dst


-- | Recursively and @parallely@ copies the source directory's contents
-- to the destination directory
--
-- @since 0.1.0.0
cpDirParallel
    :: CpOptions
    -> Path Abs Dir
    -> Path Abs Dir
    -> IO ()
cpDirParallel opt src dst = do
    S.drain $ parallely $ copyDirToDir opt src dst
