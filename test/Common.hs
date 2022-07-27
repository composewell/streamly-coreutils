module Common
    ( createParent
    , createDirWithParent
    , createDir
    , createFileWithParent
    , createFile
    )
where

import Control.Monad (unless)
import System.Directory (createDirectory, createDirectoryIfMissing)
import System.FilePath ((</>), takeDirectory)
import System.IO ( IOMode (WriteMode), openFile, hClose)

createParent :: FilePath -> FilePath -> IO ()
createParent file parent = do
    createDirectoryIfMissing True (parent </> takeDirectory file)

createDirWithParent :: FilePath -> FilePath -> IO ()
createDirWithParent dir parent =
    unless (null dir) $ createDirectoryIfMissing True (parent </> dir)

createDir :: FilePath -> FilePath -> IO ()
createDir dir parent =
    unless (null dir) $ createDirectory (parent </> dir)

createFileWithParent :: FilePath -> FilePath -> IO ()
createFileWithParent file parent = do
    unless (null file) $
        createDirectoryIfMissing True (parent </> takeDirectory file)
    openFile (parent </> file) WriteMode >>= hClose

createFile :: FilePath -> FilePath -> IO ()
createFile file parent =
    openFile (parent </> file) WriteMode >>= hClose
