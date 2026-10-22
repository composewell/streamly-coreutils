-- |
-- Module      : Streamly.Coreutils.Rename
-- Copyright   : (c) 2026 Composewell Technologies
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Rename without replacing an existing destination.

module Streamly.Coreutils.Rename
    ( renameNoReplace
    )
where

import Streamly.FileSystem.Path (Path)
import qualified Streamly.FileSystem.Path as Path

#if defined(CABAL_OS_WINDOWS)
import Data.List (isPrefixOf)
import Foreign.C.String (CWString, withCWString)
import System.IO.Error (catchIOError)
import System.Win32.Info (getFullPathName)
import System.Win32.Types (DWORD, failWith)

import Streamly.Coreutils.FileTest
    (doesItExist, sameFileAs, test, testl)
#else
import Foreign.C.Error
    (Errno, eEXIST, eINVAL, eNOSYS, eNOTSUP, errnoToIOError, getErrno)
import Foreign.C.String (CString)
import Foreign.C.Types (CInt(..))
import System.Directory (renamePath)

import qualified Streamly.Internal.Data.Array as Array (asCStringUnsafe)

import Streamly.Coreutils.FileTest (doesItExist, testl)
#endif

#if defined(CABAL_OS_WINDOWS)
-- Like furnishPath in directory, which renamePath uses: make the path
-- absolute and prepend \\?\, or \\?\UNC\ for \\server\share, so that
-- paths longer than MAX_PATH work. GetFullPathNameW also converts / to \ and
-- resolves . and .., which the Windows API does not do for \\?\ paths.
-- If GetFullPathNameW fails, the path is used unchanged.
toExtendedLengthPath :: String -> IO String
toExtendedLengthPath path
    | any (`isPrefixOf` path) ["\\\\?\\", "\\??\\", "\\\\.\\"] =
        return path
    | otherwise =
        fmap addPrefix (getFullPathName path)
            `catchIOError` \_ -> return path

    where

    addPrefix full =
        case full of
            '\\' : '\\' : rest -> "\\\\?\\UNC\\" ++ rest
            _ -> "\\\\?\\" ++ full

foreign import ccall safe "streamly_coreutils_rename_noreplace_w"
    c_renameNoReplaceW :: CWString -> CWString -> IO DWORD

-- ERROR_FILE_EXISTS, ERROR_ALREADY_EXISTS
isExistsError :: DWORD -> Bool
isExistsError e = e == 80 || e == 183
#else
foreign import ccall safe "streamly_coreutils_rename_noreplace"
    c_renameNoReplace :: CString -> CString -> IO CInt

-- Not atomic: a destination created between the check and the rename is
-- replaced.
renameCheckFirst :: Path -> Path -> IO Bool
renameCheckFirst src dest = do
    exists <- testl dest doesItExist
    if exists
    then return False
    else do
        renamePath (Path.toString src) (Path.toString dest)
        return True

-- Errors meaning that the platform or the file system does not support an
-- atomic no-replace rename. Linux returns EINVAL when the file system does
-- not support RENAME_NOREPLACE, macOS returns ENOTSUP, FreeBSD returns
-- EOPNOTSUPP, which is the same value as ENOTSUP.
isUnsupported :: Errno -> Bool
isUnsupported e = e == eNOSYS || e == eINVAL || e == eNOTSUP
#endif

-- | Rename @src@ to @dest@ unless @dest@ exists. Returns 'False', without
-- renaming, if @dest@ exists; a symlink at @dest@ counts as existing even if
-- its target does not.
--
-- Atomic on Linux (@renameat2@ with @RENAME_NOREPLACE@), macOS (@renamex_np@
-- with @RENAME_EXCL@), FreeBSD 15.1 and later (@renameat2@ with
-- @AT_RENAME_NOREPLACE@) and Windows (@MoveFileExW@ without
-- @MOVEFILE_REPLACE_EXISTING@). On other platforms, on older FreeBSD
-- releases, and on file systems that do not support these calls, @dest@ is
-- checked before the rename, so a @dest@ created in between is replaced.
renameNoReplace :: Path -> Path -> IO Bool
#if defined(CABAL_OS_WINDOWS)
renameNoReplace src dest = do
    -- MoveFileExW without MOVEFILE_REPLACE_EXISTING still renames src when
    -- dest is a hard link of the same file, which removes the name src.
    srcExists <- test src doesItExist
    sameFile <-
        if srcExists
        then testl dest (sameFileAs src)
        else return False
    if sameFile
    then return False
    else do
        srcStr <- toExtendedLengthPath (Path.toString src)
        destStr <- toExtendedLengthPath (Path.toString dest)
        r <- withCWString srcStr $ \s ->
            withCWString destStr $ \d ->
                c_renameNoReplaceW s d
        if r == 0
        then return True
        else
            if isExistsError r
            then return False
            else failWith "renameNoReplace" r
#else
renameNoReplace src dest = do
    r <- Array.asCStringUnsafe (Path.toArray src) $ \s ->
        Array.asCStringUnsafe (Path.toArray dest) $ \d ->
            c_renameNoReplace s d
    if r == 0
    then return True
    else do
        errno <- getErrno
        if errno == eEXIST
        then return False
        else if isUnsupported errno
        then renameCheckFirst src dest
        else
            ioError
                $ errnoToIOError
                    "renameNoReplace" errno Nothing (Just (Path.toString src))
#endif
