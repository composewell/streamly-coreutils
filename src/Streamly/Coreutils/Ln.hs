{-# LANGUAGE CPP #-}
-- |
-- Module      : Streamly.Coreutils.Ln
-- Copyright   : (c) 2022 Composewell Technologies
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- create a link to TARGET with the given name

module Streamly.Coreutils.Ln
    ( ln

    -- * Options
    , LnOptions
    , force
    , symbolic
    )
where

import Control.Monad (unless, when)
import Data.Maybe (fromMaybe)
import Foreign.C.Error (eEXIST)
import GHC.IO.Exception (IOErrorType(InvalidArgument))
import Streamly.Coreutils.FileTest (test, testl, doesItExist, sameFileAs)
import System.Directory (removeFile)
#if defined(CABAL_OS_WINDOWS)
import Streamly.Coreutils.FileTest (isDir)
import Streamly.Coreutils.FileTest.Windows (isDirSymLink)
import System.Directory
    (createDirectoryLink, createFileLink, removeDirectory)
#endif

import qualified System.PosixCompat.Files as Posix
import Streamly.FileSystem.Path (Path)
import qualified Streamly.FileSystem.Path as Path

import Streamly.Coreutils.Error (commandError, errnoError)

data LnOptions = LnOptions
    { lnForce :: Bool
    , lnSymbolic :: Bool
    }

defaultConfig :: LnOptions
defaultConfig = LnOptions False False

-- | Remove an existing destination before creating the link, like @ln -f@.
-- Raises an 'IOError' of type 'InvalidArgument' if the source and the
-- destination are the same file.
--
-- Default (without this modifier): 'False', raise an exception if the
-- destination exists.
force :: Bool -> LnOptions -> LnOptions
force opt cfg = cfg {lnForce = opt}

-- | Create a symbolic link instead of a hard link, like @ln -s@. The source
-- path is stored in the link as given, so a relative source is relative to
-- the directory of the link, not to the current directory.
--
-- On Windows a symbolic link is either a file link or a directory link. A
-- directory link is created if the source is a directory, otherwise a file
-- link, including when the source does not exist.
--
-- Default (without this modifier): 'False', create a hard link.
symbolic :: Bool -> LnOptions -> LnOptions
symbolic opt cfg = cfg {lnSymbolic = opt}

#if defined(CABAL_OS_WINDOWS)
-- A symbolic link to a directory is a directory entry on Windows:
-- removeFile fails on it, removeDirectory removes the link and not the
-- directory it points to.

-- | Remove a file or a symbolic link. A symbolic link is not followed.
removeEntry :: Path -> IO ()
removeEntry path = do
    dirSymLink <- testl path isDirSymLink
    if dirSymLink
    then removeDirectory pathStr
    else removeFile pathStr

    where

    pathStr = Path.toString path

-- unix-compat does not support symbolic links on Windows. The source is
-- tested relative to the directory of the link, the way the OS resolves
-- the link target.

-- | @createSymbolicLink src tgt@ creates a symbolic link @tgt@ pointing to
-- @src@.
createSymbolicLink :: Path -> Path -> IO ()
createSymbolicLink src tgt = do
    srcIsDir <- test resolved isDir
    if srcIsDir
    then createDirectoryLink srcStr tgtStr
    else createFileLink srcStr tgtStr

    where

    srcStr = Path.toString src
    tgtStr = Path.toString tgt
    resolved =
        case Path.splitFile tgt of
            Just (Just dir, _) | not (Path.isRooted src) -> Path.join dir src
            _ -> src
#else
-- | Remove a file or a symbolic link. A symbolic link is not followed.
removeEntry :: Path -> IO ()
removeEntry = removeFile . Path.toString

-- | @createSymbolicLink src tgt@ creates a symbolic link @tgt@ pointing to
-- @src@.
createSymbolicLink :: Path -> Path -> IO ()
createSymbolicLink src tgt =
    Posix.createSymbolicLink (Path.toString src) (Path.toString tgt)
#endif

-- | @ln modifier source dest@ creates a link @dest@ to @source@, like GNU
-- @ln@. Pass 'id' for defaults; each modifier documents its own default.
--
-- Raises an 'IOError' with errno @EEXIST@
-- ('System.IO.Error.isAlreadyExistsError') if @dest@ exists, unless 'force'
-- is 'True'. A failure to create the link raises the 'IOError' of the
-- system call.
ln :: (LnOptions -> LnOptions) -> Path -> Path -> IO ()
ln f src tgt = do
    let opt = f defaultConfig
    -- target can be a symlink or non-symlink
    found <- testl tgt doesItExist
    when found $ do
        unless (lnForce opt) $ ioError (errnoError "ln" eEXIST tgt)
        -- Do not remove the source itself.
        srcFound <- test src doesItExist
        same <- if srcFound then isSameEntry else pure False
        when same
            $ ioError
            $ commandError InvalidArgument "ln" sameMsg (Just src)
        removeEntry tgt
    case lnSymbolic opt of
        False -> Posix.createLink srcStr tgtStr
        True -> createSymbolicLink src tgt

    where

    tgtStr = Path.toString tgt
    srcStr = Path.toString src

    -- Same directory entry: same inode, same parent directory and same name.
    -- A path without a file name part ("x/", ".", "..") is treated as the
    -- same entry when the inodes match, so that the source is never removed.
    isSameEntry = do
        sameInode <- testl tgt (sameFileAs src)
        case (Path.splitFile src, Path.splitFile tgt) of
            (Just (srcDir, srcName), Just (tgtDir, tgtName)) -> do
                sameDir <- test (dirOrCwd tgtDir) (sameFileAs (dirOrCwd srcDir))
                pure
                    $ sameInode
                    && sameDir
                    && Path.eqPath id srcName tgtName
            _ -> pure sameInode

    dirOrCwd = fromMaybe (Path.fromString_ ".")

    sameMsg = "'" ++ srcStr ++ "' and '" ++ tgtStr ++ "' are the same file"
