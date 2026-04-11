-- |
-- Module      : Streamly.Coreutils.Rm
-- Copyright   : (c) 2022 Composewell Technologies
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Deletes a file or directory. Like the Posix rm utility but safer and easier
-- to reason about.
--
-- This module provides two primary entry points to handle the ambiguity of
-- "trailing slashes" in the @rm@ utility.
--
-- 1. 'rm': Operates strictly on the path provided. It never follows symbolic
--    links. If you point it at a link, the link is removed, not the target.
--
-- 2. 'rmContents': Removes the /contents/ of the target container but not the
-- container itself. The container may be a dir or a symlink to a dir.
--
-- = Idioms
--
-- Equivalent to @rm -f@, removes readonly files as well:
--
--  >>> rmf = rm (withForce Force)
--
-- Equivalent to @rm -r@, removes directories with writable files:
--
--  >>> rmr = rm (recursive True . withForce Force)
--
-- Equivalent to @rm -rf@, removes directories with readonly files as well,
-- directories must have write permissions:
--
--  >>> rmrf = rm (recursive True . withForce Force)
--
-- Removes directories without write permissions as well:
--
--  >>> rmrff = rm (recursive True . withForce FullForce)
--
-- Equivalent to @rm -rf dirSymLink/@, removing the contents but leaving the
-- directory intact:
--
--  >>> rmrc = rmContents Force
--
-- = Comparison with Posix @rm@
--
-- The behavior of posix rm and the rm in this module is identical except one
-- case -- when the path has trailing slash in recursive mode. The difference
-- is intentional to make it safer.
--
-- * Non-recursive case is identical to posix rm:
--
--     * Deletes files and symbolic links.
--     * Directories return an error.
--
-- * Recursive case when path is not a symlink or a symlink without a trailing slash, is identical to posix rm:
--
--     * Deletes the entire directory tree.
--     * Symbolic links encountered __during traversal__ are deleted as
--       files; their targets are never followed.
--     * If the __source argument__ is a symbolic link, only the link is
--       deleted; the target is untouched.
--
-- * Recursive case when path is symlink with a trailing slash (@rm -rf dir/@) is different from posix rm:
--
--     * rm in this module just deletes the symlink.
--     * Posix rm deletes the contents of the symlink dir, this is equivalent
--       to 'rmContents' in this module.

-- TODO: replace the error calls with exceptions

module Streamly.Coreutils.Rm
    ( rm
    , rmContents

    -- * Options
    , RmOptions
    , RmForce(..)
    , withForce
    , recursive

    -- * Deprecated
    , force
    )
where

import Control.Monad (forM_, when)
import Streamly.Coreutils.FileTest
    (doesExist, test, testl, isDir, isWritableByMode)
#if defined(mingw32_HOST_OS)
import Streamly.Coreutils.FileTest.Windows (isDirSymLink)
#endif
import System.Directory
    ( getPermissions
    , removeFile
    , removeDirectory
    , removeDirectoryRecursive
    , removePathForcibly
    , setPermissions
    , listDirectory
    , writable
    )
import System.FilePath ((</>))

-- TODO: backward compatibility for Rm, None, Nuke changes.

-- | Defines how @rm@ handles missing paths and permission obstacles.
--
-- On Unix, deleting a file requires write permission on its parent directory,
-- not on the file itself. On Windows, it requires the file's own read-only
-- attribute to be unset.
data RmForce
    = NoForce
    -- ^ Default. Errors if a path is missing. Errors if a file or directory
    -- is write-protected (on Unix: checked via mode bits; on Windows: checked
    -- via the read-only attribute).
    | Force
    -- ^ Mirrors @rm -f@. Silently ignores missing paths. Deletes
    -- write-protected files and directories where the OS permits it — that
    -- is, where the current user has write permission on the parent directory
    -- (Unix) or is not blocked by ACLs (Windows). Errors only on genuine
    -- permission failures.
    | FullForce
    -- ^ Extends @Force@. In recursive mode, also chmods directories to
    -- ensure their contents can be traversed and removed, even if the
    -- directory itself is write-protected. On Windows this is equivalent
    -- to @Force@ since directory mode bits are not enforced.

data RmOptions = RmOptions
    { rmForce :: RmForce
    , rmRecursive :: Bool
    }

defaultConfig :: RmOptions
defaultConfig = RmOptions
    { rmForce = NoForce
    , rmRecursive = False
    }

-- | Default is 'NoForce'. See 'RmForce'.
withForce :: RmForce -> RmOptions -> RmOptions
withForce val cfg = cfg { rmForce = val }

{-# DEPRECATED force "Use withForce instead" #-}
force :: RmForce -> RmOptions -> RmOptions
force = withForce

-- | Default is 'False'. If 'True', 'rm' removes directories and their trees.
-- Symbolic links are never followed during recursive traversal.
recursive :: Bool -> RmOptions -> RmOptions
recursive opt cfg = cfg { rmRecursive = opt }

-------------------------------------------------------------------------------
-- Internal
-------------------------------------------------------------------------------

withWriteProtectionCheck :: FilePath -> (FilePath -> IO b) -> [Char] -> IO b
withWriteProtectionCheck path f msg = do
    -- Note: there is an inherent TOCTOU race between this check and
    -- the call to the deletion function "f".
    writable <- testl path isWritableByMode
    if writable
    then f path
    else
        error
            $ "rm: cannot remove '"
            ++ path ++ "': write-protected " ++ msg

rmdir :: RmOptions -> FilePath -> IO ()
rmdir options path =
    case rmRecursive options of
        False ->
            error $ "rm: cannot remove '" ++ path ++ "': Is a directory"
        True ->
            case rmForce options of
                FullForce -> removePathForcibly path
                Force ->
#if defined(mingw32_HOST_OS)
                    -- On Unix removePathForcibly makes directories writable to
                    -- facilitate removal of files in them, but on Windows
                    -- directory attributes do not affect file deletion, it
                    -- changes the FILE_ATTRIBUTE_READONLY on files to make
                    -- them deletable. This is exactly what we want for the
                    -- Force option. So FullForce and Force are essentially
                    -- same on Windows.
                    removePathForcibly path
#else
                    removeDirectoryRecursive path
#endif
                NoForce -> do
                    contents <- listDirectory path
                    withWriteProtectionCheck path (const (pure ())) "directory"
                    forM_ contents $ \item ->
                        rm (withForce (rmForce options) . recursive True)
                           (path </> item)
                    withWriteProtectionCheck path removeDirectory "directory"

-- XXX implement and use "chmod"

-- | Make a path writable by the owner. Used on Windows before deletion to
-- clear FILE_ATTRIBUTE_READONLY. Compiled on all platforms so that it is
-- tested on Unix too, but only called on Windows.
_setWritable :: FilePath -> IO ()
_setWritable path = do
    perms <- getPermissions path
    when (not (writable perms)) $
        setPermissions path (perms { writable = True })

rmfile :: RmOptions -> FilePath -> IO ()
rmfile options path =
    case rmForce options of
        FullForce -> removePathForcibly path
        NoForce   -> withWriteProtectionCheck path removeFile "regular file"
        Force     -> do
#if defined(mingw32_HOST_OS)
            -- On Windows, file deletability is tied to the file's own
            -- read-only attribute (unlike POSIX where only parent-dir write
            -- matters). Force must clear it before unlinking.
            _setWritable path
#endif
            removeFile path

performRm :: RmOptions -> FilePath -> IO ()
performRm options path = do
    -- isDir returns false if path is symlink
    dir <- testl path isDir
    if dir
    then rmdir options path
    else do
#if defined(mingw32_HOST_OS)
        dirSymLink <- testl path isDirSymLink
        if dirSymLink
        then do
            case rmForce options of
                FullForce -> _setWritable path
                Force     -> _setWritable path
                NoForce   -> return ()
            removeDirectory path
        else rmfile options path
#else
        rmfile options path
#endif

-------------------------------------------------------------------------------
-- API
-------------------------------------------------------------------------------

-- | Removes a file, symlink, or directory.
--
-- This function never follows symbolic links. If the path is a symlink,
-- the link itself is removed regardless of whether it points to a
-- directory or a file.
--
-- Note: When 'recursive' is 'False' (the default), passing a directory path
-- always results in an error, even under 'FullForce'.
rm :: (RmOptions -> RmOptions) -> FilePath -> IO ()
rm f path = do
    let options = f defaultConfig
    -- Note this test is required not just for existence check but also so that
    -- we fail if there is no permission to access the path.
    --
    found <- testl path doesExist
    if found
    then performRm options path
    else
        case rmForce options of
           NoForce ->
               error $ "rm: cannot remove '" ++ path
                      ++ "': No such file or directory"
           _ -> return ()

-- | Recursively removes everything *inside* the given path, while
-- preserving the path itself.
--
-- This function treats the source path as a "container." It resolves
-- the path to its target (following a symlink if necessary) and
-- removes all children. Removal stops at the first failure; any items
-- after the failing entry are left untouched.
--
-- * If the path is a directory: The directory is emptied.
-- * If the path is a symlink: The target directory is emptied;
--   the link and the target directory are preserved.
--
-- @Note:@ Under 'NoForce', this fails if the path does not exist, is a
-- broken symlink, or is a regular file. Under 'Force', these cases
-- are silent no-ops.
rmContents :: RmForce -> FilePath -> IO ()
rmContents flevel path = do
    -- 'test' follows symlinks, unlike 'testl'.
    -- This checks if the *target* exists and is a directory.
    isTargetDir <- test path isDir
    if isTargetDir
    then
        do
            contents <- listDirectory path
            forM_ contents $ \item ->
                rm (withForce flevel . recursive True) (path </> item)
    else
        case flevel of
            NoForce ->
                error $ "rmContents: cannot access '" ++ path
                       ++ "': Not a directory or broken symlink"
            _ ->
                return ()
