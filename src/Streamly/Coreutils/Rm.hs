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
--  >>> rmf = rm (force True)
--
-- Equivalent to @rm -r@, removes directories with writable files:
--
--  >>> rmr = rm (recursive True)
--
-- Equivalent to @rm -rf@, removes directories with readonly files as well,
-- directories must have write permissions:
--
--  >>> rmrf = rm (recursive True . force True)
--
-- Removes directories without write permissions as well:
--
--  >>> rmrff = rm (recursive True . force True . makeDirsWritable True)
--
-- Equivalent to @rm -rf dirSymLink/@, removing the contents but leaving the
-- directory intact:
--
--  >>> rmcf = rmContents (force True)
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

module Streamly.Coreutils.Rm
    ( rm
    , rmContents

    -- * Options
    , RmOptions
    , force
    , makeDirsWritable
    , recursive
    )
where

import Control.Monad (forM_, unless, when)
import Foreign.C.Error (eACCES, eISDIR, eNOENT, eNOTDIR)
#if !defined(CABAL_OS_WINDOWS)
import Streamly.Coreutils.Chmod (Perm(..), addOwner, chmod)
#endif
import System.IO.Error (ioeSetErrorString)
import Streamly.Coreutils.FileTest
    ( pathExists
    , test
    , testl
    , isDir
    , isWritableByMode
#if !defined(CABAL_OS_WINDOWS)
    , isWritable
#endif
    )
#if defined(CABAL_OS_WINDOWS)
import Streamly.Coreutils.FileTest.Windows (isDirSymLink)
#endif
import System.Directory
    ( getPermissions
    , removeFile
    , removeDirectory
#if defined(CABAL_OS_WINDOWS)
    , removePathForcibly
#else
    , readable
    , removeDirectoryRecursive
    , searchable
#endif
    , setPermissions
    , listDirectory
    , writable
    )
import System.FilePath
    ( (</>)
#if !defined(CABAL_OS_WINDOWS)
    , dropTrailingPathSeparator
    , takeDirectory
#endif
    )
import Streamly.FileSystem.Path (Path)
import qualified Streamly.FileSystem.Path as Path

import Streamly.Coreutils.Error (errnoError)

-- = Design notes
--
-- * Force options. GNU @rm -f@ is 'force' :: Bool, the same name and type
--   as @Ln.force@. 'force' is ignoreMissing plus "do not refuse a
--   write-protected entry"; it applies to each file or directory being
--   removed and does not change its mode on Unix. 'makeDirsWritable'
--   applies to the contents of the directories inside the tree being
--   removed. The two are independent, and all four combinations are
--   meaningful.
--
-- * Rejected: a sum type @NoForce | Force | FullForce@. @force Force@
--   repeats the word, and @withForce Force@ was the only modifier in the
--   package with a @with@ prefix. Numbered levels (@ForceLevel1@) are
--   opaque. Two exclusive modifiers (@force@, @fullForce@) setting one field
--   do not show in the type that they are exclusive.
--
-- * The name 'makeDirsWritable' says that permissions are changed, unlike
--   @ignoreDirPermissions@. @forceDirs@ reads like "remove directories",
--   and @forceRecursive@ / @forceTree@ suggest that it implies 'recursive'.
--
-- * 'makeDirsWritable' never changes the parent of the path. A change to
--   the parent would outlive the call and weaken a protection its owner
--   chose; GNU @rm@ does not change it either. Use chmod explicitly.
--
-- * 'makeDirsWritable' checks that the parent is writable before changing
--   any permission. Without the check, removing @a/b@ with @a@
--   write-protected empties @b@, leaves @b@ writable, and then fails with
--   EACCES (this is what 'removePathForcibly' does). The check is best
--   effort: it does not cover the sticky bit or a concurrent change.

data RmOptions = RmOptions
    { rmForce :: Bool
    , rmMakeDirsWritable :: Bool
    , rmRecursive :: Bool
    }

defaultConfig :: RmOptions
defaultConfig = RmOptions
    { rmForce = False
    , rmMakeDirsWritable = False
    , rmRecursive = False
    }

-- | Like @rm -f@. This combines two behaviours:
--
-- * ignore missing paths, and
-- * remove write-protected files and directories instead of raising an
--   exception. On Unix, the mode of the file or directory is not changed:
--   removing an entry needs write permission on its parent, not on the
--   entry. On Windows, the read-only attribute is cleared, because it
--   prevents deletion.
--
-- Removal can still fail with the error of the system call, e.g. without
-- write permission on the parent (Unix) or when blocked by an ACL
-- (Windows). 'force' does not make the contents of a write-protected
-- directory removable; that is 'makeDirsWritable'.
--
-- Default (without this modifier): 'False', raise an exception for a
-- missing path or a write-protected file or directory.
force :: Bool -> RmOptions -> RmOptions
force opt cfg = cfg { rmForce = opt }

-- | In recursive mode, give the owner read, write and execute permission on
-- the directories in the tree being removed, so that their contents can be
-- removed. Unlike 'force', which applies to each file or directory being
-- removed, this applies to the contents of a directory. The parent
-- directory of the path is not changed: if it is not writable, an exception
-- with errno @EACCES@ is raised before any permission is changed. The check
-- does not cover the sticky bit or a concurrent change of the parent. If
-- removal fails later, directories that were changed keep their new
-- permissions.
--
-- Does not imply 'force': without it, a missing path or a write-protected
-- file still raises an exception. Has no effect without 'recursive' 'True',
-- or on Windows, where a directory's read-only attribute does not prevent
-- removing its contents.
--
-- Default (without this modifier): 'False', raise an exception for a
-- write-protected directory.
makeDirsWritable :: Bool -> RmOptions -> RmOptions
makeDirsWritable opt cfg = cfg { rmMakeDirsWritable = opt }

-- | If 'True', 'rm' removes directories and their trees. Symbolic links are
-- never followed during recursive traversal.
--
-- Default (without this modifier): 'False', raise an exception for a
-- directory.
recursive :: Bool -> RmOptions -> RmOptions
recursive opt cfg = cfg { rmRecursive = opt }

-------------------------------------------------------------------------------
-- Internal
-------------------------------------------------------------------------------

withWriteProtectionCheck :: Path -> (Path -> IO b) -> String -> IO b
withWriteProtectionCheck path f msg = do
    -- Note: there is an inherent TOCTOU race between this check and
    -- the call to the deletion function "f".
    writable <- testl path isWritableByMode
    if writable
    then f path
    else
        ioError
            $ ioeSetErrorString (errnoError "rm" eACCES path)
            $ "write-protected " ++ msg

#if !defined(CABAL_OS_WINDOWS)
-- | Raise an exception with errno @EACCES@ if the directory is not writable.
-- Used by 'makeDirsWritable' before changing any permission.
requireWritableDir :: Path -> IO ()
requireWritableDir dir = do
    ok <- test dir isWritable
    unless ok
        $ ioError
        $ ioeSetErrorString (errnoError "rm" eACCES dir)
            "directory not writable"

parentDir :: Path -> IO Path
parentDir path =
    Path.fromString
        $ takeDirectory
        $ dropTrailingPathSeparator
        $ Path.toString path

-- | Give the owner read, write and execute permission on a directory, like
-- @chmod u+rwx@. The mode is not changed if the directory is already
-- accessible, e.g. through its group permissions, so that a directory owned
-- by another user does not fail with EPERM.
makeDirAccessible :: Path -> IO ()
makeDirAccessible dir = do
    perms <- getPermissions (Path.toString dir)
    unless (readable perms && writable perms && searchable perms)
        $ chmod id (addOwner [Read, Write, Exec]) dir
#endif

-- | Remove each entry of a directory with the same options.
removeEntries :: RmOptions -> FilePath -> IO ()
removeEntries options dir = do
    contents <- listDirectory dir
    forM_ contents $ \item ->
        Path.fromString (dir </> item) >>= removePath options

rmdir :: RmOptions -> Path -> IO ()
rmdir options path
    | not (rmRecursive options) = ioError (errnoError "rm" eISDIR path)
#if !defined(CABAL_OS_WINDOWS)
    | rmMakeDirsWritable options = do
        makeDirAccessible path
        removeEntries options pathStr
        removeDirectory pathStr
#endif
    | rmForce options =
#if defined(CABAL_OS_WINDOWS)
        -- On Unix removePathForcibly makes directories writable to
        -- facilitate removal of files in them, but on Windows directory
        -- attributes do not affect file deletion, it changes the
        -- FILE_ATTRIBUTE_READONLY on files to make them deletable. This is
        -- exactly what we want for the force option.
        removePathForcibly pathStr
#else
        removeDirectoryRecursive pathStr
#endif
    | otherwise = do
        withWriteProtectionCheck path (const (pure ())) "directory"
        removeEntries options pathStr
        withWriteProtectionCheck
            path
            (removeDirectory . Path.toString)
            "directory"

    where

    pathStr = Path.toString path

-- | Make a path writable by the owner. Used on Windows before deletion to
-- clear FILE_ATTRIBUTE_READONLY. Compiled on all platforms so that it is
-- tested on Unix too, but only called on Windows.
_setWritable :: Path -> IO ()
_setWritable path = do
    let pathStr = Path.toString path
    perms <- getPermissions pathStr
    unless (writable perms) $
        setPermissions pathStr (perms { writable = True })

rmfile :: RmOptions -> Path -> IO ()
rmfile options path =
    let pathStr = Path.toString path
     in
    if rmForce options
    then do
#if defined(CABAL_OS_WINDOWS)
        -- On Windows, file deletability is tied to the file's own
        -- read-only attribute (unlike POSIX where only parent-dir write
        -- matters). Force must clear it before unlinking.
        _setWritable path
#endif
        removeFile pathStr
    else
        withWriteProtectionCheck
            path
            (removeFile . Path.toString)
            "regular file"

performRm :: RmOptions -> Path -> IO ()
performRm options path = do
    -- testl isDir returns False if path is a symlink.
    dir <- testl path isDir
    if dir
    then rmdir options path
    else do
#if defined(CABAL_OS_WINDOWS)
        dirSymLink <- testl path isDirSymLink
        if dirSymLink
        then do
            when (rmForce options) $ _setWritable path
            removeDirectory (Path.toString path)
        else rmfile options path
#else
        rmfile options path
#endif

-- | 'rm' without the parent check of 'makeDirsWritable', for the entries of
-- a tree whose parent is already known to be writable.
removePath :: RmOptions -> Path -> IO ()
removePath options path = do
    -- Note this test is required not just for existence check but also so that
    -- we fail if there is no permission to access the path.
    --
    found <- testl path pathExists
    if found
    then performRm options path
    else unless (rmForce options) $ ioError (errnoError "rm" eNOENT path)

-------------------------------------------------------------------------------
-- API
-------------------------------------------------------------------------------

-- | Removes a file, symlink, or directory. Pass 'id' for defaults; each
-- modifier documents its own default.
--
-- This function never follows symbolic links. If the path is a symlink,
-- the link itself is removed regardless of whether it points to a
-- directory or a file.
--
-- Raises an 'IOError' for each of the following, like GNU @rm@:
--
-- * a missing path without 'force', with errno @ENOENT@
--   ('System.IO.Error.isDoesNotExistError'),
-- * a directory when 'recursive' is 'False' (the default), even with
--   'force', with errno @EISDIR@,
-- * a write-protected file without 'force', or a write-protected directory
--   without 'makeDirsWritable', with errno @EACCES@
--   ('System.IO.Error.isPermissionError').
--
-- A failure to remove a path raises the 'IOError' of the system call.
rm :: (RmOptions -> RmOptions) -> Path -> IO ()
rm f path = do
    let options = f defaultConfig
#if !defined(CABAL_OS_WINDOWS)
    when (rmMakeDirsWritable options && rmRecursive options) $ do
        dir <- testl path isDir
        when dir $ parentDir path >>= requireWritableDir
#endif
    removePath options path

-- | Recursively removes everything *inside* the given path, while
-- preserving the path itself. 'recursive' is implied; the other options
-- are those of 'rm'. With 'makeDirsWritable', the path itself is not
-- changed: if it is not writable, an exception with errno @EACCES@ is raised
-- before any permission is changed.
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
-- Without 'force', raises an 'IOError' with errno @ENOENT@ if the path does
-- not exist or is a broken symlink, and with errno @ENOTDIR@ if it is not a
-- directory, like GNU @rm -r path/@. With 'force', these cases are silent
-- no-ops.
rmContents :: (RmOptions -> RmOptions) -> Path -> IO ()
rmContents f path = do
    let options = (f defaultConfig) { rmRecursive = True }
    -- 'test' follows symlinks, unlike 'testl'.
    -- This checks if the *target* exists and is a directory.
    isTargetDir <- test path isDir
    if isTargetDir
    then do
#if !defined(CABAL_OS_WINDOWS)
        when (rmMakeDirsWritable options) $ requireWritableDir path
#endif
        removeEntries options (Path.toString path)
    else
        unless (rmForce options) $ do
            found <- test path pathExists
            ioError
                $ errnoError "rm" (if found then eNOTDIR else eNOENT) path
