-- |
-- Module      : Streamly.Coreutils.Mv
-- Copyright   : (c) 2022 Composewell Technologies
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Rename source to dest, or move source(s) to directory.

module Streamly.Coreutils.Mv
    (
      mv
    , mvMany

    -- * Options
    , MvOptions
    , Overwrite (..)
    , overwrite
    , intoDirectory
    )
where

import Control.Exception (IOException, try)
import Control.Monad (unless, void, when)
import Data.Either (lefts)
import System.Directory (canonicalizePath, renamePath)

import qualified System.PosixCompat.Files as Files

import Streamly.FileSystem.Path (Path)
import qualified Streamly.FileSystem.Path as Path

import Streamly.Coreutils.FileTest
    (and_, doesItExist, isDir, isSymLink, not_, sameFileAs, test, testl)
import Streamly.Coreutils.Overwrite
    (Overwrite(..), destinationExistsError, shouldOverwrite, targetInDir)
import Streamly.Coreutils.Rename (renameNoReplace)

-- = Design notes
--
-- * 'OverwriteNever' and 'OverwriteError' use 'renameNoReplace', which is
--   atomic on Linux, macOS and Windows, like GNU @mv@. 'OverwriteOnly' and
--   'OverwriteUpdate' check the destination before 'renamePath' is called,
--   so the destination can change in between; GNU @mv -u@ has the same
--   race.
--
-- * 'mv' takes one source and 'mvMany' takes a list, instead of one function
--   taking a list: with one source the destination may be a new name, with
--   several it must be an existing directory.
--
-- * 'mvMany' raises one 'userError' listing every failure instead of
--   rethrowing the first 'IOException': with several failures there is no
--   single error type to keep. A caller that needs to handle each failure
--   can call 'mv' per source.

-- | Options for 'mv' and 'mvMany'.
data MvOptions = MvOptions
    { mvOverwrite :: Overwrite
    , mvIntoDirectory :: Bool
    }

defaultConfig :: MvOptions
defaultConfig = MvOptions
    { mvOverwrite = OverwriteAlways
    , mvIntoDirectory = True
    }

-- | Specify what to do when the destination exists. See 'Overwrite'.
--
-- A symlink at the destination counts as an existing destination even if
-- its target does not exist. 'OverwriteUpdate' compares the modification
-- time of the source with that of the symlink, not of its target, like GNU
-- @mv -u@. A symlink at the source is not followed either.
--
-- 'mv' never asks the user before replacing a destination. The default,
-- 'OverwriteAlways', is equivalent to GNU @mv -f@. GNU @mv@ without @-f@
-- asks before replacing a destination that is not writable, but only when
-- the input is a terminal; otherwise it replaces the destination, like
-- @mv -f@.
--
-- GNU @mv -i@ asks before replacing any existing destination. 'mv' does
-- not ask; use 'OverwriteError' to keep every existing destination and
-- raise an error, 'OverwriteNever' to keep it without an error, or
-- 'OverwriteUpdate' to replace only a destination older than the source.
--
-- With 'OverwriteNever' and 'OverwriteError' the check and the rename are one
-- atomic operation on Linux, macOS and Windows, so a destination created
-- concurrently is never replaced. On FreeBSD, and on file systems that do not
-- support it, the destination is checked first and a destination created
-- between the check and the rename is replaced.
--
-- Default (without this modifier): 'OverwriteAlways', like GNU @mv@.
overwrite :: Overwrite -> MvOptions -> MvOptions
overwrite opt cfg = cfg {mvOverwrite = opt}

-- | The destination of 'mv' can be a file or a directory. When the
-- destination is an existing directory, or a symlink to one, 'mv' can
-- either move the source into the directory or rename the source to the
-- directory's name. 'intoDirectory' chooses between the two.
--
-- With 'True', @mv id "a" "d"@ moves @a@ into the directory @d@, to @d/a@.
--
-- With 'False', @mv (intoDirectory False) "a" "d"@ renames @a@ to @d@,
-- like GNU @mv -T@. On POSIX systems the rename succeeds only if @a@ is
-- also a directory and @d@ is empty; @d@ is then replaced by @a@.
-- When @d@ is a symlink to a directory and @a@ is not a directory, GNU
-- @mv -T@ replaces the symlink with @a@. 'mv' does the same on POSIX
-- systems, but on Windows the rename fails because @MoveFileEx@ cannot
-- replace a directory symlink with a file.
--
-- When the destination does not exist or is not a directory, 'mv' renames
-- the source to the destination and ignores this option. 'mvMany' always
-- moves the sources into the directory and ignores this option.
--
-- Default (without this modifier): 'True', like GNU @mv@.
intoDirectory :: Bool -> MvOptions -> MvOptions
intoDirectory opt cfg = cfg {mvIntoDirectory = opt}

-- GNU mv refuses to move a file onto itself (same_file_ok in GNU copy.c).
-- Without the check, rename(2) on two hard links of the same file succeeds and
-- does nothing, and renaming a symlink onto its target replaces the only name
-- of the target with a symlink to itself. A symlink at dest is replaced like
-- any other file. A symlink at src may replace a hard link of its target if
-- the link it resolves to is another one, which remains.

-- | True if moving src to dest would move a file onto itself.
isSameFile :: Path -> Path -> IO Bool
isSameFile src dest = do
    destIsFile <- testl dest (doesItExist `and_` not_ isSymLink)
    same <-
        if destIsFile
        then test src (doesItExist `and_` sameFileAs dest)
        else return False
    if same
    then do
        srcIsLink <- testl src isSymLink
        if srcIsLink
        then do
            st <- Files.getSymbolicLinkStatus (Path.toString dest)
            -- dest is the only name of the file; moving would lose it.
            if Files.linkCount st < 2
            then return True
            else
                -- True if src resolves to the path dest itself; if it
                -- resolves to another hard link of dest, the file survives
                -- the move.
                (==) <$> canonicalizePath (Path.toString src)
                     <*> canonicalizePath (Path.toString dest)
        else return True
    else return False

-- Rename src to dest, subject to the overwrite option.
moveTo :: MvOptions -> Path -> Path -> IO ()
moveTo opt src dest =
    case mvOverwrite opt of
        OverwriteNever -> void $ renameNoReplace src dest
        OverwriteError -> do
            r <- renameNoReplace src dest
            unless r $ ioError (destinationExistsError dest)
        option -> do
            same <- isSameFile src dest
            when same
                $ ioError $ userError
                $ "mv: '" ++ Path.toString src ++ "' and '"
                    ++ Path.toString dest ++ "' are the same file"
            r <- shouldOverwrite False option src dest
            when r $ renamePath (Path.toString src) (Path.toString dest)

-- | @mvMany modifier sources dir@ moves each of @sources@ into the existing
-- directory @dir@, like GNU @mv -t dir sources...@. Pass 'id' for defaults;
-- each modifier documents its own default.
--
-- Raises an 'IOError' without moving anything if @dir@ is not a directory
-- or a symlink to one. A source that fails to move does not stop the
-- others, like GNU @mv@; after all sources are tried, an 'IOError' listing
-- each failure is raised.
mvMany :: (MvOptions -> MvOptions) -> [Path] -> Path -> IO ()
mvMany f srcs dir = do
    let opt = f defaultConfig
        dirStr = Path.toString dir
    dirIsDir <- test dir (doesItExist `and_` isDir)
    unless dirIsDir $
        ioError (userError ("mv: target is not a directory: " ++ dirStr))
    results <- mapM (moveOne opt) srcs
    case lefts results of
        [] -> return ()
        errs -> ioError (userError (unlines (map show errs)))

    where

    moveOne :: MvOptions -> Path -> IO (Either IOException ())
    moveOne opt src = try (targetInDir dir src >>= moveTo opt src)

-- | @mv modifier source dest@ renames @source@ to @dest@. If @dest@ is an
-- existing directory, or a symlink to one, @source@ is moved into it; see
-- 'intoDirectory'. Pass 'id' for defaults; each modifier documents its own
-- default.
--
-- A symlink at @source@ is moved itself, not the file it points to, like GNU
-- @mv@. Moving a file onto itself raises an exception, like GNU @mv@: when
-- @source@ and @dest@ are hard links of one file, or @source@ is a symlink
-- to @dest@. With 'OverwriteNever' the move is skipped instead.
mv :: (MvOptions -> MvOptions) -> Path -> Path -> IO ()
mv f src dest = do
    let opt = f defaultConfig
    destIsDir <-
        if mvIntoDirectory opt
        then test dest (doesItExist `and_` isDir)
        else return False
    if destIsDir
    then targetInDir dest src >>= moveTo opt src
    else moveTo opt src dest
