-- |
-- Module      : Streamly.Coreutils.Cp
-- Copyright   : (c) 2022 Composewell Technologies
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Copy a file or directory.

module Streamly.Coreutils.Cp
    ( cp

    -- * Cp options
    , CpOptions
    , Overwrite (..)
    -- , CpBackup
    , overwrite
    , CpMethod (CopyContents, HardLink)
    , cpMethod
    , intoDirectory
    )
where

import Control.Monad (when)
import Foreign.C.Error (eISDIR)
import GHC.IO.Exception (IOErrorType(InvalidArgument))
import System.IO.Error (ioeSetErrorString)
import System.Directory (canonicalizePath)
import System.PosixCompat.Files (createLink)
#if !defined(CABAL_OS_WINDOWS)
import Control.Exception (bracket)
import Data.Function ((&))
import System.Posix.Files
    (accessModes, fileMode, getFileStatus, intersectFileModes)
import System.Posix.IO (createFile, fdToHandle)
import qualified Streamly.Data.Stream as Stream
import qualified Streamly.Internal.FileSystem.Posix.File as OsFile
#else
import qualified Streamly.Internal.FileSystem.Windows.File as OsFile
#endif
import qualified System.IO as IO
import qualified Streamly.FileSystem.Handle as Handle
import qualified Streamly.Internal.FileSystem.FileIO as File
import Streamly.FileSystem.Path (Path)
import qualified Streamly.FileSystem.Path as Path

import Streamly.Coreutils.Error (commandError, errnoError)
import Streamly.Coreutils.FileTest
import Streamly.Coreutils.Overwrite
    (Overwrite(..), shouldOverwrite, targetInDir)

-- Note: Recursive copy can be done using find.
--
-- For modular copying, break down the cp operation in two parts, (1) use a
-- "touch" command to create a file with specified attrs, (2) use a copy
-- command to copy the contents from another file or source to an existing
-- destination. Then we can use find, touch and copy to do whatever we want.
--
-- "touch" can also support copying attributes from source file.
--
-- Ideally, cp should not hard link as we have ln for hard linking, but it can
-- be useful when we need to hard link recursively.

-- | When overwriting specify backup behavior.
-- newtype CpBackup = Backup (Maybe String)

-- SymbolicLink and CopyClone are not implemented; their constructors are not
-- exported until they are.

-- | How to copy the source to destination?
data CpMethod =
      CopyContents
    | HardLink      -- ^ Hard link the file a symlink at the source points
                    -- to, not the symlink, like GNU @cp -l@.
    | SymbolicLink
    | CopyClone     -- Use the cloning method if available on the platform

data CpOptions = CpOptions
    { optOverwrite :: Overwrite
    , optCopyMethod :: CpMethod
    , optIntoDirectory :: Bool
    }

defaultOptions :: CpOptions
defaultOptions = CpOptions
    { optOverwrite = OverwriteAlways
    , optCopyMethod = CopyContents
    , optIntoDirectory = True
    }

-- | Specify what to do when the destination exists. See 'Overwrite'.
--
-- Symlinks at the source and the destination are followed, like GNU @cp@:
-- a symlink at the destination is written through, and 'OverwriteUpdate'
-- compares the modification times of the files the symlinks point to, not
-- of the symlinks.
--
-- With 'CopyContents', a dangling symlink at the destination raises an
-- exception with every 'Overwrite' option, including 'OverwriteNever', like
-- GNU @cp@: @cp: not writing through dangling symlink@.
--
-- Default (without this modifier): 'OverwriteAlways', like GNU @cp@.
--
overwrite :: Overwrite -> CpOptions -> CpOptions
overwrite opt options = options { optOverwrite = opt }

-- | Specify the copy method.
--
-- Default is 'CopyContents'.
--
cpMethod :: CpMethod -> CpOptions -> CpOptions
cpMethod opt options = options { optCopyMethod = opt }

-- | The destination of 'cp' can be a file or a directory. When the
-- destination is an existing directory, or a symlink to one, 'cp' can
-- either copy the source into the directory or write the copy at the
-- directory's path. 'intoDirectory' chooses between the two.
--
-- With 'True', @cp id "a" "d"@ copies @a@ into the directory @d@, to @d/a@.
-- The name of the copy is the last component of the source path, ignoring
-- trailing separators.
--
-- With 'False', @cp (intoDirectory False) "a" "d"@ writes the copy at @d@,
-- like GNU @cp -T@. The source of 'cp' is never a directory, and a file
-- cannot replace a directory, so the copy raises an exception. An
-- 'Overwrite' option that skips an existing destination, such as
-- 'OverwriteNever', skips the copy without an exception.
--
-- When the destination does not exist or is not a directory, 'cp' copies
-- the source to the destination and ignores this option.
--
-- Default (without this modifier): 'True', like GNU @cp@.
--
intoDirectory :: Bool -> CpOptions -> CpOptions
intoDirectory opt options = options { optIntoDirectory = opt }

-- streamly-core has no stat and no way to give a new file a mode, so on
-- POSIX we use the FilePath based functions from unix. FileIO.withFile
-- returns a stream, which opens the file only when the stream is run, after
-- fromChunks has created the destination. So the source is opened with
-- withBinaryFile from the internal platform specific File module, which
-- runs an IO action.

-- | Copy the source to a destination that does not exist. On POSIX the
-- destination is created with the permission bits of the source; the process
-- umask applies. Like GNU cp without @--preserve@, special bits (setuid,
-- setgid, sticky) are not copied. The destination is written even if the new
-- mode is read-only.
--
-- The source is opened before the destination is created, like GNU cp, so a
-- missing or unreadable source leaves no destination behind.
copyToNew :: Path -> Path -> IO ()
copyToNew src dest =
    OsFile.withBinaryFile src IO.ReadMode $ \h ->
        writeNew (Handle.readChunks h)

    where

#if defined(CABAL_OS_WINDOWS)
    writeNew = File.fromChunks dest
#else
    writeNew chunks = do
        st <- getFileStatus (Path.toString src)
        let mode = intersectFileModes (fileMode st) accessModes
            create = createFile (Path.toString dest) mode >>= fdToHandle
        bracket create IO.hClose $ \h ->
            chunks & Stream.fold (Handle.writeChunks h)
#endif

-- GNU cp refuses to write through a dangling symlink unless POSIXLY_CORRECT
-- is set; POSIX allows it. We follow the GNU default.

-- | True if the path is a symlink whose target does not exist.
isDanglingSymLink :: Path -> IO Bool
isDanglingSymLink path = do
    isLink <- testl path isSymLink
    if isLink
    then not <$> test path doesItExist
    else return False

-- | Unconditionally copy the source to destination using the specified copy
-- method.
cpCopy :: CpMethod -> Path -> Path -> IO ()
cpCopy method src dest =
    case method of
        CopyContents -> do
            -- An existing destination keeps its own mode, like GNU cp.
            exists <- test dest doesItExist
            if exists
            then do
                -- Writing would truncate the source before it is read.
                same <- test src (sameFileAs dest)
                when same
                    $ ioError
                    $ commandError
                        InvalidArgument "cp"
                        "source and destination are the same file"
                        (Just src)
                File.fromChunks dest (File.readChunks src)
            else copyToNew src dest
        HardLink -> do
            -- link(2) does not follow a symlink on Linux; GNU cp -l uses
            -- linkat with AT_SYMLINK_FOLLOW.
            srcTarget <- canonicalizePath (Path.toString src)
            createLink srcTarget (Path.toString dest)
        -- Not exported, so unreachable.
        SymbolicLink -> error "cp: BUG: SymbolicLink is not implemented"
        CopyClone -> error "cp: BUG: CopyClone is not implemented"

-- | @cp option-modifier source destination@. Copy a file.
--
-- A symlink at @source@ is followed: the file it points to is copied, like
-- GNU @cp@ without @-P@. A directory @source@, or a symlink to one, raises an
-- 'IOError' with errno @EISDIR@, like GNU @cp@ without @-r@. Copying a file
-- onto itself raises an 'IOError' of type
-- 'GHC.IO.Exception.InvalidArgument'.
--
-- If @destination@ is an existing directory, or a symlink to one, @source@
-- is copied into it; see 'intoDirectory'. Pass 'id' for defaults; each
-- modifier documents its own default.
cp :: (CpOptions -> CpOptions) -> Path -> Path -> IO ()
cp f src dest = do
    let options = f defaultOptions
    srcIsDir <- test src isDir
    when srcIsDir
        $ ioError
        $ ioeSetErrorString (errnoError "cp" eISDIR src)
            "-r not specified; omitting directory"
    destIsDir <-
        if optIntoDirectory options
        then test dest isDir
        else return False
    target <- if destIsDir then targetInDir "cp" dest src else return dest
    case optCopyMethod options of
        CopyContents -> do
            dangling <- isDanglingSymLink target
            when dangling
                $ ioError
                $ commandError
                    InvalidArgument "cp"
                    "not writing through dangling symlink" (Just target)
        _ -> return ()
    r <- shouldOverwrite "cp" True (optOverwrite options) src target
    when r $ cpCopy (optCopyMethod options) src target
