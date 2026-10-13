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
    , CpMethod (..)
    , cpMethod
    , intoDirectory
    )
where

import Control.Monad (when)
import System.PosixCompat.Files (createLink)
#if !defined(CABAL_OS_WINDOWS)
import Control.Exception (bracket)
import Data.Function ((&))
import System.IO (hClose)
import System.Posix.Files
    (accessModes, fileMode, getFileStatus, intersectFileModes)
import System.Posix.IO (createFile, fdToHandle)
import qualified Streamly.Data.Stream as Stream
import qualified Streamly.FileSystem.Handle as Handle
#endif
import qualified Streamly.Internal.FileSystem.FileIO as File
import Streamly.FileSystem.Path (Path)
import qualified Streamly.FileSystem.Path as Path

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

-- | How to copy the source to destination?
data CpMethod =
      CopyContents
    | HardLink
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
-- POSIX we use the FilePath based functions from unix.

-- | Copy the source to a destination that does not exist. On POSIX the
-- destination is created with the permission bits of the source; the process
-- umask applies. Like GNU cp without @--preserve@, special bits (setuid,
-- setgid, sticky) are not copied. The destination is written even if the new
-- mode is read-only.
copyToNew :: Path -> Path -> IO ()
#if defined(CABAL_OS_WINDOWS)
copyToNew src dest = File.fromChunks dest (File.readChunks src)
#else
copyToNew src dest = do
    st <- getFileStatus (Path.toString src)
    let mode = intersectFileModes (fileMode st) accessModes
        create = createFile (Path.toString dest) mode >>= fdToHandle
    bracket create hClose $ \h ->
        File.readChunks src & Stream.fold (Handle.writeChunks h)
#endif

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
                    $ ioError $ userError
                    $ "cp: source and destination are the same file: "
                        ++ Path.toString src
                File.fromChunks dest (File.readChunks src)
            else copyToNew src dest
        HardLink -> createLink (Path.toString src) (Path.toString dest)
        SymbolicLink -> error "Unimplemented"
        CopyClone -> error "Unimplemented"

-- | @cp option-modifier source destination@. Copy a file or directory.
--
-- If @destination@ is an existing directory, or a symlink to one, @source@
-- is copied into it; see 'intoDirectory'. Pass 'id' for defaults; each
-- modifier documents its own default.
cp :: (CpOptions -> CpOptions) -> Path -> Path -> IO ()
cp f src dest = do
    let options = f defaultOptions
    destIsDir <-
        if optIntoDirectory options
        then test dest (doesItExist `and_` isDir)
        else return False
    target <- if destIsDir then targetInDir dest src else return dest
    r <- shouldOverwrite True (optOverwrite options) src target
    when r $ cpCopy (optCopyMethod options) src target
