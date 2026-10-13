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
    )
where

import Control.Monad (when)
import Data.Function ((&))
import System.PosixCompat.Files (createLink)
#if !defined(CABAL_OS_WINDOWS)
import Control.Exception (bracket)
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
import Streamly.Coreutils.Overwrite (Overwrite(..), shouldOverwrite)

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
    }

defaultOptions :: CpOptions
defaultOptions = CpOptions
    { optOverwrite = OverwriteAlways
    , optCopyMethod = CopyContents
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
            then File.readChunks src & File.fromChunks dest
            else copyToNew src dest
        HardLink -> createLink (Path.toString src) (Path.toString dest)
        SymbolicLink -> error "Unimplemented"
        CopyClone -> error "Unimplemented"

-- | @cp option-modifier source destination@. Copy a file or directory.
cp :: (CpOptions -> CpOptions) -> Path -> Path -> IO ()
cp f src dest = do
    let options = f defaultOptions
    r <- shouldOverwrite test (optOverwrite options) src dest
    when r $ cpCopy (optCopyMethod options) src dest
