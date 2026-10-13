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
    , CpOverwrite (..)
    -- , CpBackup
    , cpOverwrite
    , CpMethod (..)
    , cpMethod
    )
where

import Control.Exception (bracket)
import Control.Monad (when)
import Data.Function ((&))
import System.IO (Handle, hClose)
import System.PosixCompat.Files (createLink)
#if defined(CABAL_OS_WINDOWS)
import System.IO (IOMode(WriteMode), openBinaryFile)
#else
import System.Posix.Files
    (accessModes, fileMode, getFileStatus, intersectFileModes)
import System.Posix.IO (createFile, fdToHandle)
#endif
import qualified Streamly.Data.Stream as Stream
import qualified Streamly.FileSystem.Handle as Handle
import qualified Streamly.Internal.FileSystem.FileIO as File
import Streamly.FileSystem.Path (Path)
import qualified Streamly.FileSystem.Path as Path

import Streamly.Coreutils.FileTest

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

-- | Specify the overwrite behavior of copy.
data CpOverwrite =
      OverwriteNever  -- ^ Do not overwrite when destination file exists
    | OverwriteAlways -- ^ Overwrite destination file if it exists
    | OverwriteOnly   -- ^ Copy only if the destination exists
    | OverwriteUpdate -- ^ Overwrite an existing destination file only if it is
                      -- older than the source file.

-- | When overwriting specify backup behavior.
-- newtype CpBackup = Backup (Maybe String)

-- | How to copy the source to destination?
data CpMethod =
      CopyContents
    | HardLink
    | SymbolicLink
    | CopyClone     -- Use the cloning method if available on the platform

data CpOptions = CpOptions
    { optOverwrite :: CpOverwrite
    , optCopyMethod :: CpMethod
    }

defaultOptions :: CpOptions
defaultOptions = CpOptions
    { optOverwrite = OverwriteAlways
    , optCopyMethod = CopyContents
    }

-- | Specify the overwrite behavior. See 'Overwrite'.
--
-- Default is 'OverwriteAlways'.
--
cpOverwrite :: CpOverwrite -> CpOptions -> CpOptions
cpOverwrite opt options = options { optOverwrite = opt }

-- | Specify the copy method.
--
-- Default is 'CopyContents'.
--
cpMethod :: CpMethod -> CpOptions -> CpOptions
cpMethod opt options = options { optCopyMethod = opt }

-- | Create the destination file, open for writing, with the permission bits
-- of the source; the process umask applies. Like GNU cp without
-- @--preserve@, special bits (setuid, setgid, sticky) are not copied. The
-- returned handle is writable even if the new mode is read-only.
createWithModeOf :: Path -> Path -> IO Handle
#if defined(CABAL_OS_WINDOWS)
createWithModeOf _ dest = openBinaryFile (Path.toString dest) WriteMode
#else
createWithModeOf src dest = do
    st <- getFileStatus (Path.toString src)
    let mode = intersectFileModes (fileMode st) accessModes
    createFile (Path.toString dest) mode >>= fdToHandle
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
            else
                bracket (createWithModeOf src dest) hClose $ \h ->
                    File.readChunks src & Stream.fold (Handle.writeChunks h)
        HardLink -> createLink (Path.toString src) (Path.toString dest)
        SymbolicLink -> error "Unimplemented"
        CopyClone -> error "Unimplemented"

-- | Determine whether source should be copied to destination based on the
-- specified overwrite behavior option.
cpShouldOverwrite :: CpOverwrite -> Path -> Path -> IO Bool
cpShouldOverwrite option src dest =
    case option of
        OverwriteAlways -> return True
        OverwriteOnly -> test dest doesItExist
        OverwriteNever -> not <$> test dest doesItExist
        OverwriteUpdate -> do
            r <- test dest doesItExist
            if r
            then test src $ newerThanFile dest
            else return True

-- | @cp option-modifier source destination@. Copy a file or directory.
cp :: (CpOptions -> CpOptions) -> Path -> Path -> IO ()
cp f src dest = do
    let options = f defaultOptions
    r <- cpShouldOverwrite (optOverwrite options) src dest
    when r $ cpCopy (optCopyMethod options) src dest
