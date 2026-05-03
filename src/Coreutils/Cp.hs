-- |
-- Module      : Coreutils.Cp
-- Copyright   : (c) 2022 Composewell Technologies
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Copy a file or directory.

module Coreutils.Cp
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

import Control.Monad (when)
import Data.Function ((&))
import System.PosixCompat.Files (createLink)
import qualified Streamly.Internal.FileSystem.FileIO as File
import Streamly.FileSystem.Path (Path)
import qualified Streamly.FileSystem.Path as Path

import Coreutils.FileTest

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

-- | Unconditionally copy the source to destination using the specified copy
-- method.
cpCopy :: CpMethod -> Path -> Path -> IO ()
cpCopy method src dest =
    case method of
        CopyContents -> File.readChunks src & File.fromChunks dest
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
