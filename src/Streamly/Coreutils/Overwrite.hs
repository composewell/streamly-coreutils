-- |
-- Module      : Streamly.Coreutils.Overwrite
-- Copyright   : (c) 2026 Composewell Technologies
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Overwrite behavior and destination resolution shared by commands that
-- write to a destination path (@cp@, @mv@).

module Streamly.Coreutils.Overwrite
    ( Overwrite (..)
    , destinationExistsError
    , shouldOverwrite
    , targetInDir
    )
where

import Foreign.C.Error (eEXIST)
import GHC.IO.Exception (IOErrorType(InvalidArgument))
import System.IO.Error (ioeSetErrorString)

import qualified System.PosixCompat.Files as Files

import Streamly.FileSystem.Path (Path)
import qualified Streamly.FileSystem.Path as Path
import qualified Streamly.Internal.FileSystem.Path as Path
    (dropTrailingSeparators)

import Streamly.Coreutils.Error (commandError, errnoError)
import Streamly.Coreutils.FileTest

-- | What to do when the destination already exists.
data Overwrite =
      OverwriteNever  -- ^ Keep an existing destination and skip without an
                      -- error, like GNU @--update=none@.
    | OverwriteError  -- ^ Keep an existing destination and raise an
                      -- 'IOError', like GNU @--update=none-fail@.
    | OverwriteAlways -- ^ Replace an existing destination, the GNU default
                      -- (@--update=all@).
    | OverwriteOnly   -- ^ Act only if the destination exists. No GNU
                      -- equivalent.
    | OverwriteUpdate -- ^ Replace an existing destination only if its
                      -- modification time is earlier than that of the
                      -- source, like GNU @-u@ (@--update=older@). Equal
                      -- times keep the destination. Whether symlinks are
                      -- followed depends on the command.

-- | @destinationExistsError cmd dest@ is the error raised by 'OverwriteError'
-- for an existing @dest@, with errno @EEXIST@ and the GNU description
-- "not replacing".
destinationExistsError :: String -> Path -> IOError
destinationExistsError cmd dest =
    ioeSetErrorString (errnoError cmd eEXIST dest) "not replacing"

-- The modification times are compared with the same follow mode as the
-- existence check. newerThanFile always follows a symlink at dest, so with
-- followLinks False it would compare with the target of the symlink, and
-- raise for a dangling one, whereas GNU mv -u compares with the symlink.

-- | @shouldOverwrite cmd followLinks option src dest@ returns 'True' if the
-- command @cmd@ should write @src@ to @dest@, and raises
-- 'destinationExistsError' for 'OverwriteError' if @dest@ exists.
-- @followLinks@ decides whether a
-- symlink at @src@ or @dest@ is followed when checking that @dest@ exists and
-- when comparing modification times: 'True' for @cp@, 'False' for @mv@.
shouldOverwrite :: String -> Bool -> Overwrite -> Path -> Path -> IO Bool
shouldOverwrite cmd followLinks option src dest =
    case option of
        OverwriteAlways -> return True
        OverwriteOnly -> runTest dest pathExists
        OverwriteNever -> not <$> runTest dest pathExists
        OverwriteError -> do
            r <- runTest dest pathExists
            if r
            then ioError (destinationExistsError cmd dest)
            else return True
        OverwriteUpdate -> do
            r <- runTest dest pathExists
            if r
            then do
                destTime <-
                    Files.modificationTimeHiRes <$> getStatus (Path.toString dest)
                runTest src $ modifyTime (> destTime)
            else return True

    where

    (runTest, getStatus) =
        if followLinks
        then (test, Files.getFileStatus)
        else (testl, Files.getSymbolicLinkStatus)

-- Path.takeFileName returns Nothing when there are trailing separators, so
-- they are dropped first. It also returns Nothing for a path ending in "."
-- or "..", and for a root. GNU cp and mv fail for such a source too.

-- | @targetInDir cmd dir src@ is the last component of @src@, ignoring
-- trailing separators, under @dir@, like GNU @cp@ and @mv@ with a directory
-- destination. Raises an 'IOError' of type 'InvalidArgument' with location
-- @cmd@ if @src@ ends in @.@ or @..@, or is a root.
targetInDir :: String -> Path -> Path -> IO Path
targetInDir cmd dir src =
    case Path.takeFileName (Path.dropTrailingSeparators src) of
        Just name -> return (Path.join dir name)
        Nothing ->
            ioError
                $ commandError
                    InvalidArgument cmd "no file name in source path"
                    (Just src)
