-- |
-- Module      : Streamly.Coreutils.Touch
-- Copyright   : (c) 2022 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Update the access and modification times of a file to the current time.

module Streamly.Coreutils.Touch
    (
      touch

    -- * Options
    , TouchOptions
    , create -- XXX rename for conflicts
    , followSymlinks -- XXX this is a common option in multiple commands
    )
where

import Streamly.Coreutils.FileTest (test, doesItExist)
import System.IO (IOMode(AppendMode))
import System.IO.Error (catchIOError, isDoesNotExistError)
import Streamly.FileSystem.Path (Path)
import qualified Streamly.FileSystem.Path as Path

#if defined (CABAL_OS_WINDOWS)
import qualified Streamly.Internal.FileSystem.Windows.File as File
#else
import qualified Streamly.Internal.FileSystem.Posix.File as File
import qualified System.Posix.Files as Posix (touchSymbolicLink)
#endif
import qualified System.PosixCompat.Files as Posix

data TouchOptions = TouchOptions
    {
      createNew :: Bool
    , deRef :: Bool   -- touch the referenced file for symbolic link
    }

defaultConfig :: TouchOptions
defaultConfig = TouchOptions True True

-- | Default is 'True'.
followSymlinks :: Bool -> TouchOptions -> TouchOptions
followSymlinks opt cfg = cfg {deRef = opt}

-- | When 'False', do not create a missing file and do not report an error
-- for it, like @touch -c@.
--
-- Default (without this modifier): 'True', a missing file is created.
create :: Bool -> TouchOptions -> TouchOptions
create opt cfg = cfg {createNew = opt}

-- | If the file does not exist create it only if both followSymlinks and
-- create are set to 'True'.
--
-- If the file or symbolic link exists then update the access and
-- modification times. If 'followSymlinks' is 'True' then the link target is
-- updated otherwise the symbolic link itself is updated.
--
-- Fails if the parent directories in the path do not exist or if there is no
-- permission to access a path component.
--
-- Defaults:
--
-- * create True
-- * followSymlinks True
--
touch :: (TouchOptions -> TouchOptions) -> Path -> IO ()
touch f path = do
    let opt = f defaultConfig
        -- XXX need to use OsPath based APIs
        pathStr = Path.toString path
    if createNew opt && deRef opt
    then do
        found <- test path doesItExist
        if found
        then Posix.touchFile pathStr
        -- AppendMode does not truncate a file created after the check.
        else File.withFile path AppendMode (\_ -> return ())
    else
        ignoreMissing opt $ case deRef opt of
            True -> Posix.touchFile pathStr
#if !defined (CABAL_OS_WINDOWS)
            False -> Posix.touchSymbolicLink pathStr
#else
            -- XXX Is it possible to support this on Windows?
            False ->
                error "touch: followSymlinks=False not supported on Windows"
#endif

    where

    -- GNU touch -c ignores ENOENT, including a missing parent directory.
    ignoreMissing opt action
        | createNew opt = action
        | otherwise =
            action `catchIOError` \e ->
                if isDoesNotExistError e then return () else ioError e
