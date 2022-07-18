-- |
-- Module      : Streamly.Coreutils.Rm
-- Copyright   : (c) 2022 Composewell Technologies
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Deletes a file or directory.

module Streamly.Coreutils.Rm
    ( rm

    -- * Options
    , Rm
    , RmForce(..)
    , force
    , recursive
    )
where

import Streamly.Coreutils.Common (Switch(..))
import Streamly.Coreutils.FileTest (isExisting, test, testM, isDir, isWritable)
import System.Directory
    ( removeFile
    , removeDirectoryRecursive
    , removePathForcibly
    )
import Control.Monad (when)

-- | Error behavior based on existence or permissions of the file.
data RmForce =
      None  -- ^ Raise error if the file or directory does not exist or if it
            -- does not have write permission.
    | Force -- ^ Do not complain even if the file does not exist, remove
            -- regular files or empty directories even if they do not have
            -- write or execute permission. In recursive mode, do not remove
            -- files from a directory if it does not have write or execute
            -- permission.
    | Nuke  -- ^ Like 'Force' but in recursive mode, even if some directories
            -- in the tree do not have write or execute permissions, force
            -- deletion of its children by changing permissions automatically.

data Rm = Rm
    {
      rmForce :: RmForce
    , rmRecursive :: Switch
    }

defaultConfig :: Rm
defaultConfig = Rm
    { rmForce = None
    , rmRecursive = Off
    }

-- | Specify the force behavior. See 'RmForce'.
--
-- Default is 'None'.
--
force :: RmForce -> Rm -> Rm
force val cfg = cfg {rmForce = val}

-- | Remove recursively when the path is a directory.
--
-- Default is 'Off'.
--
recursive :: Switch -> Rm -> Rm
recursive sw cfg = cfg {rmRecursive = sw}

rmFileWith :: (FilePath -> IO ()) -> Rm -> FilePath -> IO ()
rmFileWith rmfile options path = do
    case rmForce options of
        None -> do
            writable <- testM path (isWritable path)
            if writable
            then rmfile path
            else
                error
                    $ "rm: cannot remove "
                    ++ path ++ ": write-protected regular file"
        _ -> rmfile path

rmWith :: (FilePath -> IO ()) -> (FilePath -> IO ()) -> Rm -> FilePath -> IO ()
rmWith rmdir rmfile options path = do
    dir <- test path isDir
    if dir
    then
        case rmRecursive options of
            Off -> error $ "rm: cannot remove '" ++ path ++ "': is a directory"
            On -> rmdir path
    -- XXX Recursive case needs to do the same checks for each file, but in
    -- that case we rely on the recursive directory removal function which
    -- might provide different error messages.
    else rmFileWith rmfile options path

rm :: (Rm -> Rm) -> FilePath -> IO ()
rm f path = do
    let options = f defaultConfig
    -- Note this test is required not just for existence check but also so that
    -- we fail if there is no permission to access the path.
    found <- test path isExisting
    case rmForce options of
        Nuke ->
            when found
                $ rmWith removePathForcibly removePathForcibly options path
        Force ->
            when found
                $ rmWith removeDirectoryRecursive removeFile options path
        None ->
            if found
            then rmWith removeDirectoryRecursive removeFile options path
            else error $ "rm: cannot remove " ++ path
                       ++ ": no such file or directory"
