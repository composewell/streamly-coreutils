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
    , force
    , nuke
    , recursive
    )
where

import Streamly.Coreutils.Common (Switch(..))
import Streamly.Coreutils.FileTest (isExisting, test, isFile)
import System.Directory
    ( removeFile
    , removeDirectoryRecursive
    , removePathForcibly
    )
import Control.Monad (when)

data Rm = Rm
    {
      rmForce :: Switch
    , rmRecursive :: Switch
    , rmNuke :: Switch
    }

defaultConfig :: Rm
defaultConfig = Rm Off Off Off

force :: Switch -> Rm -> Rm
force sw cfg = cfg {rmForce = sw}

recursive :: Switch -> Rm -> Rm
recursive sw cfg = cfg {rmRecursive = sw}

nuke :: Switch -> Rm -> Rm
nuke sw cfg = cfg {rmNuke = sw}

rm :: (Rm -> Rm) -> FilePath -> IO ()
rm f path = do
    let opt = f defaultConfig
    case rmNuke opt of
        On ->
            removePathForcibly path
        Off ->
            case rmRecursive opt of
                On -> do
                    found <- test path isExisting
                    if found
                    then removeDirectoryRecursive path
                    else error msg
                Off -> do
                    found <- test path isExisting
                    if found
                    then do
                        case rmForce opt of
                            On -> do
                                notDir <- test path isFile
                                when notDir $ removePathForcibly path
                            Off -> removeFile path
                    else error msg

    where

    msg = "Path" ++ path ++ " doesn't isExisting."
