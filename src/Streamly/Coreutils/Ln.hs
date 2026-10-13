-- |
-- Module      : Streamly.Coreutils.Ln
-- Copyright   : (c) 2022 Composewell Technologies
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- create a link to TARGET with the given name

module Streamly.Coreutils.Ln
    ( ln

    -- * Options
    , LnOptions
    , force
    , symbolic
    )
where

import Control.Monad (when)
import Data.Maybe (fromMaybe)
import Streamly.Coreutils.FileTest (test, testl, doesItExist, sameFileAs)
import System.Directory (removeFile)

import qualified System.PosixCompat.Files as Posix
import Streamly.FileSystem.Path (Path)
import qualified Streamly.FileSystem.Path as Path

data LnOptions = LnOptions
    { lnForce :: Bool
    , lnSymbolic :: Bool
    }

defaultConfig :: LnOptions
defaultConfig = LnOptions False False

-- | Remove an existing destination before creating the link, like @ln -f@.
-- Fails if the source and the destination are the same file.
--
-- Default (without this modifier): 'False', fail if the destination exists.
force :: Bool -> LnOptions -> LnOptions
force opt cfg = cfg {lnForce = opt}

symbolic :: Bool -> LnOptions -> LnOptions
symbolic opt cfg = cfg {lnSymbolic = opt}

ln :: (LnOptions -> LnOptions) -> Path -> Path -> IO ()
ln f src tgt = do
    let opt = f defaultConfig
    -- target can be a symlink or non-symlink
    found <- testl tgt doesItExist
    when found $ do
        when (lnForce opt == False) $ error msg
        -- Do not remove the source itself.
        srcFound <- test src doesItExist
        same <- if srcFound then isSameEntry else pure False
        when same $ error sameMsg
        removeFile tgtStr
    case lnSymbolic opt of
        False -> Posix.createLink srcStr tgtStr
        True -> Posix.createSymbolicLink srcStr tgtStr

    where

    tgtStr = Path.toString tgt
    srcStr = Path.toString src

    -- Same directory entry: same inode, same parent directory and same name.
    -- A path without a file name part ("x/", ".", "..") is treated as the
    -- same entry when the inodes match, so that the source is never removed.
    isSameEntry = do
        sameInode <- testl tgt (sameFileAs src)
        case (Path.splitFile src, Path.splitFile tgt) of
            (Just (srcDir, srcName), Just (tgtDir, tgtName)) -> do
                sameDir <- test (dirOrCwd tgtDir) (sameFileAs (dirOrCwd srcDir))
                pure
                    $ sameInode
                    && sameDir
                    && Path.eqPath id srcName tgtName
            _ -> pure sameInode

    dirOrCwd = fromMaybe (Path.fromString_ ".")

    msg = "ln: Link target exists, use force to create anyway"
    sameMsg =
        "ln: '" ++ srcStr ++ "' and '" ++ tgtStr
            ++ "' are the same file"
