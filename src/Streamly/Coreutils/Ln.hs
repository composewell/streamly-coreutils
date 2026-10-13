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
import Streamly.Coreutils.FileTest (test, testl, doesItExist, sameFileAs)
import System.Directory (removeFile)
import System.FilePath (takeDirectory, takeFileName)

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
    -- testl: a dangling symlink at tgt also counts as existing.
    found <- testl tgt doesItExist
    when found $ do
        when (lnForce opt == False) $ error msg
        -- Like GNU ln, refuse instead of removing the source itself.
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
    -- Another hard link to the source is a different entry and is replaced.
    isSameEntry = do
        sameInode <- testl tgt (sameFileAs src)
        srcDir <- Path.fromString (takeDirectory srcStr)
        tgtDir <- Path.fromString (takeDirectory tgtStr)
        sameDir <- test tgtDir (sameFileAs srcDir)
        pure $ sameInode && sameDir && takeFileName srcStr == takeFileName tgtStr

    msg = "ln: Link target exists, use force to create anyway"
    sameMsg =
        "ln: '" ++ srcStr ++ "' and '" ++ tgtStr
            ++ "' are the same file"
