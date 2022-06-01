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
    , Cp
    , createOnly
    , updateOnly
    , hardLink
    )
where

import Control.Monad (unless, when)
import Data.Default.Class (Default(..))
import Data.Function ((&))
#if !defined (CABAL_OS_WINDOWS)
import System.Posix.Files (createLink)
#endif

import qualified Streamly.Internal.FileSystem.File as File

import Streamly.Coreutils.FileTest

data Cp = HardLink | UpdateOnly | CreateOnly | Copy

instance Default Cp where
    def = Copy

updateOnly :: Cp
updateOnly = UpdateOnly

createOnly :: Cp
createOnly = CreateOnly

hardLink :: Cp
hardLink = HardLink

-- | > cp input.txt output.txt
cp :: Cp -> FilePath -> FilePath -> IO ()
cp options src dest = do
    case options of
        CreateOnly ->
            do
            exist <- test dest exists
            unless exist copy
#if !defined (CABAL_OS_WINDOWS)
        HardLink ->
            createLink src dest
#endif
        UpdateOnly ->
            do
            destExists <- test dest exists
            if destExists
            then do
                isNewer <- isNewerThan dest
                srcUpdated <- test src isNewer
                when srcUpdated copy
            else copy

        Copy -> copy

    where

    copy = File.toChunks src & File.fromChunks dest
