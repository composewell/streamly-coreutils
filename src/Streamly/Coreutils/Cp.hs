-- |
-- Module      : Streamly.Coreutils.Copy
-- Copyright   : (c) 2022 Composewell Technologies
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Copy a file or directory.

module Streamly.Coreutils.Cp
    ( cp
    , noClobber
    , updateOnly
    , hardLink
    )
where

import Control.Monad (unless, when)
import Data.Default.Class (Default(..))
import Data.Function ((&))
import GHC.Generics (Generic)
import Streamly.Coreutils.Common (Switch(..))
#if !defined (CABAL_OS_WINDOWS)
import System.Posix.Files (createLink)
#endif

import qualified Streamly.Internal.FileSystem.File as File

import Streamly.Coreutils.FileTest

data Options = Options
    { optNoClobber :: Switch
    , optUpdateOnly :: Switch
    , optHardLink :: Switch
    } deriving (Generic, Eq , Show)

instance Default Options

noClobber :: Switch -> Options -> Options
noClobber sw options = options {optNoClobber = sw}

updateOnly :: Switch -> Options -> Options
updateOnly sw options = options {optUpdateOnly = sw}

hardLink :: Switch -> Options -> Options
hardLink sw options = options {optHardLink = sw}

-- | > cp input.txt output.txt
cp :: Options -> FilePath -> FilePath -> IO ()
cp options src dest = do
    case options of
        Options {optNoClobber = On} ->
            do
            exist <- test dest exists
            unless exist copy
#if !defined (CABAL_OS_WINDOWS)
        Options {optHardLink = On} ->
            createLink src dest
#endif
        Options {optUpdateOnly = On} ->
            do
            destExists <- test dest exists
            if destExists
            then do
                isNewer <- isNewerThan dest
                srcUpdated <- test src isNewer
                when srcUpdated copy
            else copy

        _ -> copy

    where

    copy = File.toChunks src & File.fromChunks dest
