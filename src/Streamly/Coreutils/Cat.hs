{-# LANGUAGE FlexibleContexts #-}
module Streamly.Coreutils.Cat (
      cat
    , defaultCatOptions
    , CatOptions (..)
   )
where
import Streamly.Coreutils.Common

import qualified Streamly.Prelude as S
import qualified Streamly.Memory.Array as A
import qualified Streamly.Data.Unicode.Stream as U
import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.FileSystem.Dir as Dir
import qualified Streamly.Internal.FileSystem.File as File
import qualified Streamly.Internal.FileSystem.Handle as FH

import Data.Word (Word8)
import System.IO (Handle, stdout)
import GHC.IO.Handle.FD (openFile)
import System.Environment (getArgs)
import Control.Monad.IO.Class (MonadIO)
import Streamly.Internal.Data.SVar (MonadAsync)
import Streamly.Data.Unicode.Stream (decodeLatin1)

import Streamly.Internal.Data.Stream.StreamK.Type (IsStream)

-------------------------------------------------------------------------------
-- Record for options used with cat
-------------------------------------------------------------------------------

data CatOptions = CatOptions {
                     showAll :: Bool
                   , numberNonEmptyLines :: Bool
                   , showEnds :: Bool
                   , numberAllLines :: Bool
                   , suppressRepeatedEmpty :: Bool
                  }

defaultCatOptions :: CatOptions
defaultCatOptions = CatOptions True True True True True

-------------------------------------------------------------------------------
-- helper functions for cat
-------------------------------------------------------------------------------

-- TODO : suppresses non-blank lines if True

cat :: (IsStream t, MonadAsync m) => CatOptions -> Handle -> t m Handle -> t m ()
cat opt dest = S.mapM (\h -> FH.fromBytes dest $ FH.toBytes h)
