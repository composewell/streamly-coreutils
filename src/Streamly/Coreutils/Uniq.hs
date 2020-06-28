module Streamly.Coreutils.Uniq (
   )
where

import Streamly.Coreutils.Types
import qualified Streamly.Prelude as S
import qualified Streamly.Data.Fold as FL
import qualified Streamly.Memory.Array as A

import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.Prelude as IP
import qualified Streamly.Internal.FileSystem.Dir as Dir
import qualified Streamly.Internal.FileSystem.File as File
import qualified Streamly.Internal.FileSystem.Handle as FH

import Streamly.Internal.Data.Stream.StreamK.Type (IsStream)

import System.IO (Handle, stdout)
import System.Environment (getArgs)
import Streamly.Data.Unicode.Stream (decodeLatin1)


-------------------------------------------------------------------------------
-- cp and helper functions for options
-------------------------------------------------------------------------------

