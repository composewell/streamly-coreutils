{-# LANGUAGE FlexibleContexts #-}
module Streamly.Coreutils.Cat (
      cat
   )
where
import Streamly.Coreutils.Types

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

--DESCRIPTION
--       Concatenate FILE(s) to standard output.
--
--       With no FILE, or when FILE is -, read standard input.
--
--       -A, --show-all
--              equivalent to -vET
--
--       -b, --number-nonblank
--              number nonempty output lines, overrides -n
--
--       -e     equivalent to -vE
--
--       -E, --show-ends
--              display $ at end of each line
--
--       -n, --number
--              number all output lines
--
--       -s, --squeeze-blank
--              suppress repeated empty output lines
--
--       -t     equivalent to -vT
--
--       -T, --show-tabs
--              display TAB characters as ^I
--
--       -u     (ignored)
--
--       -v, --show-nonprinting
--              use ^ and M- notation, except for LFD and TAB
--
--       --help display this help and exit
--
--       --version
--              output version information and exit



-- TODO : suppresses non-blank lines if True

cat :: (IsStream t, MonadAsync m) => CatOptions -> Handle -> t m Handle -> t m ()
cat opt dest = S.mapM (\h -> FH.fromBytes dest $ FH.toBytes h)
