module Streamly.Coreutils.FileSystem (
      defaultRmdirOptions
    , RmdirOptions (..)
    , rmdir
   )
where
import Control.Monad.Trans.Control (MonadBaseControl (..))

import Streamly.Coreutils.Common
import qualified Streamly.Prelude as S
import qualified Streamly.Memory.Array as A
import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Data.Unicode.Stream as Un
import qualified Streamly.Internal.FileSystem.File as File
import qualified Streamly.Internal.FileSystem.Handle as FH

import System.Directory (removeDirectory)
import Data.Word (Word8)
import System.IO (hPutStr)
import Data.Char (toLower, isSpace)
import System.IO (Handle, stdout)
import Foreign.Storable (Storable)
import Control.Monad.Catch (MonadCatch, MonadThrow)
import System.IO.Unsafe (unsafePerformIO)
import Streamly.Internal.Data.SVar (MonadAsync)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Streamly.Internal.Data.Stream.Serial(SerialT)
import Streamly.Internal.Data.Stream.StreamK.Type (IsStream)

-------------------------------------------------------------------------------
-- Record for options used with uniq
-------------------------------------------------------------------------------
-- pwd, rm, rmdir, mkdir, mv, ls

{-# INLINE intMax #-}

intMax :: Int
intMax = maxBound


data RmdirOptions = RmdirOptions {
                       verbose :: Bool
                     , ignoreFail :: Bool   -- ignore failure because of a non-empty directory
                     , parents :: Bool      -- remove directory & its ancestors
                                            -- rmdir -p a/b/c is similar to rmdir a/b/c a/b a
                  }

{-# INLINE defaultRmdirOptions #-}

defaultRmdirOptions :: RmdirOptions
defaultRmdirOptions = RmdirOptions True False False



data PwdOptions = PwdOptions {
                       physical :: Bool     -- avoid all symlinks
                  }

{-# INLINE defaultPwdOptions #-}

defaultPwdOptions :: PwdOptions
defaultPwdOptions = PwdOptions True




data MkdirOptions = MkdirOptions {
                       parents :: Bool     -- create parents if not present
                     , verbose :: Bool     -- print each directory created
                     --, mode :: String      -- TODO need to understand
                  }

{-# INLINE defaultMkdirOptions #-}

defaultMkdirOptions :: MkdirOptions
defaultMkdirOptions = MkdirOptions True True

-------------------------------------------------------------------------------
-- helper functions for rmdir
-------------------------------------------------------------------------------


rmdir :: RmdirOptions -> SomeBase Dir -> IO ()
rmdir opt fp = if (parents opt) == True
               -- get a/b/c a, b, c to be removed
               else
                  removeDirectory fp
