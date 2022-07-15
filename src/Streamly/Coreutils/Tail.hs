-- |
-- Module      : Streamly.Coreutils.Tail
-- Copyright   : (c) 2022 Composewell Technologies
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Output the last part of files.

module Streamly.Coreutils.Tail
    (tail, tailN)
where

import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Function ((&))
import Streamly.Prelude (SerialT)
import System.IO (IOMode(..), openFile)

import qualified Streamly.FileSystem.Handle as File
import qualified Streamly.Internal.Data.Array as Array
import qualified Streamly.Internal.Data.Stream.IsStream as Stream
import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.Unicode.Stream as Unicode

import Prelude hiding (tail)

tailN :: Int -> FilePath -> SerialT IO [Char]
tailN n fp = do
    fh <- liftIO $ openFile fp ReadMode
    Stream.unfold File.read fh
        & Unicode.decodeUtf8
        & Unicode.lines FL.toList
        & Stream.fold (Array.writeLastN n)
        & Stream.map Array.toStream
        & Stream.concat

tail :: FilePath -> SerialT IO [Char]
tail = tailN 10
