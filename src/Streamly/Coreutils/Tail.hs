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

    ( tail

    -- * Options
    , Tail
    , follow
    , tailN
    )
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
import Streamly.Coreutils.Common (Switch(..))

newtype Tail = Tail{tailFollow :: Switch}

defaultConfig :: Tail
defaultConfig = Tail Off

follow :: Switch -> Tail -> Tail
follow opt cfg = cfg {tailFollow = opt}

tailForEver :: Int -> FilePath -> SerialT IO [Char]
tailForEver n fp = do
    fh <- liftIO $ openFile fp ReadMode
    Stream.concatMap Stream.fromList
        $ Stream.repeatM
        $ Stream.toList
        $ Stream.unfold File.read fh
        & Unicode.decodeUtf8
        & Unicode.lines FL.toList
        & Stream.fold (Array.writeLastN n)
        & Stream.map Array.toStream
        & Stream.concat

tailN :: (Tail -> Tail) ->  Int -> FilePath -> SerialT IO [Char]
tailN f n fp = do
    fh <- liftIO $ openFile fp ReadMode
    let opt = f defaultConfig
    case tailFollow opt of
        On -> tailForEver n fp
        Off ->
            Stream.unfold File.read fh
                & Unicode.decodeUtf8
                & Unicode.lines FL.toList
                & Stream.fold (Array.writeLastN n)
                & Stream.map Array.toStream
                & Stream.concat

tail :: (Tail -> Tail) -> FilePath -> SerialT IO [Char]
tail f = tailN f 10
