-- |
-- Module      : Streamly.Coreutils.Echo
-- Copyright   : (c) 2021 Composewell Technologies
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--

module Streamly.Coreutils.Echo
    (
    )
where

import Data.Word (Word8)
import Streamly.Data.Array.Foreign (Array)
import Streamly.Prelude (SerialT)

-- | A convenient way to convert data types to bytes that can be displayed on
-- console, sent to network or written to files. Basically serialize the data
-- types to byte stream.
--
class IsBytes a where
    bytes :: a -> IO ()

-- XXX Move this to Console.Stdio?
-- | A convenient way to display data types on console.
class IsEcho a where
    echo :: a -> IO ()

-- instances for all serializable types?
-- Any type that can be converted to a byte stream

-- Char sequences
instance IsEcho String where
    echo = putStrLn

instance IsEcho (SerialT m Char) where
    echo = undefined

instance IsEcho (Array Char) where
    echo = undefined

-- Word8 sequences
instance IsEcho [Word8] where
    echo = undefined

instance IsEcho (SerialT m Word8) where
    echo = undefined

instance IsEcho (Array Word8) where
    echo = undefined

-- Nested
instance IsEcho [String] where
    echo = putStrLn . concat

instance IsEcho (SerialT m (Array Word8)) where
    echo = undefined

instance IsEcho (SerialT m (SerialT m Word8)) where
    echo = undefined

instance IsEcho [SerialT m Word8] where
    echo = undefined
