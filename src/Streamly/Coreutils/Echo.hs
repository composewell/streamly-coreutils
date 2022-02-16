-- |
-- Module      : Streamly.Coreutils.Echo
-- Copyright   : (c) 2021 Composewell Technologies
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- A Haskell function similar in spirit to the POSIX echo command.
--
-- See also: putStr, putStrLn, print, printf from the base package.
--
-- See https://pubs.opengroup.org/onlinepubs/007904975/utilities/echo.html .

-- There are several common ways to serialize and deserialize data types in
-- Haskell. Some of those are:
--
-- * Show and Read: serialize to unicode strings
-- * Binary: serialize to binary data
--
-- The 'print' function in Haskell prelude uses 'Show' to serialize, and the
-- 'read' function uses 'Read' to deserialize.
--
-- Echo is similar to 'print' but does not use 'Show'.  It provides a one way
-- serialization of data for human consumption.  Deserialization to get the
-- same data back may not be possible. We can also say that this is a pretty
-- printing function.
--
-- Echo is a simple but handy pretty printing function that bundles the
-- following functionality:
--
-- * Write its arguments to console as utf8 bytes
-- * Serialize containers of Char to utf8 bytes. Anything that can be turned to
--   a stream of utf8 bytes can be echoed.
-- * Join nested containers of Char with a space char
-- * Join variadic arguments with a space char
--
-- echo_ (t Char) - print to console
-- echo_ (t1 Char) (t2 Char) - print joined by spaces
-- echo_ (t1 (t2 Char)) - print joined by spaces
--
-- echo = echo_ followed "\n"
-- echoBy sep = like echo but concated by sep (sep :: t Char)
--
-- Note if we use (t Char) then we cannot use that for Text or Utf8 types. If
-- we use any type instead of t Char we can also use "Array Word8".
--
-- Semantics: Only supports containers of Char or Word8. Char is converted to
-- utf8 and Word8 is written as if it is utf8.

-- Use an IsStream type class to convert to stream. IsStream would have toStream
-- and fromStream as members. An Unfold can implement toStream instead of
-- "unfold" and similarly a "fold" can implement fromStream instead of using
-- "fold". This type class would be like IsList.
-- We can use ToStream and FromStream as well because some types can be one but
-- not the other or IsUnfold and IsFold instead? So we can have IsUnfold,
-- IsStream and IsFold.
--
-- class IsUnfold t m a b where
--  unfold :: t -> a -> Stream m b
--
-- class IsFold t m a b where
--  fold :: t -> Stream m a -> m b
--
--  class IsStream t a where
--      toStream :: t -> Stream m a
--      fromStream :: Stream m a -> m t
--
--  class Show a where
--      show :: Unfold m a Char
--
--  class Read a where
--      read :: Fold m Char a

module Streamly.Coreutils.Echo
    (
    )
where

import Data.Functor.Identity (Identity)
import Data.Word (Word8)
import Streamly.Data.Array.Foreign (Array)
import Streamly.Prelude (SerialT)
import Foreign.Storable (Storable)
import qualified Streamly.Internal.Data.Array.Foreign as Array
import qualified Streamly.Internal.Data.Array.Stream.Foreign as ArrayStream
import qualified Streamly.Prelude as Stream
import qualified Streamly.Unicode.Stream as Unicode

-- This type class is to unify three common container types namely, Streams,
-- Lists and Arrays.
--
-- | A convenient way to convert data types to utf8 bytes that can be displayed
-- on console, sent to network or written to files. Basically serialize the
-- data types to a utf8 byte stream.
--
class ToUtf8 t m where
    toUtf8 :: Monad m => t -> SerialT m Word8

{-
-- If we have a ToStream type class then we can define a generic instance for
-- 't Char' given that 't Char' is an instance of ToStream.

class ToStream t m where
      toStream :: (Monad m, Storable a) => t a -> SerialT m a

instance ToStream (SerialT m) m where
    toStream = id

instance ToStream [] m where
    toStream = Stream.fromList

instance ToStream Array m where
    toStream = Array.toStream
-}

instance ToUtf8 (SerialT m Char) m where
    toUtf8 = Unicode.encodeUtf8

instance ToUtf8 [Char] m where
    toUtf8 = Unicode.encodeUtf8 . Stream.fromList

instance ToUtf8 (Array Char) m where
    toUtf8 = Unicode.encodeUtf8 . Array.toStream

-- Word8 types that are already Utf8 encoded
instance ToUtf8 (SerialT m Word8) m where
    toUtf8 = id

instance ToUtf8 [Word8] m where
    toUtf8 = Stream.fromList

instance ToUtf8 (Array Word8) m where
    toUtf8 = Array.toStream

-- Chunked

instance ToUtf8 (SerialT m (Array Word8)) m where
    toUtf8 = Stream.unfoldMany Array.read


-- Use getUtf8 and putUtf8 instead of echo.

{-
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
    -}
