{-# LANGUAGE QuasiQuotes #-}
-- |
-- Module      : Streamly.Coreutils.Tail
-- Copyright   : (c) 2025 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Read last N lines from a file.

-- TODO: we can add a tail1 or last to read the last line.
-- TODO: tail/head can be replaced with a single file reading utility which can
-- read lines or bytes in a range.
module Streamly.Coreutils.Tail
    (
      tail

    -- * Options
    , TailOptions
    , follow
    , lines
    , bytes
    , fromLine
    , fromByte
    )
where

import Data.Word (Word8)
import Streamly.Data.Array (Array)
import Streamly.Data.Stream.Prelude (Stream)
import Streamly.FileSystem.Path (Path)
import Streamly.Unicode.String (str)

import qualified Streamly.FileSystem.Path as Path
import qualified Streamly.System.Command as Command


import Prelude hiding (tail, lines)

data TailOptions = TailOptions
    { _follow :: Bool
    , _offset :: Either Int Int -- Left = lines, Right = bytes
    , _reverse :: Bool
    }

defaultConfig :: TailOptions
defaultConfig = TailOptions
    { _follow = False
    , _offset = Left 10
    , _reverse = False -- False means default i.e. from the end
    }

-- | Run forever following any appends to the file.
--
-- Same as @--follow@ flag in the standard tail command.
follow :: Bool -> TailOptions -> TailOptions
follow opt cfg = cfg {_follow = opt}

-- Note we could have used negative offset to indicate from the end and
-- positive from the beginning. But in that case 0 becomes ambiguous, is it
-- from the end or from the beginning.

-- | Read the specified number of lines at the end of the file.
--
lines :: Int -> TailOptions -> TailOptions
lines x cfg = cfg {_offset = Left x, _reverse = False}

-- | Read the specified number of bytes at the end of the file.
--
bytes :: Int -> TailOptions -> TailOptions
bytes x cfg = cfg {_offset = Right x, _reverse = False}

-- | Read from the specified line number up to the end of file.
fromLine :: Int -> TailOptions -> TailOptions
fromLine x cfg = cfg {_offset = Left x, _reverse = True}

-- | Read from the specified byte count up to the end of file.
fromByte :: Int -> TailOptions -> TailOptions
fromByte x cfg = cfg {_offset = Right x, _reverse = True}

-- XXX Replace the "tail" shell command with Haskell native implementation. A
-- portable implementation of "--follow" would use a sleep and poll, on linux
-- we can use streamly-fsevents (inotify).
--
-- XXX We can have a configurable option to return chunks of one line each. Or
-- line splitting can also be done by the user, unless we are getting it for
-- free here. We can also have an option to emit chunks with or without the
-- newline chars.

-- | Same as the standard @tail@ command. Returns byte arrays.
--
-- Note: currently this function depends on the @tail@ executable being
-- installed in the PATH.
tail :: (TailOptions -> TailOptions) -> Path -> Stream IO (Array Word8)
tail modifier path =
    let p = Path.toString path
        cfg = modifier defaultConfig
        -- XXX make portable
        foll = if _follow cfg then "-f" else ""
        dir = if _reverse cfg then "+" else ""
        loc =
            case _offset cfg of
                Left x -> "-n " ++ dir ++ show x
                Right x -> "-c " ++ dir ++ show x
        in Command.toChunks [str|tail #{foll} #{loc} #{p}|]
     {-
        if _follow cfg
        then Command.toChunks [str|tail #{foll} #{loc} #{p}|]
        -- The following is a generic implementation that works for all types
        -- of files including a fifo where we cannot determine the size and
        -- seek to the end.
        --
        -- Note that with this implementation @yes | tail -n 10@ may never
        -- terminate because we may never reach the EOF. Even @yes | tail -f -n
        -- 10@ does not terminate.
        --
        -- However, if the file is not a FIFO we can work on a snapshot, and we
        -- can read the file in reverse from the end. In case of a seekable
        -- file we can stat and determine the size of the file and then print
        -- the last n lines from that point.
        else Stream.concatEffect $ do
              r <- FileIO.readChunks path
                    & Array.compactEndByLn_ -- XXX need compactEndByLn
                    & Stream.fold (GArray.createOfLast (_lines cfg))
              pure (GArray.read r)
      -}
