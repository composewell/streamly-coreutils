{-# LANGUAGE TemplateHaskell #-}
-- |
-- Module      : Streamly.Coreutils.StringQ
-- Copyright   : (c) 2022 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- change file mode bits.

-- XXX Rename to "Permissions" or "AccessControl"

module Streamly.Coreutils.StringQ
    (
      Role(..)
    , Permissions(..)
    , setReadable
    , setWritable
    , setExecutable
    , reset
    )
where

import Control.Applicative (Alternative(..))
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.Char (chr)
import Data.Data (Data)
import Data.Default.Class (Default(..))
import Language.Haskell.TH (Exp, Q, Pat)
import Language.Haskell.TH.Quote (QuasiQuoter(..), dataToExpQ, dataToPatQ)
import Streamly.Internal.Data.Parser (Parser)

import qualified Streamly.Internal.Data.Fold as Fold
import qualified Streamly.Internal.Data.Parser as Parser
import qualified Streamly.Internal.Data.Stream.IsStream as Stream
import qualified Streamly.Internal.Unicode.Parser as Parser

-------------------------------------------------------------------------------
-- Permissions
-------------------------------------------------------------------------------

-- | Permissions for access control
data Permissions = Permissions
  { readable :: Bool
  , writable :: Bool
  , executable :: Bool
  -- , searchable :: Bool -- for portability, keep it separate
  } deriving (Eq, Ord, Read, Show, Data)

{-
defaultPermissions =
    Permissions
    { readable = False
    , writable = False
    , executable = False
    }
-}

-- | Enable @read@ permission.
setReadable :: Bool -> Permissions -> Permissions
setReadable x perms = perms { readable = x }

-- | Enable @write@ permission.
setWritable :: Bool -> Permissions -> Permissions
setWritable x perms = perms { writable = x }

-- | Enable @execute@ permission.
setExecutable :: Bool -> Permissions -> Permissions
setExecutable x perms = perms { executable = x }

-- | Disable all permissions.
reset :: Permissions -> Permissions
reset = setReadable False . setWritable False . setExecutable False

-------------------------------------------------------------------------------
-- Roles
-------------------------------------------------------------------------------

-- | Roles to whom access is granted.
data Role =
      Owner
    | Group
    | Others
        deriving (Eq, Ord, Read, Show, Data)

-------------------------------------------------------------------------------
-- Mode parser
-------------------------------------------------------------------------------

{-
strParser :: MonadCatch m => Parser Char m String
strParser =
    let ut = Parser.char 'u'
            <|> Parser.char 'g'
            <|> Parser.char 'o'
            <|> Parser.char 'a'
        op = Parser.char '='    -- supports only override permissions bits
        p1 = Parser.char (chr 0)
            <|> Parser.char 'r'
            <|> Parser.char 'w'
            <|> Parser.char 'x'
        r  = ut *> op
        r1 = ut *> op *> p1
        r2 = ut *> op *> p1 *> p1
        r3 = ut *> op *> p1 *> p1 *> p1
        s =  r <|> r1 <|> r2 <|> r3
      in Parser.some s Fold.toList

expandVars :: String -> IO ()
expandVars ln =
    case Stream.parse strParser (Stream.fromList ln) of
        Left _ -> fail "Parsing of perm quoted string failed."
        Right _ -> return ()

parseExpr :: MonadIO m => String -> m [(Role, Permissions)]
parseExpr s = do
    liftIO $ expandVars s
    let ut = head s
        bits = tail $ tail s
    return $
        case ut of
            'u' -> UserTypePerm Owner $ setPermission bits
            'g' -> UserTypePerm Group $ setPermission bits
            'o' -> UserTypePerm Others $ setPermission bits
            'a' -> UserTypePerm All $ setPermission bits
            _ -> error "Invalid permissions"

    where

    setPermission bits =
        case bits of
            "rwx" -> Permissions True True True
            "rw" -> Permissions True True False
            "r" ->  Permissions True False False
            "w" ->  Permissions False True False
            "x" ->  Permissions False False True
            "rx" ->  Permissions True False True
            "wx" ->  Permissions False True True
            _ ->  def

quoteExprExp :: String -> Q Exp
quoteExprExp s =  do
    expr <- parseExpr s
    dataToExpQ (const Nothing) expr

quoteExprPat :: String -> Q Pat
quoteExprPat s = do
    expr <- parseExpr s
    dataToPatQ (const Nothing) expr

-- TODO: perms can have a single letter from the set ugo, in that case the
-- existing permissions are copied from that role.

-- When we get a "=" use 'reset', when we get a '+' use an operation with
-- argument True, else use False.

-- | The format of a symbolic mode is [roles][-+=][perms...], where roles is
-- either zero or more letters from the set ugoa. perms is either zero or more
-- letters from the set rwxXst. Multiple symbolic modes can be given, separated
-- by commas.
--
-- Examples:
--
-- @
-- -
-- -rwx
-- g-rx
-- g-x+r
-- go-x+rw
-- go-x+rw,u+r
-- @
--
-- If the role is omitted it is assumed to be 'a'.
mode :: QuasiQuoter
mode =
    QuasiQuoter
        { quoteExp = quoteExprExp
        , quotePat = quoteExprPat
        , quoteType = error "mode: quoteType not supported."
        , quoteDec = error "mode: quoteDec not supported."
        }
-}
