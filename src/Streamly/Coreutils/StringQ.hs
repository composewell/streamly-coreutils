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

module Streamly.Coreutils.StringQ
    (
      perm
    , UserType(..)
    , Permissions(..)
    , UserTypePerm(..)
    )
where

import Control.Applicative (Alternative(..))
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.Char (chr)
import Data.Data (Data, Typeable)
import Data.Default.Class (Default(..))
import Language.Haskell.TH (Exp, Q, Pat)
import Language.Haskell.TH.Quote (QuasiQuoter(..), dataToExpQ, dataToPatQ)
import Streamly.Internal.Data.Parser (Parser)

import qualified Streamly.Internal.Data.Fold as Fold
import qualified Streamly.Internal.Data.Parser as Parser
import qualified Streamly.Internal.Data.Stream.IsStream as Stream
import qualified Streamly.Internal.Unicode.Char.Parser as Parser

strParser :: MonadCatch m => Parser m Char String
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

data Permissions = Permissions
  { readable :: Bool
  , writable :: Bool
  , executable :: Bool
  } deriving (Eq, Ord, Read, Show, Typeable, Data)

data UserType =
      Owner
    | Group
    | Others
    | All
        deriving (Eq, Ord, Read, Show, Typeable, Data)

data UserTypePerm =
    UserTypePerm
    { utype :: UserType
    , permssions :: Permissions
    } deriving (Eq, Ord, Read, Show, Typeable, Data)

instance Default Permissions where
    def = Permissions
        { readable = False
        , writable = False
        , executable = False
        }

parseExpr :: MonadIO m => String -> m UserTypePerm
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

perm :: QuasiQuoter
perm =
    QuasiQuoter
        { quoteExp = quoteExprExp
        , quotePat = quoteExprPat
        , quoteType = notSupported
        , quoteDec = notSupported
        }

    where

    notSupported = error "perm: Not supported."
