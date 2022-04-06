-- |
-- Module      : Streamly.Coreutils.ShellWords
-- Copyright   : (c) 2022 Composewell Technologies
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Parse a string into words using shell escaping rules.

module Streamly.Coreutils.ShellWords
    (
      escapedChar
    , escapedWord
    , quotedString
    , flag
    , shellWord
    , shellWords
    )
where

import Control.Applicative ((<|>), optional)
import Control.Monad.Catch (MonadCatch)
import Data.Char (isSpace)
import Data.Foldable (asum)
import Data.Maybe (fromMaybe)
import Streamly.Internal.Data.Parser (Parser)

import qualified Streamly.Internal.Data.Fold as Fold
import qualified Streamly.Internal.Data.Parser as Parser
import qualified Streamly.Internal.Unicode.Char.Parser as Parser

-------------------------------------------------------------------------------
-- Parsing utils
-------------------------------------------------------------------------------

-- | Match the input with the supplied string and return it if successful.
string :: (MonadCatch m, Eq a) => [a] -> Parser m a [a]
string s = s <$ Parser.eqBy (==) s

-- | Parse a single item of a series of items separated by the supplied
-- separator.
{-# INLINE sepBy1 #-}
sepBy1 :: MonadCatch m => Parser m a b -> Parser m a c -> Parser m a [b]
sepBy1 p sep = do
    x <- p
    xs <- Parser.many (sep >> p) Fold.toList
    return (x:xs)

-- | sepBy1 or empty, does not fail.
{-# INLINE sepBy #-}
sepBy :: MonadCatch m => Parser m a b -> Parser m a c -> Parser m a [b]
sepBy p sep = sepBy1 p sep <|> return []

-- | Parse exactly one item irrespective of what it is.
one :: MonadCatch m => Parser m a a
one = Parser.satisfy $ const True

-- | Skip one or more white space characters.
space1 :: MonadCatch m => Parser m Char ()
space1 = Parser.takeWhile1 isSpace Fold.drain

-------------------------------------------------------------------------------
-- Escaped words
-------------------------------------------------------------------------------

-- | Parse a char escaped by a backslash
escapedChar :: MonadCatch m => Char -> Parser m Char Char
escapedChar c = c <$ Parser.eqBy (==) ("\\" <> [c])

-- | Take a non-space character, honor escaping of space.
notSpace :: MonadCatch m => Parser m Char Char
notSpace =
        escapedChar ' '
    <|> Parser.satisfy (not . isSpace)

-- | Take a token that is not space char and not the given char. Honors
-- escaping of space or the supplied char.
notSpaceOr :: MonadCatch m => Char -> Parser m Char Char
notSpaceOr ch =
        escapedChar ' '
    <|> escapedChar ch
    <|> Parser.satisfy (\c -> not (isSpace c) && (c /= ch))

-- | Take characters until an unescaped space is encountered.
escapedWord :: MonadCatch m => Parser m Char String
escapedWord = Parser.many notSpace Fold.toList

-------------------------------------------------------------------------------
-- Quoted strings
-------------------------------------------------------------------------------

-- | Single or double quoted string with balanced quotes. The string may
-- contain escaped quotes.
quotedString :: MonadCatch m => Parser m Char String
quotedString = do
    q <- Parser.satisfy (\c -> elem c ['\'', '\"'])
    Parser.manyTill (escapedChar q <|> one) (Parser.char q) Fold.toList

-------------------------------------------------------------------------------
-- Flags
-------------------------------------------------------------------------------

-- | Equal sign followed by a string e.g. in an assignment like @k=value@.
assignment :: MonadCatch m => Parser m Char String
assignment = (:) <$> Parser.char '=' <*> (quotedString <|> escapedWord)

-- | A flag, for example, @-flag@, @--flag@, or @--\"quoted flag\"@ without an
-- assignment.
flagWithoutAssign :: MonadCatch m => Parser m Char String
flagWithoutAssign =
    (<>)
        <$> (string "--" <|> string "-")
        <*> (quotedString <|> Parser.many (notSpaceOr '=') Fold.toList)

-- | A flag, with or without an assignment
flag :: MonadCatch m => Parser m Char String
flag =
    (<>)
        <$> flagWithoutAssign
        <*> (fromMaybe "" <$> optional assignment)

-------------------------------------------------------------------------------
-- Shell escaped parsing
-------------------------------------------------------------------------------

-- | Parse an escaped word, a quoted string or a flag with or without
-- assignment using an equal sign.
shellWord :: MonadCatch m => Parser m Char String
shellWord = asum [escapedWord, quotedString, flag]

-- | Parse many shell words. See 'shellWord'.
shellWords :: MonadCatch m => Parser m Char [String]
shellWords = shellWord `sepBy` space1
