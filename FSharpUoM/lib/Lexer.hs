{-# LANGUAGE OverloadedStrings #-}

module Lexer where

import Control.Applicative.Combinators (between, sepEndBy)

import Data.Void (Void)
import Data.Text (Text, pack, singleton)

import Numeric (readDec)

import Text.Megaparsec (MonadParsec (..), Parsec, anySingle, choice, many, some, noneOf, oneOf, optional, sepBy1, (<|>))
import Text.Megaparsec.Char (binDigitChar, char, char', digitChar, hexDigitChar, letterChar, newline, octDigitChar, space1)

import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad (void)
import Control.Applicative (empty)

type Parser = Parsec Void Text

-- Comments
lineComment :: Parser ()
lineComment = L.skipLineComment "//"

blockComment :: Parser ()
blockComment = L.skipBlockComment "(*" "*)"

-- SpaceConsumers
scn :: Parser ()
scn = L.space space1 lineComment empty

sc :: Parser ()
sc = L.space (void $ some (char ' ' <|> char '\t')) lineComment empty

-- TextParsingHelpers

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

-- Symbols

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

semicolon :: Parser Text
semicolon = symbol ";"

comma :: Parser Text
comma = symbol ","