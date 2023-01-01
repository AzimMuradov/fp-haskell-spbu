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
scn = L.space space1 lineComment blockComment

sc :: Parser ()
sc = L.space (void $ some (char ' ' <|> char '\t')) lineComment blockComment

-- TextParsingHelpers

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

-- SmartChoice
choice' :: (Foldable f, MonadParsec e s m, Functor f) => f (m a) -> m a
choice' x = choice $ try <$> x

-- Symbols

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

mlparens :: Parser a -> Parser a
mlparens = between (symbol "<") (symbol ">")

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

semicolon :: Parser Text
semicolon = symbol ";"

comma :: Parser Text
comma = symbol ","

-- Keywords

-- KeywordParser
keywordP :: Parser Text
keywordP = choice [kIf, kThen, kElse, kLet, kRec, kIn, kMeasure, kType]

-- ifParser
kIf :: Parser Text
kIf = symbol "if"

-- thenParser
kThen :: Parser Text
kThen = symbol "then"

-- elseParser
kElse :: Parser Text
kElse = symbol "else"

-- letParser
kLet :: Parser Text
kLet = symbol "let"

-- recParser
kRec :: Parser Text
kRec = symbol "rec"

-- inParser
kIn :: Parser Text
kIn = symbol "in"

-- MeasureParser
kMeasure :: Parser Text
kMeasure = symbol "[<Measure>]"

-- TypeParser
kType :: Parser Text
kType = symbol "type"