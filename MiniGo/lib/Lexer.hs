{-# LANGUAGE OverloadedStrings #-}

module Lexer where

import Control.Applicative.Combinators (between, sepEndBy)
import Control.Monad (void)
import Data.Text (Text, pack, singleton)
import Data.Void (Void)
import Numeric (readBin, readDec, readHex, readOct)
import Text.Megaparsec (MonadParsec (..), Parsec, anySingle, choice, many, oneOf, optional, sepBy1, (<|>))
import Text.Megaparsec.Char (binDigitChar, char, char', digitChar, hexDigitChar, letterChar, newline, octDigitChar, space1)
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

-- Basic lexer parts

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

-- Symbols

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

semicolon :: Parser Text
semicolon = symbol ";"

comma :: Parser Text
comma = symbol ","

listed :: Parser a -> Parser Text -> Parser [a]
listed p sep = parens $ sepEndBy p sep

-- Basic literals

int :: Parser Integer
int = choice $ try <$> [binaryInt, octalInt, hexInt, decimalInt]

decimalInt :: Parser Integer
decimalInt =
  lexeme $
    (0 <$ char '0') <|> do
      first <- oneOf ['1' .. '9']
      other <- many $ optional (char '_') *> digitChar
      return $ readInteger readDec $ first : other

binaryInt :: Parser Integer
binaryInt = abstractInt (char' 'b') binDigitChar readBin

octalInt :: Parser Integer
octalInt = abstractInt (optional $ char' 'o') octDigitChar readOct

hexInt :: Parser Integer
hexInt = abstractInt (char' 'x') hexDigitChar readHex

abstractInt :: Parser a -> Parser Char -> ReadS Integer -> Parser Integer
abstractInt charIdP digitP reader = lexeme $ do
  void $ char '0' *> charIdP *> optional (char '_')
  intStr <- sepBy1 digitP $ optional $ char '_'
  return $ readInteger reader intStr

stringChar :: Parser Text
stringChar = notFollowedBy (choice [newline, char '\\', char '"']) *> (singleton <$> anySingle) <|> escapedChar

escapedChar :: Parser Text
escapedChar =
  char '\\'
    *> choice
      [ "\a" <$ char 'a',
        "\b" <$ char 'b',
        "\f" <$ char 'f',
        "\n" <$ char 'n',
        "\r" <$ char 'r',
        "\t" <$ char 't',
        "\v" <$ char 'v',
        "\\" <$ char '\\',
        "\"" <$ char '\"'
      ]

-- Identifier

identifierTextP :: Parser Text
identifierTextP =
  notFollowedBy (predeclaredIdentifierTextP <|> keywordTextP) *> do
    first <- letterP
    other <- many $ letterP <|> digitChar
    return $ pack $ first : other
  where
    letterP = letterChar <|> char '_'

-- Reserved

keywordTextP :: Parser Text
keywordTextP = choice [kwVar, kwFunc, kwReturn, kwIf, kwElse, kwFor, kwBreak, kwContinue]

kwVar = symbol "var"

kwFunc = symbol "func"

kwReturn = symbol "return"

kwIf = symbol "if"

kwElse = symbol "else"

kwFor = symbol "for"

kwBreak = symbol "break"

kwContinue = symbol "continue"

predeclaredIdentifierTextP :: Parser Text
predeclaredIdentifierTextP = choice [idBool, idInt, idString, idTrue, idFalse, idNil, stdlibFuncTextP]

idBool = symbol "bool"

idInt = symbol "int"

idString = symbol "string"

idTrue = symbol "true"

idFalse = symbol "false"

idNil = symbol "nil"

stdlibFuncTextP :: Parser Text
stdlibFuncTextP = choice $ symbol <$> [funcNameLen, funcNamePanic, funcNamePrintLn, funcNamePrint]

funcNameLen = "len"

funcNamePanic = "panic"

funcNamePrintLn = "println"

funcNamePrint = "print"

-- Utils

readInteger :: ReadS Integer -> String -> Integer
readInteger reader s = fst $ head $ reader s
