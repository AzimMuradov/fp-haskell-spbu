{-# LANGUAGE OverloadedStrings #-}

module Lexer where

import qualified Ast (Identifier)
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

identifierP :: Parser Ast.Identifier
identifierP =
  lexeme $
    notFollowedBy (predeclaredIdentifierP <|> keywordP) *> do
      first <- letterP
      other <- many $ letterP <|> digitChar
      return $ pack $ first : other
  where
    letterP = letterChar <|> char '_'

-- Reserved

keywordP :: Parser Ast.Identifier
keywordP = choice [kwVar, kwFunc, kwReturn, kwIf, kwElse, kwFor, kwBreak, kwContinue]

kwVar :: Parser Text
kwVar = symbol "var"

kwFunc :: Parser Text
kwFunc = symbol "func"

kwReturn :: Parser Text
kwReturn = symbol "return"

kwIf :: Parser Text
kwIf = symbol "if"

kwElse :: Parser Text
kwElse = symbol "else"

kwFor :: Parser Text
kwFor = symbol "for"

kwBreak :: Parser Text
kwBreak = symbol "break"

kwContinue :: Parser Text
kwContinue = symbol "continue"

predeclaredIdentifierP :: Parser Ast.Identifier
predeclaredIdentifierP = choice [idBool, idInt, idString, idTrue, idFalse, idNil, stdlibFuncP]

idBool :: Parser Text
idBool = symbol "bool"

idInt :: Parser Text
idInt = symbol "int"

idString :: Parser Text
idString = symbol "string"

idTrue :: Parser Text
idTrue = symbol "true"

idFalse :: Parser Text
idFalse = symbol "false"

idNil :: Parser Text
idNil = symbol "nil"

stdlibFuncP :: Parser Ast.Identifier
stdlibFuncP = choice $ symbol <$> [funcNameLen, funcNamePanic, funcNamePrintLn, funcNamePrint]

funcNameLen :: Text
funcNameLen = "len"

funcNamePanic :: Text
funcNamePanic = "panic"

funcNamePrintLn :: Text
funcNamePrintLn = "println"

funcNamePrint :: Text
funcNamePrint = "print"

-- Utils

readInteger :: ReadS Integer -> String -> Integer
readInteger reader s = fst $ head $ reader s
