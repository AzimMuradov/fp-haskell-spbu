{-# LANGUAGE OverloadedStrings #-}

module Parser.Lexer where

import Control.Monad (void)
import Data.Text (Text, concat, pack, singleton)
import Data.Void (Void)
import Numeric (readBin, readDec, readHex, readOct)
import qualified Parser.Ast as Ast (Identifier)
import Text.Megaparsec (MonadParsec (..), Parsec, anySingle, between, choice, many, oneOf, optional, sepBy1, sepEndBy, sepEndBy1, (<|>))
import Text.Megaparsec.Char (binDigitChar, char, char', digitChar, hexDigitChar, letterChar, newline, octDigitChar, space1)
import qualified Text.Megaparsec.Char.Lexer as L
import Prelude hiding (concat)

---------------------------------------------------Basic lexer parts----------------------------------------------------

-- | Parser monad.
type Parser = Parsec Void Text

-- | Space consumer, parses whitespace and comments.
sc :: Parser ()
sc = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

-- | Lexeme, automatically parses trailing whitespace and comments.
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- | Symbol, automatically parses trailing whitespace and comments.
symbol :: Text -> Parser Text
symbol = L.symbol sc

--------------------------------------------------------Symbols---------------------------------------------------------

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

listed1 :: Parser a -> Parser Text -> Parser [a]
listed1 p sep = parens $ sepEndBy1 p sep

--------------------------------------------------------Literals--------------------------------------------------------

-- | Integer literal parser.
intLitP :: Parser Integer
intLitP = choice $ try <$> [binaryInt, octalInt, hexInt, decimalInt]

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

-- | Boolean literal parser.
boolLitP :: Parser Bool
boolLitP = True <$ idTrue <|> False <$ idFalse

-- | String literal parser.
stringLitP :: Parser Text
stringLitP = lexeme $ concat <$> between (char '"') (char '"') (many stringChar)

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

------------------------------------------------Identifiers and reserved------------------------------------------------

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

-- Keywords

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

-- Predeclared identifiers

predeclaredIdentifierP :: Parser Ast.Identifier
predeclaredIdentifierP = choice [idBool, idInt, idString, idTrue, idFalse, idNil]

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

---------------------------------------------------------Utils----------------------------------------------------------

readInteger :: ReadS Integer -> String -> Integer
readInteger reader s = fst $ head $ reader s
