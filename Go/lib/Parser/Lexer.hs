{-# LANGUAGE OverloadedStrings #-}

-- | Provides lexer parts for the [Parser]("Parser.Parser") module.
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

-- * Basic lexer parts

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

-- * Symbols

-- | Wraps given parser with parenthesis.
parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

-- | Wraps given parser with braces.
braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

-- | Wraps given parser with brackets.
brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

-- | Comma parser.
comma :: Parser Text
comma = symbol ","

-- | Colon parser.
colon :: Parser Text
colon = symbol ":"

-- | Semicolon parser.
semicolon :: Parser Text
semicolon = symbol ";"

-- | List parser.
listed :: Parser a -> Parser Text -> Parser [a]
listed p sep = parens $ sepEndBy p sep

-- | Non-empty list parser.
listed1 :: Parser a -> Parser Text -> Parser [a]
listed1 p sep = parens $ sepEndBy1 p sep

--------------------------------------------------------Literals--------------------------------------------------------

-- * Literals

-- | Integer literal parser.
intLitP :: Parser Integer
intLitP = choice $ try <$> [binaryInt, octalInt, hexInt, decimalInt]

-- | Decimal integer literal parser.
decimalInt :: Parser Integer
decimalInt =
  lexeme $
    (0 <$ char '0') <|> do
      first <- oneOf ['1' .. '9']
      other <- many $ optional (char '_') *> digitChar
      return $ readInteger readDec $ first : other

-- | Binary integer literal parser.
binaryInt :: Parser Integer
binaryInt = abstractInt (char' 'b') binDigitChar readBin

-- | Octal integer literal parser.
octalInt :: Parser Integer
octalInt = abstractInt (optional $ char' 'o') octDigitChar readOct

-- | Hex integer literal parser.
hexInt :: Parser Integer
hexInt = abstractInt (char' 'x') hexDigitChar readHex

-- | Abstract integer parser, encapsulates integer parser structure.
abstractInt :: Parser a -> Parser Char -> ReadS Integer -> Parser Integer
abstractInt charIdP digitP reader = lexeme $ do
  void $ char '0' *> charIdP *> optional (char '_')
  intStr <- sepBy1 digitP $ optional $ char '_'
  return $ readInteger reader intStr

-- | Parse integer using given reader and integer string.
readInteger :: ReadS Integer -> String -> Integer
readInteger reader s = fst $ head $ reader s

-- | Boolean literal parser.
boolLitP :: Parser Bool
boolLitP = True <$ idTrue <|> False <$ idFalse

-- | String literal parser.
stringLitP :: Parser Text
stringLitP = lexeme $ concat <$> between (char '"') (char '"') (many stringChar)

-- | String character parser.
stringChar :: Parser Text
stringChar = notFollowedBy (choice [newline, char '\\', char '"']) *> (singleton <$> anySingle) <|> escapedChar

-- | Escaped character parser.
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

-- * Identifiers and reserved

-- ** Identifier

-- | Custom identifier parser.
identifierP :: Parser Ast.Identifier
identifierP =
  lexeme $
    notFollowedBy (predeclaredIdentifierP <|> keywordP) *> do
      first <- letterP
      other <- many $ letterP <|> digitChar
      return $ pack $ first : other
  where
    letterP = letterChar <|> char '_'

-- ** Keywords

-- | Keyword parser.
keywordP :: Parser Text
keywordP = choice [kwVar, kwFunc, kwReturn, kwIf, kwElse, kwFor, kwBreak, kwContinue]

-- | @var@ keyword parser.
kwVar :: Parser Text
kwVar = symbol "var"

-- | @func@ keyword parser.
kwFunc :: Parser Text
kwFunc = symbol "func"

-- | @return@ keyword parser.
kwReturn :: Parser Text
kwReturn = symbol "return"

-- | @if@ keyword parser.
kwIf :: Parser Text
kwIf = symbol "if"

-- | @else@ keyword parser.
kwElse :: Parser Text
kwElse = symbol "else"

-- | @for@ keyword parser.
kwFor :: Parser Text
kwFor = symbol "for"

-- | @break@ keyword parser.
kwBreak :: Parser Text
kwBreak = symbol "break"

-- | @continue@ keyword parser.
kwContinue :: Parser Text
kwContinue = symbol "continue"

-- ** Predeclared identifiers

-- | Predeclared identifier parser.
predeclaredIdentifierP :: Parser Ast.Identifier
predeclaredIdentifierP = choice [idBool, idInt, idString, idTrue, idFalse, idNil]

-- | @bool@ identifier parser.
idBool :: Parser Text
idBool = symbol "bool"

-- | @int@ identifier parser.
idInt :: Parser Text
idInt = symbol "int"

-- | @string@ identifier parser.
idString :: Parser Text
idString = symbol "string"

-- | @true@ identifier parser.
idTrue :: Parser Text
idTrue = symbol "true"

-- | @false@ identifier parser.
idFalse :: Parser Text
idFalse = symbol "false"

-- | @nil@ identifier parser.
idNil :: Parser Text
idNil = symbol "nil"
