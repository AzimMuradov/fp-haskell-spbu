{-# LANGUAGE OverloadedStrings #-}

module ParserCombinators where

import qualified Ast
import Data.Char (chr, isNumber)
import Data.Functor (void)
import Data.Maybe (fromJust, listToMaybe)
import Data.Text (Text, concat, intercalate, pack, singleton, unpack)
import Data.Void (Void)
import GHC.Num (integerToInt)
import Numeric (readBin, readDec, readHex, readInt, readOct)
import Text.Megaparsec (MonadParsec (eof, notFollowedBy, takeP, try), Parsec, anySingle, between, choice, count, eitherP, many, oneOf, optional, satisfy, sepBy, some, (<?>), (<|>))
import Text.Megaparsec.Char (alphaNumChar, binDigitChar, char, char', digitChar, hexDigitChar, letterChar, newline, octDigitChar, space1, string)
import Text.Megaparsec.Char.Lexer (decimal)
import qualified Text.Megaparsec.Char.Lexer as Lexer
import Text.Read.Lex (readIntP)

type Parser output = Parsec Void Text output

-- Program

fileP :: Parser Ast.Program
fileP = programP <* eof

programP :: Parser Ast.Program
programP = Ast.Program <$> (sc *> many topLevelDeclP)

topLevelDeclP :: Parser Ast.TopLevelDecl
topLevelDeclP =
  choice
    [ Ast.TopLevelVarDecl <$> (varDeclP <* char ';'),
      Ast.TopLevelFunctionDecl <$> funcDeclP
    ]

varDeclP :: Parser [Ast.VarSpec]
varDeclP = undefined

funcDeclP :: Parser Ast.FunctionDecl
funcDeclP = undefined

-- Expression

{-
ExpressionList = { expressionP ~ ( "," ~ expressionP )* }

expressionP     = { UnaryExpr ~ (binaryOpP ~ UnaryExpr)* }

UnaryExpr      = { unaryOpP* ~ PrimaryExpr }
PrimaryExpr    = { Operand ~ ( Index | Arguments )* }

Operand        = { Literal | OperandName | "(" ~ expressionP ~ ")" }
OperandName    = { identifier }

indexP :: Parser
indexP          = between (char '[') (char ']') expressionP
Arguments      = { "(" ~ ( ( ExpressionList | Type ~ ( "," ~ ExpressionList )? ) ~ "..."? ~ ","? )? ~ ")" }
-}

-- Operators

assignOpP :: Parser Ast.AssignOp
assignOpP = do
  op <- optional $ eitherP addOpP mulOpP
  void $ char '='
  return $ maybe Ast.AssignOp Ast.ComplexAssignOp op

binaryOpP :: Parser Ast.BinaryOp
binaryOpP =
  choice
    [ Ast.OrOp <$ string "||",
      Ast.AndOp <$ string "&&",
      Ast.AddOp <$> addOpP,
      Ast.MulOp <$> mulOpP,
      Ast.RelOp <$> relOpP
    ]

relOpP :: Parser Ast.RelOp
relOpP =
  choice
    [ Ast.EqOp <$ string "==",
      Ast.NeOp <$ string "!=",
      Ast.LeOp <$ string "<=",
      Ast.LtOp <$ char '<',
      Ast.MeOp <$ string ">=",
      Ast.MtOp <$ char '>'
    ]

addOpP :: Parser Ast.AddOp
addOpP =
  choice
    [ Ast.PlusOp <$ char '+',
      Ast.MinusOp <$ char '-',
      Ast.BitOrOp <$ char '|',
      Ast.BitXorOp <$ char '^'
    ]

mulOpP :: Parser Ast.MulOp
mulOpP =
  choice
    [ Ast.MultOp <$ char '*',
      Ast.DivOp <$ char '/',
      Ast.ModOp <$ char '%',
      Ast.BitShiftLeftOp <$ string "<<",
      Ast.BitShiftRightOp <$ string ">>",
      Ast.BitClearOp <$ string "&^",
      Ast.BitAndOp <$ char '&'
    ]

unaryOpP :: Parser Ast.UnaryOp
unaryOpP =
  choice
    [ Ast.UnaryPlusOp <$ char '+',
      Ast.UnaryMinusOp <$ char '-',
      Ast.NotOp <$ char '!',
      Ast.BitwiseComplementOp <$ char '^'
    ]

{-
// Type

Type      = { TypeLit | TypeName | "(" ~ Type ~ ")" }
TypeName  = { identifier }
TypeLit   = { ArrayType | FunctionType }

// Function declaration (definition)

FunctionDecl = { "func" ~ FunctionName ~ Signature ~ FunctionBody }
FunctionName = { identifier }
FunctionBody = { Block }

FunctionType   = { "func" ~ Signature }
Signature      = { Parameters ~ Result? }
Result         = { Parameters | Type }
Parameters     = { "(" ~ ( ParameterList ~ ","? )? ~ ")" }
ParameterList  = { ParameterDecl ~ ( "," ~ ParameterDecl )* }
ParameterDecl  = { IdentifierList? ~ "..."? ~ Type }

// Statements

Statement = { ReturnStmt | BreakStmt | ContinueStmt | ForStmt | VarDecl | IfElseStmt | Block | SimpleStmt }

ReturnStmt   = { "return" ~ ExpressionList? }
BreakStmt    = { "break" }
ContinueStmt = { "continue" }

ForStmt   = { "for" ~ ( ForClause | Condition )? ~ Block }
ForClause = { SimpleStmt? ~ ";" ~ Condition? ~ ";" ~ SimpleStmt? }
Condition = { expressionP }

VarDecl     = { "var" ~ ( VarSpec | "(" ~ ( VarSpec ~ ";" )* ~ ")" ) }
VarSpec     = { IdentifierList ~ ( Type ~ ( "=" ~ ExpressionList )? | "=" ~ ExpressionList ) }

IfElseStmt = { "if" ~ ( SimpleStmt ~ ";" )? ~ expressionP ~ Block ~ ("else" ~ ( IfElseStmt | Block ))? }

SimpleStmt = { Assignment | IncDecStmt | ShortVarDecl | ExpressionStmt }

Assignment     = { ExpressionList ~ assign_op ~ ExpressionList }
IncDecStmt     = { expressionP ~ ( "++" | "--" ) }
ShortVarDecl   = { IdentifierList ~ ":=" ~ ExpressionList }
ExpressionStmt = { expressionP }

blockP = { "{" ~ ( Statement? ~ ";")* ~ "}" }
-}

-- Array

-- ArrayType   = { "[" ~ expressionP ~ "]" ~ Type }

-- Literal

literalP :: Parser Ast.Literal
literalP =
  choice
    [ Ast.StringLiteral <$> stringLitP,
      Ast.IntLiteral <$> intLitP
      -- , arrayLitP, functionLitP
    ]

-- Complex literals

{- functionLitP   = { "func" ~ Signature ~ FunctionBody }

arrayLitP  = { ArrayType ~ arrayLitValueP }
arrayLitValueP  = { "{" ~ ( ElementList ~ ","? )? ~ "}" }
elementListP   = { KeyedElement ~ ( "," ~ KeyedElement )* }
keyedElementP  = { ( Key ~ ":" )? ~ Element }
keyP           = { expressionP }
elementP       = { expressionP | LiteralValue } -}

-- Basic literals

stringLitP :: Parser Text
stringLitP = lexeme $ Data.Text.concat <$> between (char '"') (char '"') (some stringCharP)

stringCharP :: Parser Text
stringCharP = notFollowedBy (choice [newline, char '\\', char '"']) *> (singleton <$> anySingle) <|> escapedCharP

-- stringCharP :: Parser Text
-- stringCharP =
--   choice
--     [ notFollowedBy (choice [newline, char '\\', char '"']) *> (singleton <$> anySingle),
--       littleUValueP,
--       bigUValueP,
--       escapedCharP,
--       byteValueP
--     ]

-- byteValueP :: Parser Text
-- byteValueP = octalByteValueP <|> hexByteValueP

-- octalByteValueP :: Parser Text
-- octalByteValueP = pack <$> (char '\\' *> count 3 octDigitChar)

-- hexByteValueP :: Parser Text
-- hexByteValueP = pack <$> (string "\\x" *> count 2 hexDigitChar)

-- littleUValueP :: Parser Text
-- littleUValueP = pack <$> (string "\\u" *> count 4 hexDigitChar)

-- bigUValueP :: Parser Text
-- bigUValueP = do
--   s <- string "\\U"
--   cs <- count 8 hexDigitChar
--   let int = readInteger readHex cs
--   if int > 1000000000000 then fail "Ha-ha" else return $ pack $ show $ chr $ integerToInt int

escapedCharP :: Parser Text
escapedCharP =
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

intLitP :: Parser Integer
intLitP = binaryLitP <|> octalLitP <|> hexLitP <|> decimalLitP

decimalLitP :: Parser Integer
decimalLitP =
  lexeme $
    (0 <$ char '0') <|> do
      first <- oneOf ['1' .. '9'] <?> "first digit"
      other <- many $ char '_' *> digitChar
      return $ readInteger readDec $ first : other

binaryLitP :: Parser Integer
binaryLitP = abstractIntLitP (char' 'b') binDigitChar readBin

octalLitP :: Parser Integer
octalLitP = abstractIntLitP (optional $ char' 'o') octDigitChar readOct

hexLitP :: Parser Integer
hexLitP = abstractIntLitP (char' 'x') hexDigitChar readHex

abstractIntLitP :: Parser a -> Parser Char -> ReadS Integer -> Parser Integer
abstractIntLitP charIdP digitP reader = lexeme $ do
  void $ char '0' *> charIdP *> optional (char '_')
  intStr <- sepBy digitP $ optional $ char '_'
  return $ readInteger reader intStr

-- Identifier

identifierListP :: Parser [Ast.Identifier]
identifierListP = sepBy identifierP $ char ','

identifierP :: Parser Ast.Identifier
identifierP = lexeme $ do
  first <- letterP
  other <- many $ letterP <|> digitChar
  return $ Ast.Identifier $ pack $ first : other

letterP :: Parser Char
letterP = letterChar <|> char '_'

-- Lexer parts

sc :: Parser ()
sc = Lexer.space space1 (Lexer.skipLineComment "//") (Lexer.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = Lexer.lexeme sc

symbol :: Text -> Parser Text
symbol = Lexer.symbol sc

-- Utils

readInteger :: ReadS Integer -> String -> Integer
readInteger reader s = fst $ head $ reader s
