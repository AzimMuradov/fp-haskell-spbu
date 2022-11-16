{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module ParserCombinators where

import qualified Ast
import ConstExprSimplification (simplifyConstExpr)
import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Data.Char (chr, isNumber)
import Data.Functor (void, ($>))
import Data.List (partition)
import Data.Maybe (catMaybes, fromJust, listToMaybe, maybeToList)
import Data.Text (Text, concat, intercalate, pack, singleton, unpack)
import Data.Void (Void)
import GHC.Num (integerToInt)
import Numeric (readBin, readDec, readHex, readInt, readOct)
import Text.Megaparsec (MonadParsec (eof, notFollowedBy, takeP, try), Parsec, anySingle, between, choice, count, eitherP, many, oneOf, optional, satisfy, sepBy, sepBy1, sepEndBy, some, (<?>), (<|>))
import Text.Megaparsec.Char (alphaNumChar, binDigitChar, char, char', digitChar, hexDigitChar, letterChar, newline, octDigitChar, space1, string)
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Read.Lex (readIntP)

type Parser = Parsec Void Text

-- Program

fileP :: Parser Ast.Program
fileP = sc *> programP <* eof

programP :: Parser Ast.Program
programP = do
  decls <- many topLevelDeclP
  let vars = [x | Left x <- decls]
  let funcs = [x | Right x <- decls]
  return Ast.Program {Ast.topLevelVarDecls = vars, Ast.topLevelFunctionDefs = funcs}

topLevelDeclP :: Parser (Either Ast.VarDecl Ast.FunctionDef)
topLevelDeclP = eitherP (stmtVarDeclP <* char ';') functionDefP

-- Expression

expressionListP :: Parser [Ast.Expression]
expressionListP = sepBy expressionP $ symbol ","

expressionP :: Parser Ast.Expression
expressionP = makeExprParser expressionTerm opsTable

expressionTerm :: Parser Ast.Expression
expressionTerm =
  choice
    [ between (symbol "(") (symbol ")") expressionP,
      Ast.ExprLiteral <$> literalP,
      Ast.ExprIdentifier <$> identifierP
    ]

opsTable :: [[Operator Parser Ast.Expression]]
opsTable =
  [ [funcCallOp, arrayAccessByIndexOp],
    [ unaryOp "+" Ast.UnaryPlusOp,
      unaryOp "-" Ast.UnaryMinusOp,
      unaryOp "!" Ast.NotOp,
      unaryOp "^" Ast.BitwiseComplementOp
    ],
    [ mulOp "*" Ast.MultOp,
      mulOp "/" Ast.DivOp,
      mulOp "%" Ast.ModOp,
      mulOp "<<" Ast.BitShiftLeftOp,
      mulOp ">>" Ast.BitShiftRightOp,
      mulOp "&^" Ast.BitClearOp,
      mulOp "&" Ast.BitAndOp
    ],
    [ addOp "+" Ast.PlusOp,
      addOp "-" Ast.MinusOp,
      addOp "|" Ast.BitOrOp,
      addOp "^" Ast.BitXorOp
    ],
    [ relOp "==" Ast.EqOp,
      relOp "!=" Ast.NeOp,
      relOp "<=" Ast.LeOp,
      relOp "<" Ast.LtOp,
      relOp ">=" Ast.MeOp,
      relOp ">" Ast.MtOp
    ],
    [andOrOp "&&" Ast.AndOp],
    [andOrOp "||" Ast.OrOp]
  ]

-- Operator Types

binary :: Text -> (Ast.Expression -> Ast.Expression -> Ast.Expression) -> Operator Parser Ast.Expression
binary name fun = InfixL (fun <$ symbol name)

prefix :: Text -> (Ast.Expression -> Ast.Expression) -> Operator Parser Ast.Expression
prefix name fun = Prefix (fun <$ symbol name)

postfix :: Parser (Ast.Expression -> Ast.Expression) -> Operator Parser Ast.Expression
postfix = Postfix

-- Operators

andOrOp :: Text -> Ast.BinaryOp -> Operator Parser Ast.Expression
andOrOp name op = binary name (Ast.ExprBinaryOp op)

relOp :: Text -> Ast.RelOp -> Operator Parser Ast.Expression
relOp name op = binary name (Ast.ExprBinaryOp (Ast.RelOp op))

addOp :: Text -> Ast.AddOp -> Operator Parser Ast.Expression
addOp name op = binary name (Ast.ExprBinaryOp (Ast.AddOp op))

mulOp :: Text -> Ast.MulOp -> Operator Parser Ast.Expression
mulOp name op = binary name (Ast.ExprBinaryOp (Ast.MulOp op))

unaryOp :: Text -> Ast.UnaryOp -> Operator Parser Ast.Expression
unaryOp name op = prefix name (Ast.ExprUnaryOp op)

funcCallOp :: Operator Parser Ast.Expression
funcCallOp = postfix $ do
  args <- between (symbol "(") (symbol ")") $ sepEndBy expressionP $ symbol ","
  return $ \func -> Ast.ExprFuncCall func args

arrayAccessByIndexOp :: Operator Parser Ast.Expression
arrayAccessByIndexOp = postfix $ do
  indexExpr <- between (symbol "[") (symbol "]") expressionP
  index <- do
    case simplifyConstExpr indexExpr of
      Just (Ast.LitInt len) -> return len
      _ -> fail "this is not a const int expression"
  return $ \arr -> Ast.ExprArrayAccessByIndex arr index

-- Type

typeP :: Parser Ast.Type
typeP =
  choice
    [ Ast.TInt <$ symbol "int",
      Ast.TBool <$ symbol "bool",
      Ast.TString <$ symbol "string",
      -- TODO : arrayTypeP
      functionTypeP,
      between (symbol "(") (symbol ")") typeP
    ]

functionTypeP :: Parser Ast.Type
functionTypeP = do
  void $ symbol "func"
  paramsTypes <- typesP
  result <- typesP <|> maybeToList <$> optional typeP
  return $ Ast.TFunction {parameters = paramsTypes, result = result}
  where
    typesP = between (symbol "(") (symbol ")") (sepEndBy typeP $ symbol ",")

-- Function definition

functionDefP :: Parser Ast.FunctionDef
functionDefP = do
  void $ symbol "func"
  name <- identifierP
  signature <- functionSignatureP
  body <- stmtBlockP
  return $ Ast.FunctionDef {name = name, signature = signature, body = body}

functionSignatureP :: Parser Ast.FunctionSignature
functionSignatureP = do
  params <- between (symbol "(") (symbol ")") (sepEndBy paramP $ symbol ",")
  result <- between (symbol "(") (symbol ")") (sepEndBy typeP $ symbol ",") <|> maybeToList <$> optional typeP
  return $ Ast.FunctionSignature {parameters = params, result = result}
  where
    paramP = do n <- identifierP; t <- typeP; return (n, t)

-- Statements

statementP :: Parser Ast.Statement
statementP =
  choice
    [ stmtReturnP,
      stmtBreakP,
      stmtContinueP,
      -- stmtForP,
      Ast.StmtVarDecl <$> stmtVarDeclP,
      Ast.StmtIfElse <$> stmtIfElseP,
      Ast.StmtBlock <$> stmtBlockP,
      Ast.StmtSimple <$> simpleStmtP
    ]

stmtReturnP :: Parser Ast.Statement
stmtReturnP = symbol "return" $> Ast.StmtReturn <*> expressionListP

stmtBreakP :: Parser Ast.Statement
stmtBreakP = Ast.StmtBreak <$ symbol "break"

stmtContinueP :: Parser Ast.Statement
stmtContinueP = Ast.StmtContinue <$ symbol "continue"

-- TODO : Add For

stmtForP :: Parser Ast.Statement
stmtForP = undefined

-- ForStmt   = { "for" ~ ( ForClause | Condition )? ~ Block }
-- ForClause = { SimpleStmt? ~ ";" ~ Condition? ~ ";" ~ SimpleStmt? }
-- Condition = { expressionP }

-- TODO : Add support for more complex assignments

-- VarDecl     = { "var" ~ ( VarSpec | "(" ~ ( VarSpec ~ ";" )* ~ ")" ) }
-- VarSpec     = { IdentifierList ~ ( Type ~ ( "=" ~ ExpressionList )? | "=" ~ ExpressionList ) }

stmtVarDeclP :: Parser Ast.VarDecl
stmtVarDeclP =
  Ast.VarDecl <$> do
    void $ symbol "var"
    between (symbol "(") (symbol ")") (sepEndBy stmtVarSpecP $ symbol ";") <|> (: []) <$> stmtVarSpecP

stmtVarSpecP :: Parser Ast.VarSpec
stmtVarSpecP = Ast.VarSpec <$> identifierListP <*> optional typeP <*> expressionListP

stmtIfElseP :: Parser Ast.IfElse
stmtIfElseP = do
  void $ symbol "if"
  stmt <- optional $ simpleStmtP <* symbol ";"
  condition <- expressionP
  block <- stmtBlockP
  -- TODO : Add Else ("else" ~ ( IfElseStmt | Block ))?
  return $ Ast.IfElse {simpleStmt = stmt, condition = condition, block = block, elseStmt = Right []}

stmtBlockP :: Parser [Ast.Statement]
stmtBlockP = between (symbol "{") (symbol "}") $ catMaybes <$> many (optional statementP <* symbol ";")

simpleStmtP :: Parser Ast.SimpleStmt
simpleStmtP = choice [stmtAssignmentP, stmtIncP, stmtDecP, stmtShortVarDeclP, stmtExpressionP]

-- TODO : Add support for more complex assignments

stmtAssignmentP :: Parser Ast.SimpleStmt
stmtAssignmentP =
  Ast.StmtAssignment
    <$> do ids <- identifierListP; return $ Ast.AssignmentLhsVar <$> ids
    <* symbol "="
    <*> expressionListP

stmtIncP :: Parser Ast.SimpleStmt
stmtIncP = Ast.StmtInc <$> expressionP <* symbol "++"

stmtDecP :: Parser Ast.SimpleStmt
stmtDecP = Ast.StmtDec <$> expressionP <* symbol "--"

stmtShortVarDeclP :: Parser Ast.SimpleStmt
stmtShortVarDeclP = Ast.StmtShortVarDecl <$> identifierListP <* symbol ":=" <*> expressionListP

stmtExpressionP :: Parser Ast.SimpleStmt
stmtExpressionP = Ast.StmtExpression <$> expressionP

-- Array

arrayTypeP :: Parser Ast.ArrayType
arrayTypeP = do
  lenExpr <- between (symbol "[") (symbol "]") expressionP
  len <- do
    case simplifyConstExpr lenExpr of
      Just (Ast.LitInt len) -> return len
      _ -> fail "this is not a const int expression"
  t <- typeP
  return Ast.ArrayType {Ast.elementType = t, Ast.length = len}

-- Literal

literalP :: Parser Ast.Literal
literalP =
  choice
    [ Ast.LitInt <$> intLitP,
      Ast.LitBool <$> boolLitP,
      Ast.LitString <$> stringLitP
      -- TODO : arrayLitP, functionLitP
    ]

-- Complex literals

-- TODO : Implement Function Literals

-- functionLitP   = { "func" ~ Signature ~ FunctionBody }

-- TODO : Implement Array Literals

arrayLitP :: Parser Ast.Literal
arrayLitP = do
  t <- arrayTypeP
  value <- arrayLitValueP
  return Ast.LitArray {Ast.t = t, Ast.value = value}

arrayLitValueP :: Parser [Ast.Element]
arrayLitValueP = undefined

-- ArrayLiteral      = { ArrayType ~ ArrayLiteralValue }
-- ArrayLiteralValue = { "{" ~ ( KeyedElementList ~ ","? )? ~ "}" }
-- KeyedElementList  = { KeyedElement ~ ( "," ~ KeyedElement )* }
-- KeyedElement      = { ( Key ~ ":" )? ~ Element }
-- Key               = { Expression }
-- Element           = { Expression | ArrayLiteralValue }

-- Basic literals

intLitP :: Parser Int
intLitP =
  choice
    [ try binaryLitP <?> "binary number literal",
      try octalLitP <?> "octal number literal",
      try hexLitP <?> "hex number literal",
      try decimalLitP <?> "decimal number literal"
    ]

decimalLitP :: Parser Int
decimalLitP =
  lexeme $
    (0 <$ char '0') <|> do
      first <- oneOf ['1' .. '9'] <?> "first not-zero decimal digit (1..9)"
      other <- many $ optional (char '_') *> digitChar
      return $ readInteger readDec $ first : other

binaryLitP :: Parser Int
binaryLitP = abstractIntLitP (char' 'b') binDigitChar readBin

octalLitP :: Parser Int
octalLitP = abstractIntLitP (optional $ char' 'o') octDigitChar readOct

hexLitP :: Parser Int
hexLitP = abstractIntLitP (char' 'x') hexDigitChar readHex

abstractIntLitP :: Parser a -> Parser Char -> ReadS Integer -> Parser Int
abstractIntLitP charIdP digitP reader = lexeme $ do
  void $ char '0' *> charIdP *> optional (char '_')
  intStr <- sepBy1 digitP $ optional $ char '_'
  return $ readInteger reader intStr

boolLitP :: Parser Bool
boolLitP = lexeme $ (True <$ string "true") <|> (False <$ string "false")

stringLitP :: Parser Text
stringLitP = lexeme $ Data.Text.concat <$> between (char '"') (char '"') (some stringCharP)

stringCharP :: Parser Text
stringCharP = notFollowedBy (choice [newline, char '\\', char '"']) *> (singleton <$> anySingle) <|> escapedCharP

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

-- Identifier

identifierListP :: Parser [Ast.Identifier]
identifierListP = sepBy identifierP $ symbol ","

identifierP :: Parser Ast.Identifier
identifierP =
  lexeme $
    Ast.Identifier
      <$> ( notFollowedBy (keywordP <|> predeclaredIdentifierP) *> do
              first <- letterP
              other <- many $ letterP <|> digitChar
              return $ pack $ first : other
          )

letterP :: Parser Char
letterP = letterChar <|> char '_'

keywordP :: Parser Text
keywordP = choice $ symbol <$> ["break", "func", "if", "else", "continue", "for", "return", "var"]

predeclaredIdentifierP :: Parser Text
predeclaredIdentifierP = choice $ symbol <$> ["bool", "int", "string", "true", "false", "nil", "len", "panic", "print", "println", "recover"]

-- Lexer parts

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

-- Utils

-- TODO : Use const expressions simplification

readInteger :: ReadS Integer -> String -> Int
readInteger reader s = fromIntegral $ fst $ head $ reader s
