{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Parser where

import Ast
import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Data.Text (Text, concat, pack)
import Data.Void (Void)
import Lexer (lexeme, parens, scn, symbol)
import Numeric (readDec)
import Text.Megaparsec (MonadParsec (..), Parsec, choice, eitherP, many, oneOf, optional, sepBy, sepBy1, some, (<|>))
import Text.Megaparsec.Char (alphaNumChar, char, digitChar, letterChar, string)
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

-- >>> 1 + 1

programP :: Parser Program
programP =
  Program
    <$> many
      ( choice $
          try
            <$> [ TopLevelVarDecl <$> varP,
                  TopLevelRecFunDecl <$> recFunP,
                  TopLevelFunDecl <$> funP
                ]
      )

varP :: Parser VarDecl
varP = VarDecl <$ symbol "let" <*> typedIdentifierP <* symbol "=" <*> exprP

funP :: Parser FunDecl
funP = FunDecl <$ symbol "let" <*> identifierP <*> some typedIdentifierP <* symbol "=" <*> exprP

recFunP :: Parser RecFunDecl
recFunP = RecFunDecl <$ symbol "let" <* symbol "rec" <*> identifierP <*> some typedIdentifierP <* symbol "=" <*> exprP

identifierP :: Parser Identifier
identifierP =
  lexeme $
    Identifier <$> do
      first <- letterP
      other <- many $ letterP <|> digitChar
      return $ pack $ first : other
  where
    letterP = letterChar <|> char '_'

typedIdentifierP :: Parser (Identifier, Maybe Type)
typedIdentifierP = do
  choice $
    try
      <$> [ parens ((,) <$> identifierP <*> (optional . try) (symbol ":" *> typeP)),
            (,) <$> identifierP <*> pure Nothing
          ]

typeP :: Parser Type
typeP =
  choice $
    try
      <$> [ TInt <$ symbol "int",
            TBool <$ symbol "bool",
            TFun <$ sepBy1 typeP (symbol "->"),
            parens typeP
          ]

exprP :: Parser Expr
exprP = makeExprParser exprTerm opsTable

exprTerm :: Parser Expr
exprTerm =
  choice $
    try
      <$> [ parens exprP,
            ENull <$ symbol "null",
            ELetIn <$ symbol "let" <*> identifierP <* symbol "=" <*> exprP <* symbol "in" <*> statementP,
            EValue <$> valueP,
            EFun <$ symbol "fun" <*> many typedIdentifierP <* symbol "->" <*> statementP,
            EIf <$ symbol "if" <*> exprP <* symbol "then" <*> statementP <* symbol "else" <*> statementP,
            EApplication <$> identifierP <*> many exprP,
            EIdentifier <$> identifierP
          ]

opsTable :: [[Operator Parser Expr]]
opsTable =
  [ [prefix "-" UnaryMinus],
    [arithmeticOp "**" ExpOp],
    [arithmeticOp "*" MulOp, arithmeticOp "/" DivOp, arithmeticOp "%" ModOp],
    [arithmeticOp "+" PlusOp, arithmeticOp "-" MinusOp],
    [bitwiseOp "^^^" BitShiftRightOp],
    [ comparisonOp "==" EqOp,
      comparisonOp "<>" NeOp,
      comparisonOp "<" LtOp,
      comparisonOp "<=" LeOp,
      comparisonOp ">" MtOp,
      comparisonOp ">=" MeOp,
      bitwiseOp "|||" BitOrOp,
      bitwiseOp "&&&" BitAndOp,
      bitwiseOp "<<<" BitShiftLeftOp,
      bitwiseOp ">>>" BitShiftRightOp
    ],
    [booleanOp "&&" AndOp],
    [booleanOp "||" OrOp],
    [prefix "not" NotOp]
  ]

valueP :: Parser Value
valueP =
  choice $
    try
      <$> [ VBool <$> boolLitP, -- add fun mb
            VInt <$> decimalIntP
          ]

boolLitP :: Parser Bool
boolLitP = True <$ symbol "true" <|> False <$ symbol "false"

decimalIntP :: Parser Integer
decimalIntP =
  lexeme $
    (0 <$ char '0') <|> do
      first <- oneOf ['1' .. '9']
      other <- many digitChar
      return $ readInteger readDec $ first : other

readInteger :: ReadS Integer -> String -> Integer
readInteger reader s = fst $ head $ reader s

binaryL :: Text -> (Expr -> Expr -> Operations) -> Operator Parser Expr
binaryL name fun = InfixL $ (\e' e'' -> EOperations $ fun e' e'') <$ symbol name

binaryR :: Text -> (Expr -> Expr -> Operations) -> Operator Parser Expr
binaryR name fun = InfixR $ (\e' e'' -> EOperations $ fun e' e'') <$ symbol name

booleanOp :: Text -> (Expr -> Expr -> BooleanOp) -> Operator Parser Expr
booleanOp name fun = binaryL name (\e' e'' -> BooleanOp $ fun e' e'')

bitwiseOp :: Text -> (Expr -> Expr -> BitwiseOp) -> Operator Parser Expr
bitwiseOp name fun = binaryL name (\e' e'' -> BitwiseOp $ fun e' e'')

comparisonOp :: Text -> (Expr -> Expr -> ComparisonOp) -> Operator Parser Expr
comparisonOp name fun = binaryL name (\e' e'' -> ComparisonOp $ fun e' e'')

arithmeticOp :: Text -> (Expr -> Expr -> ArithmeticOp) -> Operator Parser Expr
arithmeticOp name fun = binaryL name (\e' e'' -> ArithmeticOp $ fun e' e'')

prefix :: Text -> (Expr -> UnaryOp) -> Operator Parser Expr
prefix name fun = Prefix $ EOperations . UnaryOp . fun <$ symbol name

statementP :: Parser Statement
statementP =
  choice $
    try
      <$> [ SExpr <$> exprP,
            SVarDecl <$> varP,
            SRecFunDecl <$> recFunP,
            SFunDecl <$> funP,
            SBlock <$> blockP
          ]

itemP :: Parser Statement
itemP = statementP

-- let f =
--   let x=1     // offside line is at column 3
--   let y=1     // this line must start at column 3
--   x+y         // this line must start at column 3

blockP :: Parser [Statement]
blockP = L.indentBlock scn p
  where
    p = do return (L.IndentMany Nothing return itemP)

-- blockP :: Parser (Statement, [(Statement, [Statement])])
-- blockP = L.nonIndented scn (L.indentBlock scn p)
--   where
--     p = do
--       header <- itemP
--       return (L.IndentSome Nothing (return . (header, )) blockItemP)
