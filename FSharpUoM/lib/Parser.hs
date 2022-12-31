{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Parser where

import Ast
import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Data.Text (Text, concat, pack)
import Data.Void (Void)
import Control.Monad (void)
import Lexer (lexeme, parens, scn, sc, symbol)
import Numeric (readDec)
-- import Data.Scientific (Scientific, toRealFloat)
import Text.Megaparsec (MonadParsec (..), Parsec, choice, eitherP, many, noneOf, oneOf, optional, sepBy, sepBy1, some, (<|>), parseMaybe)
import Text.Megaparsec.Char (alphaNumChar, char, digitChar, letterChar, string, newline, space1)
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text 


-- | Parser entry point
parse :: Text -> Maybe Program
parse = parseMaybe $ scn *> programP <* eof

programP :: Parser Program
programP = Program <$> many (L.nonIndented scn statementP <* scn)

-- TODO --
-- let f x = 
--           let (p:int) = 2
--           let v = 3
--           let m = 4 in <-- срабатывает varDecl и in не считывается
--            p + x + v + m
-- TODO --
varP :: Parser VarDecl
varP = VarDecl <$ symbol "let" <*> typedIdentifierP <* symbol "=" <*> exprP <* try (notFollowedBy ( (symbol "in")))

-- funP :: Parser FunDecl
-- funP = FunDecl <$ symbol "let" <*> identifierP <*> some typedIdentifierP <* symbol "=" <*> exprP

funP :: Parser FunDecl
funP = choice $
          try
            <$> [
                  FunDecl <$ symbol "let" <*> identifierP <*> some typedIdentifierP <* symbol "=" <*> ((:[]) <$> (SExpr <$> exprP)),
                  let headerP = (,) <$ symbol "let" <*> identifierP <*> some typedIdentifierP <* symbol "=" in 
                    blockSomeP headerP statementP (\((a, b), c) -> FunDecl a b c) 
            ]

-- recFunP :: Parser RecFunDecl
-- recFunP = RecFunDecl <$ symbol "let" <* symbol "rec" <*> identifierP <*> some typedIdentifierP <* symbol "=" <*> exprP

recFunP :: Parser RecFunDecl
recFunP = choice $
          try
            <$> [
                  RecFunDecl <$ symbol "let" <* symbol "rec" <*> identifierP <*> some typedIdentifierP <* symbol "=" <*> ((:[]) <$> (SExpr <$> exprP)),
                  let headerP = (,) <$ symbol "let" <* symbol "rec" <*> identifierP <*> some typedIdentifierP <* symbol "=" in 
                    blockSomeP headerP statementP (\((a, b), c) -> RecFunDecl a b c) 
            ]

statementP :: Parser Statement
statementP =
  choice $
    try
      <$> [ SExpr <$> exprP,
            SRecFunDecl <$> recFunP,
            SFunDecl <$> funP,
            SVarDecl <$> varP
            
            -- SBlock <$> blockP,
          ]

identifierP :: Parser Identifier
identifierP =
  lexeme $
    Identifier <$> (notFollowedBy (choice[symbol "if", symbol "else", symbol "then", symbol "let", symbol "fun", symbol "rec", symbol "in"]) *> do
      first <- letterP
      other <- many $ letterP <|> digitChar
      return $ pack $ first : other)
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
            -- TDouble <$ symbol "double",
            TFun <$ sepBy1 typeP (symbol "->"),
            parens typeP
          ]


-- lambdaP :: Parser Expr
-- lambdaP = choice $
--           try
--             <$> [
--                   EFun <$ symbol "fun" <*> many typedIdentifierP <* symbol "->" <*> ((:[]) <$> (SExpr <$> exprP)),
--                   let headerP = (,) <$ symbol "fun" <*> many typedIdentifierP <* symbol "->" in 
--                     blockSomeP headerP statementP (\((a, b), c) -> EFun a b c) 
--             ]

letInP :: Parser Expr
letInP = choice $
          try
            <$> [
                  ELetIn <$ symbol "let" <*> identifierP <* symbol "=" <*> exprP<* symbol "in" <*> ((:[]) <$> exprP),
                  let headerP = (,) <$ symbol "let" <*> identifierP <* symbol "=" <*> exprP <* symbol "in" in 
                    blockSomeP headerP exprP (\((a, b), c) -> ELetIn a b c) 
            ]

blockSomeP :: Parser a -> Parser b -> ((a, [b]) -> c) -> Parser c
blockSomeP headerP itemP mk = L.indentBlock scn p
  where
    p = do
      header <- headerP
      return (L.IndentSome Nothing (return . mk . (header, )) itemP)

blockManyP :: Parser a -> Parser b -> ((a, [b]) -> c) -> Parser c
blockManyP headerP itemP mk = L.indentBlock scn p
  where
    p = do
      header <- headerP
      return (L.IndentMany Nothing (return . mk . (header, )) itemP)


exprP :: Parser Expr
exprP = makeExprParser exprTerm opsTable

exprTerm :: Parser Expr
exprTerm =
  choice $
    try
      <$> [ parens exprP,
            ENull <$ symbol "null",
            -- ELetIn <$ symbol "let" <*> identifierP <* symbol "=" <*> exprP <* symbol "in" <*> ((:[]) <$> exprP),
            letInP,
            EValue <$> valueP,
            -- lambdaP,
            EIf <$ symbol "if" <*> exprP <* symbol "then" <*> statementP <* symbol "else" <*> statementP,
            -- EApplication <$> identifierP <*> some exprP,
            EIdentifier <$> identifierP
          ]

opsTable :: [[Operator Parser Expr]]
opsTable =
  [ [appl],
    [prefix "-" UnaryMinus], -- TODO некоректно срабатывает в выражениях вида x - 1 и распознает как унарный а не MinusOp
    [arithmeticOp "**" ExpOp],
    [arithmeticOp "*" MulOp, arithmeticOp "/" DivOp, arithmeticOp "%" ModOp],
    [arithmeticOp "+" PlusOp, arithmeticOp "-" MinusOp],
    [ comparisonOp "==" EqOp,
      comparisonOp "<>" NeOp,
      comparisonOp "<" LtOp,
      comparisonOp "<=" LeOp,
      comparisonOp ">" MtOp,
      comparisonOp ">=" MeOp
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
            -- VDouble <$> doubleP
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

comparisonOp :: Text -> (Expr -> Expr -> ComparisonOp) -> Operator Parser Expr
comparisonOp name fun = binaryL name (\e' e'' -> ComparisonOp $ fun e' e'')

arithmeticOp :: Text -> (Expr -> Expr -> ArithmeticOp) -> Operator Parser Expr
arithmeticOp name fun = binaryL name (\e' e'' -> ArithmeticOp $ fun e' e'')

prefix :: Text -> (Expr -> UnaryOp) -> Operator Parser Expr
prefix name fun = Prefix $ EOperations . UnaryOp . fun <$ symbol name

appl :: Operator Parser Expr
appl = InfixL $ return $ \a b -> EApplication a [b]

-- let f =
--   let x=1     // offside line is at column 3
--   let y=1     // this line must start at column 3
--   x+y         // this line must start at column 3

-- itemP :: Parser Statement
-- itemP = statementP

-- blockP :: Parser [Statement]
-- blockP = L.indentBlock scn p
--   where
--     p = do return (L.IndentMany Nothing return itemP)

-- blockP' :: Parser [Statement]
-- blockP' = many $ (char '\n' *> ) *> statementP

-- blockP :: Parser (Statement, [(Statement, [Statement])])
-- blockP = L.nonIndented scn (L.indentBlock scn p)
--   where
--     p = do
--       header <- itemP
--       return (L.IndentSome Nothing (return . (header, )) blockItemP)

-- itemP :: Parser Statement
-- itemP = statementP

-- -- pItem  :: String
-- -- pItem = lexeme $ some (alphaNumChar <|> char '-')

-- pComplexItem :: Parser [Statement]
-- pComplexItem = L.indentBlock scn p
--   where
--     p = do
--       return (L.IndentMany Nothing return statementP)

-- parser :: Parser (String, [(String, [String])])
-- parser = pItemList <* eof

-- pItemList :: Parser (String, [(String, [String])])
-- pItemList = L.nonIndented scn (L.indentBlock scn p)
--   where
--     p = do
--       header <- statementP
--       return (L.IndentSome Nothing (return . (header, )) pComplexItem)