{-# LANGUAGE OverloadedStrings #-}

module Parser.Parser where

import Parser.Ast
import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack)
import Parser.Lexer
import Text.Megaparsec (MonadParsec (..), many, optional, parseMaybe, sepBy1, some, (<|>), sepEndBy1)
import Text.Megaparsec.Char (char, digitChar, letterChar)
import qualified Text.Megaparsec.Char.Lexer as L

-- * MainSection

-- | Parser entry point
parse :: Parser a -> Text -> Maybe a
parse p = parseMaybe $ sc *> p <* eof

-- | Main Parser
fileP :: Parser [Program]
fileP = sepEndBy1 programP (symbol ";;")

programP :: Parser Program
programP = Program <$> sepBy1 statementP (notFollowedBy (symbol ";;") >> semicolon)

-- | Global Statements Parser
statementP :: Parser Statement
statementP =
  choice'
    [ SMeasureDecl <$> measureP,
      SExpr <$> exprP,
      SRecFunDecl <$> recFunP,
      SFunDecl <$> funP,
      SVarDecl <$> varP
    ]

-- ** DeclarationSection

varP :: Parser VarDecl
varP = VarDecl <$ kLet <*> typedIdentifierP <* eq <*> blockP <* try (notFollowedBy kIn)

funP :: Parser FunDecl
funP = FunDecl <$ kLet <*> identifierP <*> (Fun <$> some typedIdentifierP <* eq <*> blockP)

recFunP :: Parser RecFunDecl
recFunP = RecFunDecl <$ kLet <* kRec <*> identifierP <*> (Fun <$> some typedIdentifierP <* eq <*> blockP)

measureP :: Parser MeasureDecl
measureP = MeasureDecl <$ kMeasure <* kType <*> identifierP <*> (optional . try) (eq *> mExprP)

-- * ExpressionSection

-- BlockExprParser

blockP :: Parser [Expr]
blockP = choice' [(:[]) <$> exprP,
                  block (sepBy1 exprP semicolon)]

-- MainExprParser

exprP :: Parser Expr
exprP = makeExprParser exprTerm opsTable

exprTerm :: Parser Expr
exprTerm =
  choice'
    [ parens exprP,
      ELetInV <$ kLet <*> typedIdentifierP <* eq <*> blockP <* kIn <*> blockP,
      ELetInF <$ kLet <*> identifierP <*> (Fun <$> some typedIdentifierP <* eq <*> blockP) <* kIn <*> blockP,
      EValue <$> valueP,
      EIf <$ kIf <*> exprP <* kThen <*> blockP <* kElse <*> blockP,
      EIdentifier <$> identifierP
    ]

-- OperationsExprTable
opsTable :: [[Operator Parser Expr]]
opsTable =
  [ [applicationOp],
    [arithmeticOp "**" ExpOp],
    [arithmeticOp "*" MulOp, arithmeticOp "/" DivOp, arithmeticOp "%" ModOp],
    [arithmeticOp "+" PlusOp, arithmeticOp "-" MinusOp],
    [ comparisonOp "=" EqOp,
      comparisonOp "<>" NeOp,
      comparisonOp "<=" LeOp,
      comparisonOp "<" LtOp,
      comparisonOp ">=" MeOp,
      comparisonOp ">" MtOp
    ],
    [booleanOp "&&" AndOp],
    [booleanOp "||" OrOp],
    [notOp]
  ]

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

notOp :: Operator Parser Expr
notOp = Prefix $ EOperations . NotOp <$ symbol "not"

applicationOp :: Operator Parser Expr
applicationOp = InfixL $ return $ \a b -> EApplication a b

-- MeasureExprParser

mExprP :: Parser MeasureTypeExpr
mExprP = makeExprParser mExprTerm mOpsTable

mExprTerm :: Parser MeasureTypeExpr
mExprTerm = choice' [parens mExprP, MIdentifier <$> identifierP]

mOpsTable :: [[Operator Parser MeasureTypeExpr]]
mOpsTable =
  [ [measureTypeExpOp],
    [measureTypeOp "*" MTypesMul, measureTypeOp "/" MTypesDiv, measureTypeOp "" MTypesMul]
  ]

measureTypeExpOp :: Operator Parser MeasureTypeExpr
measureTypeExpOp = Postfix $ flip MTypesExp <$ symbol "^" <*> signedIntP

measureTypeOp :: Text -> (MeasureTypeExpr -> MeasureTypeExpr -> MeasureTypeExpr) -> Operator Parser MeasureTypeExpr
measureTypeOp name fun = InfixL $ fun <$ symbol name

-- * OtherParsersSection

-- IdentifierParsers

identifierP :: Parser Identifier
identifierP =
  lexeme
    ( notFollowedBy reservedP *> do
        first <- letterP
        other <- many $ letterP <|> digitChar
        return $ pack $ first : other
    )
  where
    letterP = letterChar <|> char '_'

typedIdentifierP :: Parser (Identifier, Maybe Type)
typedIdentifierP = do
  choice'
    [ parens ((,) <$> identifierP <*> (optional . try) (colon *> typeP)),
      (,) <$> identifierP <*> pure Nothing
    ]

-- MeasureTypeParser

helpMeasureP :: Parser (Maybe MeasureTypeExpr)
helpMeasureP = (optional . try) (mlparens mExprP)

-- TypeParser

typeP :: Parser Type
typeP =
  choice'
    [ TFun <$> typeP' <* arrow <*> typeP,
      parens typeP,
      typeP'
    ]

typeP' :: Parser Type
typeP' =
  choice'
    [ parens typeP,
      TBool <$ wBool,
      TInt <$ wInt <*> helpMeasureP,
      TDouble <$ wDouble <*> helpMeasureP
    ]

-- ValueParsers

valueP :: Parser Value
valueP =
  choice'
    [ VDouble <$> signedDoubleP <*> helpMeasureP,
      VInt <$> signedIntP <*> helpMeasureP,
      VBool <$> boolLitP,
      VFun <$> (Fun <$ kFun <*> many typedIdentifierP <* arrow <*> blockP)
    ]

signedDoubleP :: Parser Double
signedDoubleP = lexeme $ do
  sign <- optional (choice' [1 <$ char '+', -1 <$ char '-'])
  num <- L.float
  return $ fromMaybe 1 sign * num

signedIntP :: Parser Integer
signedIntP = lexeme $ do
  sign <- optional (choice' [1 <$ char '+', -1 <$ char '-'])
  num <- L.decimal
  return $ fromMaybe 1 sign * num

boolLitP :: Parser Bool
boolLitP = True <$ wTrue <|> False <$ wFalse
