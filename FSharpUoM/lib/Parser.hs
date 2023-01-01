{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Parser where

import Ast
import Control.Applicative (empty)
import Control.Monad (void)
import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Data.Text (Text, concat, pack)
import Data.Void (Void)
import Lexer (choice', kIn, keywordP, lexeme, parens, mlparens, sc, scn, symbol)
import Numeric (readDec)
import Text.Megaparsec (MonadParsec (..), Parsec, choice, eitherP, many, noneOf, oneOf, optional, parseMaybe, sepBy, sepBy1, some, (<|>))
import Text.Megaparsec.Char (alphaNumChar, char, digitChar, letterChar, newline, space1, string)
import qualified Text.Megaparsec.Char.Lexer as L

-- import Data.Scientific (toRealFloat)

type Parser = Parsec Void Text

-- <------------>
-- | MainSection|
-- <------------>

-- Parser entry point
parse :: Text -> Maybe Program
parse = parseMaybe $ scn *> programP <* eof

programP :: Parser Program
programP = Program <$> many (L.nonIndented scn statementP <* scn)

statementP :: Parser Statement
statementP =
  choice'
    [ SMeasureDecl <$> measureP,
      SExpr <$> exprP,
      SVarDecl <$> varP,
      SRecFunDecl <$> recFunP,
      SFunDecl <$> funP
    ]

-- <------------>
-- | MainSection|
-- <------------>

-- <------------------->
-- | DeclarationSection| -- TODO ошибка если сначала VarDecl, а после LetIn
-- <------------------->
varP :: Parser VarDecl
varP = VarDecl <$ symbol "let" <*> typedIdentifierP <* symbol "=" <*> exprP <* try (notFollowedBy kIn)

funP :: Parser FunDecl
funP =
  choice'
    [ FunDecl <$ symbol "let" <*> identifierP <*> some typedIdentifierP <* symbol "=" <*> ((: []) <$> (SExpr <$> exprP)),
      let headerP = (,) <$ symbol "let" <*> identifierP <*> some typedIdentifierP <* symbol "="
       in blockSomeP headerP statementP (\((a, b), c) -> FunDecl a b c)
    ]

recFunP :: Parser RecFunDecl
recFunP =
  choice'
    [ RecFunDecl <$ symbol "let" <* symbol "rec" <*> identifierP <*> some typedIdentifierP <* symbol "=" <*> ((: []) <$> (SExpr <$> exprP)),
      let headerP = (,) <$ symbol "let" <* symbol "rec" <*> identifierP <*> some typedIdentifierP <* symbol "="
       in blockSomeP headerP statementP (\((a, b), c) -> RecFunDecl a b c)
    ]

measureP :: Parser MeasureDecl
measureP = MeasureDecl <$ (symbol "[<Measure>]" <* scn <* symbol "type") <*> (TMeasure <$> mExprP) <*> (optional . try) (symbol "=" *> mExprP)

-- <------------------->
-- | DeclarationSection|
-- <------------------->

-- <------------------->
-- | IndentationSection|
-- <------------------->
blockSomeP :: Parser a -> Parser b -> ((a, [b]) -> c) -> Parser c
blockSomeP headerP itemP mk = L.indentBlock scn p
  where
    p = do
      header <- headerP
      return (L.IndentSome Nothing (return . mk . (header,)) itemP)

blockManyP :: Parser a -> Parser b -> ((a, [b]) -> c) -> Parser c
blockManyP headerP itemP mk = L.indentBlock scn p
  where
    p = do
      header <- headerP
      return (L.IndentMany Nothing (return . mk . (header,)) itemP)

ifElseBlockSomeP :: Parser b -> Parser [b]
ifElseBlockSomeP itemP = L.indentBlock scn p
  where
    p = return (L.IndentSome Nothing return itemP)

-- <------------------->
-- | IndentationSection|
-- <------------------->

-- <------------------>
-- | ExpressionSection|
-- <------------------>

-- MainExprParser
exprP :: Parser Expr
exprP = makeExprParser exprTerm opsTable

exprTerm :: Parser Expr
exprTerm =
  choice'
    [ parens exprP,
      ENull <$ symbol "null",
      letInP,
      EValue <$> valueP,
      lambdaP,
      ifElseP,
      -- EApplication <$> identifierP <*> some exprP,
      EIdentifier <$> identifierP
    ]

-- SingleExprParsers

lambdaP :: Parser Expr
lambdaP =
  let headerP = symbol "fun" *> many typedIdentifierP <* symbol "->"
   in choice'
        [ EFun <$> headerP <*> ((: []) <$> (SExpr <$> exprP)),
          blockSomeP headerP statementP (uncurry EFun)
        ]

letInP :: Parser Expr
letInP =
  choice'
    [ ELetIn <$ symbol "let" <*> identifierP <* symbol "=" <*> exprP <* symbol "in" <*> ((: []) <$> exprP),
      let headerP = (,) <$ symbol "let" <*> identifierP <* symbol "=" <*> exprP <* symbol "in"
       in blockSomeP headerP exprP (\((a, b), c) -> ELetIn a b c)
    ]

ifElseP :: Parser Expr
ifElseP =
  let headerP = symbol "if" *> exprP <* symbol "then"
   in choice'
        [ EIf <$> headerP <*> ((: []) <$> (SExpr <$> exprP)) <* symbol "else" <*> ((: []) <$> (SExpr <$> exprP)),
          EIf <$> headerP <*> ((: []) <$> (SExpr <$> exprP)) <* symbol "else" <*> ifElseBlockSomeP statementP,
          EIf <$> headerP <*> ifElseBlockSomeP statementP <* symbol "else" <*> ((: []) <$> (SExpr <$> exprP)),
          EIf <$> headerP <*> ifElseBlockSomeP statementP <* symbol "else" <*> ifElseBlockSomeP statementP
        ]

-- OperationsExprTable

opsTable :: [[Operator Parser Expr]]
opsTable =
  [ [applicationOp],
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

applicationOp :: Operator Parser Expr
applicationOp = InfixL $ return $ \a b -> EApplication a [b]


-- MeasureExprParser

mExprP :: Parser Expr
mExprP = makeExprParser mExprTerm mOpsTable

mOpsTable :: [[Operator Parser Expr]]
mOpsTable =
  [
    [arithmeticOp "^" ExpMeasureOp],
    [arithmeticOp "" MulOp, arithmeticOp "*" MulOp, arithmeticOp "/" DivOp]
    ]

mExprTerm :: Parser Expr
mExprTerm =
  choice'
    [ parens mExprP,
      EValue <$> valueP, -- TODO запретить возможность объявлять все кроме int без UoM
      EIdentifier <$> identifierP
    ]
  
-- <------------------>
-- | ExpressionSection|
-- <------------------>

-- <-------------------->
-- | OtherParsersSection|
-- <-------------------->

-- IdentifiersParsers

identifierP :: Parser Identifier
identifierP =
  lexeme $
    Identifier
      <$> ( notFollowedBy keywordP *> do
              first <- letterP
              other <- many $ letterP <|> digitChar
              return $ pack $ first : other
          )
  where
    letterP = letterChar <|> char '_'

typedIdentifierP :: Parser (Identifier, Maybe Type)
typedIdentifierP = do
  choice'
    [ parens ((,) <$> identifierP <*> (optional . try) (symbol ":" *> typeP)),
      (,) <$> identifierP <*> pure Nothing
    ]

-- MeasureTypeParser

helpMeasureP :: Parser (Maybe Expr)
helpMeasureP = (optional . try) (mlparens mExprP)

-- TypeParser

typeP :: Parser Type
typeP =
  choice'
    [ parens typeP,
      TBool <$ symbol "bool",
      TInt <$ symbol "int" <*> helpMeasureP,
      TDouble <$ (symbol "float" <|> symbol "double") <*> helpMeasureP,
      TFun <$ sepBy1 typeP (symbol "->")
    ]

-- ValueParsers

valueP :: Parser Value
valueP =
  choice'
    [ VDouble <$> signedDoubleP <*> helpMeasureP,
      VInt <$> signedIntP <*> helpMeasureP,
      VBool <$> boolLitP -- add fun mb
    ]

boolLitP :: Parser Bool
boolLitP = True <$ symbol "true" <|> False <$ symbol "false"

decimalIntP :: Parser Integer
decimalIntP = lexeme L.decimal

signedIntP :: Parser Integer
signedIntP = L.signed sc decimalIntP

doubleP :: Parser Double
doubleP = lexeme L.float

signedDoubleP :: Parser Double
signedDoubleP = L.signed sc doubleP

-- IntegerReader

readInteger :: ReadS Integer -> String -> Integer
readInteger reader s = fst $ head $ reader s

-- <-------------------->
-- | OtherParsersSection|
-- <-------------------->