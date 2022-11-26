{-# LANGUAGE OverloadedStrings #-}

module Parser.Parser (parse) where

import Control.Monad (void)
import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Data.Either (lefts, rights)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Parser.Ast as Ast
import Parser.Lexer
import Text.Megaparsec (MonadParsec (..), choice, eitherP, many, optional, parseMaybe)

---------------------------------------------------------Parser---------------------------------------------------------

-- | Parser entry point
parse :: Text -> Maybe Ast.Program
parse = parseMaybe $ sc *> programP <* eof

--------------------------------------------------------Program---------------------------------------------------------

-- | Program parser.
programP :: Parser Ast.Program
programP = do
  decls <- many topLevelDeclP
  return Ast.Program {Ast.topLevelVarDecls = lefts decls, Ast.topLevelFunctionDefs = rights decls}

-- | Top level declaration parser, it parses either var declaration or function definition.
topLevelDeclP :: Parser (Either Ast.VarDecl Ast.FunctionDef)
topLevelDeclP = eitherP' (varDeclP <* semicolon) functionDefP

-- | Function definition parser.
functionDefP :: Parser Ast.FunctionDef
functionDefP = Ast.FunctionDef <$ kwFunc <*> identifierP <*> functionLitWithoutKwP

------------------------------------------------------Expressions-------------------------------------------------------

-- | Expression parser.
expressionP :: Parser Ast.Expression
expressionP = makeExprParser termExpressionP opsTable

-- | Terminal expression parser, its terminal in terms of `makeExprParser` parser.
termExpressionP :: Parser Ast.Expression
termExpressionP = choice' [parens expressionP, Ast.ExprValue <$> valueP, Ast.ExprIdentifier <$> identifierP]

-- | Operators table, contains all operator parsers and their fixity.
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
      mulOp' "&" ["&&"] Ast.BitAndOp
    ],
    [ addOp "+" Ast.PlusOp,
      addOp "-" Ast.MinusOp,
      addOp' "|" ["||"] Ast.BitOrOp,
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

-- Associativity and arity types

-- | Utility function, that takes operator symbol, operator function and gives new binary operator in return.
binary :: Text -> (Ast.Expression -> Ast.Expression -> Ast.Expression) -> Operator Parser Ast.Expression
binary opSym fun = InfixL $ fun <$ symbol opSym

-- TODO : Docs
binary' :: Text -> [Text] -> (Ast.Expression -> Ast.Expression -> Ast.Expression) -> Operator Parser Ast.Expression
binary' opSym notOpsSyms fun = InfixL $ fun <$ (notFollowedBy (choice' $ symbol <$> notOpsSyms) *> symbol opSym)

-- | Utility function, that takes operator symbol, operator function and gives new prefix operator in return.
prefix :: Text -> (Ast.Expression -> Ast.Expression) -> Operator Parser Ast.Expression
prefix opSym fun = Prefix $ fun <$ symbol opSym

-- | Utility function, that takes operator symbol, operator function and gives new postfix operator in return.
postfix :: Parser (Ast.Expression -> Ast.Expression) -> Operator Parser Ast.Expression
postfix = Postfix

-- Operators types

-- TODO : Docs
andOrOp :: Text -> Ast.BinaryOp -> Operator Parser Ast.Expression
andOrOp opSym op = binary opSym $ Ast.ExprBinaryOp op

-- TODO : Docs
relOp :: Text -> Ast.RelOp -> Operator Parser Ast.Expression
relOp opSym op = binary opSym $ Ast.ExprBinaryOp (Ast.RelOp op)

-- TODO : Docs
addOp :: Text -> Ast.AddOp -> Operator Parser Ast.Expression
addOp opSym op = binary opSym $ Ast.ExprBinaryOp (Ast.AddOp op)

-- TODO : Docs
addOp' :: Text -> [Text] -> Ast.AddOp -> Operator Parser Ast.Expression
addOp' opSym notOpsSyms op = binary' opSym notOpsSyms $ Ast.ExprBinaryOp (Ast.AddOp op)

-- TODO : Docs
mulOp :: Text -> Ast.MulOp -> Operator Parser Ast.Expression
mulOp opSym op = binary opSym $ Ast.ExprBinaryOp (Ast.MulOp op)

-- TODO : Docs
mulOp' :: Text -> [Text] -> Ast.MulOp -> Operator Parser Ast.Expression
mulOp' opSym notOpsSyms op = binary' opSym notOpsSyms $ Ast.ExprBinaryOp (Ast.MulOp op)

-- TODO : Docs
unaryOp :: Text -> Ast.UnaryOp -> Operator Parser Ast.Expression
unaryOp opSym op = prefix opSym $ Ast.ExprUnaryOp op

-- TODO : Docs
funcCallOp :: Operator Parser Ast.Expression
funcCallOp = postfix $ flip Ast.ExprFuncCall <$> listed expressionP comma

-- TODO : Docs
arrayAccessByIndexOp :: Operator Parser Ast.Expression
arrayAccessByIndexOp = postfix $ flip Ast.ExprArrayAccessByIndex <$> brackets expressionP

---------------------------------------------------------Types----------------------------------------------------------

-- | Type parser.
typeP :: Parser Ast.Type
typeP =
  choice'
    [ Ast.TInt <$ idInt,
      Ast.TBool <$ idBool,
      Ast.TString <$ idString,
      functionTypeP,
      parens typeP
    ]

-- TODO : arrayTypeP,

-- arrayTypeP :: Parser Ast.ArrayType
-- arrayTypeP = do
--   lenExpr <- brackets expressionP
--   len <- do
--     case simplifyConstExpr lenExpr of
--       Just (Ast.LitInt len) -> return len
--       _ -> fail "this is not a const int expression"
--   t <- typeP
--   return Ast.ArrayType {Ast.elementType = t, Ast.length = len}

-- | Function type (e.g., `func (int, string) bool`) parser.
functionTypeP :: Parser Ast.Type
functionTypeP = do
  void kwFunc
  params <- listed typeP comma
  result <- optional' typeP
  return $ Ast.TFunction $ Ast.FunctionType params result

-------------------------------------------------------Statements-------------------------------------------------------

-- | Statement parser.
statementP :: Parser Ast.Statement
statementP =
  choice'
    [ stmtReturnP,
      stmtBreakP,
      stmtContinueP,
      stmtForP,
      Ast.StmtVarDecl <$> varDeclP,
      Ast.StmtIfElse <$> ifElseP,
      Ast.StmtBlock <$> blockP,
      Ast.StmtSimple <$> simpleStmtP
    ]

-- | Return statement parser.
stmtReturnP :: Parser Ast.Statement
stmtReturnP = Ast.StmtReturn <$ kwReturn <*> optional' expressionP

-- | Break statement parser.
stmtBreakP :: Parser Ast.Statement
stmtBreakP = Ast.StmtBreak <$ kwBreak

-- | Continue statement parser.
stmtContinueP :: Parser Ast.Statement
stmtContinueP = Ast.StmtContinue <$ kwContinue

-- TODO : Docs
stmtForP :: Parser Ast.Statement
stmtForP = Ast.StmtFor <$> (Ast.For <$ void kwFor <*> forKindP <*> blockP)

-- TODO : Docs
forKindP :: Parser Ast.ForKind
forKindP =
  choice'
    [ Ast.ForKindFor
        <$> optional' simpleStmtP
        <* semicolon
        <*> optional' expressionP
        <* semicolon
        <*> optional' simpleStmtP,
      Ast.ForKindWhile <$> expressionP,
      return Ast.ForKindLoop
    ]

-- | Var declaration parser.
varDeclP :: Parser Ast.VarDecl
varDeclP = Ast.VarDecl <$ kwVar <*> choice' [listed1 varSpecP semicolon, (: []) <$> varSpecP]

-- | Var specification parser.
varSpecP :: Parser Ast.VarSpec
varSpecP =
  choice'
    [ Ast.VarSpec <$> identifierP <*> optional' typeP <* symbol "=" <*> expressionP,
      Ast.DefaultedVarSpec <$> identifierP <*> typeP
    ]

-- If-else parser.
ifElseP :: Parser Ast.IfElse
ifElseP = do
  void kwIf
  stmt <- optional' $ simpleStmtP <* semicolon
  condition <- expressionP
  block <- blockP
  elseStmt <- optional' $ kwElse *> eitherP' ifElseP blockP
  return $ Ast.IfElse stmt condition block elseStmt

-- | Block parser.
blockP :: Parser [Ast.Statement]
blockP = braces $ catMaybes <$> many (optional' statementP <* semicolon)

-- | Simple statement parser.
simpleStmtP :: Parser Ast.SimpleStmt
simpleStmtP = choice' [stmtAssignmentP, stmtIncP, stmtDecP, stmtShortVarDeclP, stmtExpressionP]

-- TODO : Add support for `Ast.UpdArrEl`

-- | Assignment statement parser.
stmtAssignmentP :: Parser Ast.SimpleStmt
stmtAssignmentP = Ast.StmtAssignment . Ast.UpdVar <$> identifierP <* symbol "=" <*> expressionP

-- | Increment statement parser.
stmtIncP :: Parser Ast.SimpleStmt
stmtIncP = Ast.StmtInc . Ast.UpdVar <$> identifierP <* symbol "++"

-- | Decrement statement parser.
stmtDecP :: Parser Ast.SimpleStmt
stmtDecP = Ast.StmtDec . Ast.UpdVar <$> identifierP <* symbol "--"

-- | Short var declaration statement parser.
stmtShortVarDeclP :: Parser Ast.SimpleStmt
stmtShortVarDeclP = Ast.StmtShortVarDecl <$> identifierP <* symbol ":=" <*> expressionP

-- | Expression statement parser.
stmtExpressionP :: Parser Ast.SimpleStmt
stmtExpressionP = Ast.StmtExpression <$> expressionP

---------------------------------------------------------Values---------------------------------------------------------

-- | Value parser.
valueP :: Parser Ast.Value
valueP =
  choice'
    [ Ast.ValInt <$> intLitP,
      Ast.ValBool <$> boolLitP,
      Ast.ValString <$> stringLitP,
      Ast.ValFunction <$> functionLitP
    ]

-- TODO : arrayLitP

-- arrayLitP :: Parser Ast.Literal
-- arrayLitP = do
--   t <- arrayTypeP
--   check arrayLitValue for size <= t.len
--   value <- arrayLitValueP
--   return $ Ast.LitArray Ast.ArrayLiteral {t = t, value = value}

-- arrayLitValueP :: Parser [Ast.Element]
-- arrayLitValueP = todo $ unpack "array literal value parser"

-- ArrayLiteral      = { ArrayType ~ ArrayLiteralValue }
-- ArrayLiteralValue = { "{" ~ ( KeyedElementList ~ ","? )? ~ "}" }
-- KeyedElementList  = { KeyedElement ~ ( "," ~ KeyedElement )* }
-- KeyedElement      = { ( Key ~ ":" )? ~ Element }
-- Key               = { Expression }
-- Element           = { Expression | ArrayLiteralValue }

-- | Function literal (can also be nil) parser.
functionLitP :: Parser Ast.FunctionValue
functionLitP = choice' [Ast.Nil <$ idNil, kwFunc *> functionLitWithoutKwP]

-- | Function literal without `func` keyword (e.g., `(x int, y string) bool { return; }`) parser.
functionLitWithoutKwP :: Parser Ast.FunctionValue
functionLitWithoutKwP = Ast.Function <$> functionSignatureP <*> blockP

-- | Function signature (e.g., `(x int, y string) bool`) parser.
functionSignatureP :: Parser Ast.FunctionSignature
functionSignatureP = do
  params <- listed ((,) <$> identifierP <*> typeP) comma
  result <- optional' typeP
  return $ Ast.FunctionSignature params result

---------------------------------------------------------Utils----------------------------------------------------------

-- | Choice between elements parser with built-in backtracking support.
choice' :: (Foldable f, MonadParsec e s m, Functor f) => f (m a) -> m a
choice' ps = choice $ try <$> ps

-- | Combine two alternatives with built-in backtracking support.
eitherP' :: MonadParsec e s m => m a -> m b -> m (Either a b)
eitherP' leftP = eitherP $ try leftP

-- | Optional element parser with built-in backtracking support.
optional' :: (MonadParsec e s m) => m a -> m (Maybe a)
optional' = optional . try
