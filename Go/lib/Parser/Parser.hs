{-# LANGUAGE OverloadedStrings #-}

-- | Provides parser that produces [AST]["Parser.Ast"].
module Parser.Parser where

import Control.Monad (void)
import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Data.Either (lefts, rights)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Parser.Ast as Ast
import Parser.Lexer
import Text.Megaparsec (MonadParsec (..), choice, eitherP, many, optional, parseMaybe, sepEndBy, some)

---------------------------------------------------------Parser---------------------------------------------------------

-- * Parser

-- | Parser entry point
parse :: Text -> Maybe Ast.Program
parse = parseMaybe $ sc *> programP <* eof

--------------------------------------------------------Program---------------------------------------------------------

-- * Program (top-level) parsers

-- | Program parser.
programP :: Parser Ast.Program
programP = do
  decls <- many topLevelDeclP
  return Ast.Program {Ast.topLevelVarDecls = lefts decls, Ast.topLevelFunctionDefs = rights decls}

-- | Top-level declaration parser, it parses either var declaration or function definition.
topLevelDeclP :: Parser (Either Ast.VarDecl Ast.FunctionDef)
topLevelDeclP = eitherP' (varDeclP <* semicolon) functionDefP

-- | Function definition parser.
functionDefP :: Parser Ast.FunctionDef
functionDefP = Ast.FunctionDef <$ kwFunc <*> identifierP <*> anonFuncWithoutKwP

------------------------------------------------------Expressions-------------------------------------------------------

-- * Expressions parsers

-- | Expression parser.
expressionP :: Parser Ast.Expression
expressionP = makeExprParser termExpressionP opsTable

-- | Terminal expression parser, it's terminal in terms of 'makeExprParser' parser.
termExpressionP :: Parser Ast.Expression
termExpressionP = choice' [parens expressionP, Ast.ExprValue <$> valueP, Ast.ExprIdentifier <$> identifierP]

-- | Operators table, contains all operator parsers and their fixity.
opsTable :: [[Operator Parser Ast.Expression]]
opsTable =
  [ [ arrayAccessByIndexOp,
      funcCallOp
    ],
    [ unaryOp "+" Ast.UnaryPlusOp,
      unaryOp "-" Ast.UnaryMinusOp,
      unaryOp "!" Ast.NotOp,
      unaryOp "^" Ast.BitwiseComplementOp
    ],
    [ binaryOp "*" Ast.MultOp,
      binaryOp "/" Ast.DivOp,
      binaryOp "%" Ast.ModOp,
      binaryOp "<<" Ast.BitShiftLeftOp,
      binaryOp ">>" Ast.BitShiftRightOp,
      binaryOp "&^" Ast.BitClearOp,
      binaryOp' "&" ["&&"] Ast.BitAndOp
    ],
    [ binaryOp "+" Ast.PlusOp,
      binaryOp "-" Ast.MinusOp,
      binaryOp' "|" ["||"] Ast.BitOrOp,
      binaryOp "^" Ast.BitXorOp
    ],
    [ binaryOp "==" Ast.EqOp,
      binaryOp "!=" Ast.NeOp,
      binaryOp "<=" Ast.LeOp,
      binaryOp "<" Ast.LtOp,
      binaryOp ">=" Ast.MeOp,
      binaryOp ">" Ast.MtOp
    ],
    [binaryOp "&&" Ast.AndOp],
    [binaryOp "||" Ast.OrOp]
  ]

-- ** Operators

-- | Utility function, that takes operator symbol, binary operator constructor and gives new binary operator in return.
binaryOp :: Text -> Ast.BinaryOp -> Operator Parser Ast.Expression
binaryOp opSym op = InfixL $ Ast.ExprBinaryOp op <$ symbol opSym

-- | Utility function, that takes operator symbol, ignored operator symbols (for parser), binary operator constructor and gives new binary operator in return.
binaryOp' :: Text -> [Text] -> Ast.BinaryOp -> Operator Parser Ast.Expression
binaryOp' opSym notOpsSyms op = InfixL $ Ast.ExprBinaryOp op <$ (notFollowedBy (choice' $ symbol <$> notOpsSyms) *> symbol opSym)

-- | Utility function, that takes operator symbol, unary operator constructor and gives new unary operator in return.
unaryOp :: Text -> Ast.UnaryOp -> Operator Parser Ast.Expression
unaryOp opSym op = Prefix $ Ast.ExprUnaryOp op <$ symbol opSym

-- | Function call operator.
funcCallOp :: Operator Parser Ast.Expression
funcCallOp = Postfix $ flip Ast.ExprFuncCall <$> listed expressionP comma

-- | Array access by index operator.
arrayAccessByIndexOp :: Operator Parser Ast.Expression
arrayAccessByIndexOp = Postfix $ flip Ast.ExprArrayAccessByIndex <$> brackets expressionP

---------------------------------------------------------Types----------------------------------------------------------

-- * Types parsers

-- | Type parser.
typeP :: Parser Ast.Type
typeP =
  choice'
    [ Ast.TInt <$ idInt,
      Ast.TBool <$ idBool,
      Ast.TString <$ idString,
      Ast.TArray <$> arrayTypeP,
      Ast.TFunction <$> functionTypeP,
      parens typeP
    ]

-- | Array type parser.
arrayTypeP :: Parser Ast.ArrayType
arrayTypeP = Ast.ArrayType <$> brackets expressionP <*> typeP

-- | Function type parser.
functionTypeP :: Parser Ast.FunctionType
functionTypeP = Ast.FunctionType <$ kwFunc <*> listed typeP comma <*> optional' typeP

-------------------------------------------------------Statements-------------------------------------------------------

-- * Statements parsers

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

-- | For statement parser.
stmtForP :: Parser Ast.Statement
stmtForP = Ast.StmtFor <$> (Ast.For <$ void kwFor <*> forKindP <*> blockP)

-- | For kind parser.
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

-- | If-else parser.
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

-- | Assignment statement parser.
stmtAssignmentP :: Parser Ast.SimpleStmt
stmtAssignmentP = Ast.StmtAssignment <$> updElP <* symbol "=" <*> expressionP

-- | Increment statement parser.
stmtIncP :: Parser Ast.SimpleStmt
stmtIncP = Ast.StmtInc <$> updElP <* symbol "++"

-- | Decrement statement parser.
stmtDecP :: Parser Ast.SimpleStmt
stmtDecP = Ast.StmtDec <$> updElP <* symbol "--"

-- | Short var declaration statement parser.
stmtShortVarDeclP :: Parser Ast.SimpleStmt
stmtShortVarDeclP = Ast.StmtShortVarDecl <$> identifierP <* symbol ":=" <*> expressionP

-- | Expression statement parser.
stmtExpressionP :: Parser Ast.SimpleStmt
stmtExpressionP = Ast.StmtExpression <$> expressionP

-- | Updatable element parser.
updElP :: Parser Ast.UpdatableElement
updElP =
  choice'
    [ Ast.UpdArrEl <$> identifierP <*> some (brackets expressionP),
      Ast.UpdVar <$> identifierP
    ]

---------------------------------------------------------Values---------------------------------------------------------

-- * Values parsers

-- | Value parser.
valueP :: Parser Ast.Value
valueP =
  choice'
    [ Ast.ValInt <$> intLitP,
      Ast.ValBool <$> boolLitP,
      Ast.ValString <$> stringLitP,
      Ast.ValArray <$> arrayValP,
      Ast.ValFunction <$> functionValP
    ]

-- | Array value parser.
arrayValP :: Parser Ast.ArrayValue
arrayValP = Ast.ArrayValue <$> arrayTypeP <*> arrayElementsP

-- | Array's elements parser.
arrayElementsP :: Parser [Ast.KeyedElement]
arrayElementsP = braces $ sepEndBy keyedElementP comma

-- | Array's optionally keyed element parser.
keyedElementP :: Parser Ast.KeyedElement
keyedElementP = Ast.KeyedElement <$> optional' (expressionP <* colon) <*> elementP

-- | Array's element parser.
elementP :: Parser Ast.Element
elementP = choice' [Ast.ElementList <$> arrayElementsP, Ast.Element <$> expressionP]

-- | Function value (can also be @nil@) parser.
functionValP :: Parser Ast.FunctionValue
functionValP = choice' [Ast.Nil <$ idNil, kwFunc *> anonFuncWithoutKwP]

-- | Anonymous function without @func@ keyword parser.
anonFuncWithoutKwP :: Parser Ast.FunctionValue
anonFuncWithoutKwP = Ast.AnonymousFunction <$> functionSignatureP <*> blockP

-- | Function signature parser.
functionSignatureP :: Parser Ast.FunctionSignature
functionSignatureP = do
  params <- listed ((,) <$> identifierP <*> typeP) comma
  result <- optional' typeP
  return $ Ast.FunctionSignature params result

---------------------------------------------------------Utils----------------------------------------------------------

-- * Utils

-- | Choice between elements parser with built-in backtracking support.
choice' :: (Foldable f, MonadParsec e s m, Functor f) => f (m a) -> m a
choice' ps = choice $ try <$> ps

-- | Combine two alternatives with built-in backtracking support.
eitherP' :: MonadParsec e s m => m a -> m b -> m (Either a b)
eitherP' leftP = eitherP $ try leftP

-- | Optional element parser with built-in backtracking support.
optional' :: (MonadParsec e s m) => m a -> m (Maybe a)
optional' = optional . try
