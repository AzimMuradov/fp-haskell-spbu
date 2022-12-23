{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

-- TODO : Docs

module Analyzer.AnalyzedAst
  ( module Analyzer.AnalyzedAst,
    UnaryOp (..),
    BinaryOp (..),
  )
where

import Data.Text (Text)
import Parser.Ast (BinaryOp (..), UnaryOp (..))

--------------------------------------------------------Program---------------------------------------------------------

data Program = Program
  { main :: FunctionDef,
    topLevelVarDecls :: [VarDecl],
    topLevelFunctionDefs :: [FunctionDef]
  }
  deriving (Show)

data FunctionDef = FunctionDef {funcName :: Identifier, func :: Function}
  deriving (Show)

------------------------------------------------------Expressions-------------------------------------------------------

data Expression
  = ExprValue Value
  | ExprIdentifier Identifier
  | ExprUnaryOp UnaryOp Expression
  | ExprBinaryOp BinaryOp Expression Expression
  | ExprArrayAccessByIndex Expression Expression
  | ExprFuncCall Expression [Expression]
  deriving (Show)

-------------------------------------------------------Statements-------------------------------------------------------

data Statement
  = StmtReturn (Maybe Expression)
  | StmtBreak
  | StmtContinue
  | StmtFor For
  | StmtVarDecl VarDecl
  | StmtIfElse IfElse
  | StmtBlock [Statement]
  | StmtSimple SimpleStmt
  deriving (Show)

data For = For {kind :: ForKind, block :: [Statement]}
  deriving (Show)

data ForKind
  = ForKindFor {preStmt :: Maybe SimpleStmt, condition :: Maybe Expression, postStmt :: Maybe SimpleStmt}
  | ForKindWhile {whileCondition :: Expression}
  | ForKindLoop
  deriving (Show)

data VarDecl = VarDecl {identifier :: Identifier, value :: Expression}
  deriving (Show)

data IfElse = IfElse
  { condition :: Expression,
    block :: [Statement],
    elseStmt :: Maybe (Either IfElse [Statement])
  }
  deriving (Show)

data SimpleStmt
  = StmtAssignment UpdatableElement Expression
  | StmtInc UpdatableElement
  | StmtDec UpdatableElement
  | StmtShortVarDecl Identifier Expression
  | StmtExpression Expression
  deriving (Show)

data UpdatableElement
  = UpdVar Identifier
  | UpdArrEl Identifier [Expression]
  deriving (Show)

---------------------------------------------------------Values---------------------------------------------------------

data Value
  = ValInt Int
  | ValBool Bool
  | ValString Text
  | ValArray [Expression]
  | ValFunction FunctionValue
  deriving (Show)

data FunctionValue
  = AnonymousFunction Function
  | Nil
  deriving (Show)

data Function
  = Function {capturedScope :: [Identifier], parameters :: [Identifier], body :: [Statement]}
  | StdLibFunction Identifier
  deriving (Show)

type Identifier = Text
