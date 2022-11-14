{-# LANGUAGE DuplicateRecordFields #-}

module Ast where

import Data.Text (Text)

-- TODO : Add VOID support !!!

-- Program

data Program = Program {topLevelVarDecls :: [VarDecl], topLevelFunctionDefs :: [FunctionDef]}
  deriving (Show)

-- Expression

data Expression
  = ExprLiteral Literal
  | ExprIdentifier Identifier
  | ExprUnaryOp UnaryOp Expression
  | ExprBinaryOp BinaryOp Expression Expression
  | ExprFuncApplication {fn :: Expression, args :: [Expression]}
  | ExprArrayIndexAccess {arr :: Expression, index :: Int}
  deriving (Show)

-- Operators

data BinaryOp
  = -- | a || b
    OrOp
  | -- | a && b
    AndOp
  | -- | TODO
    RelOp RelOp
  | -- | TODO
    AddOp AddOp
  | -- | TODO
    MulOp MulOp
  deriving (Show)

-- | Relation operators, works only on boolean types.
data RelOp
  = -- | a == b
    EqOp
  | -- | a != b
    NeOp
  | -- | a < b
    LtOp
  | -- | a <= b
    LeOp
  | -- | a > b
    MtOp
  | -- | a >= b
    MeOp
  deriving (Show)

data AddOp
  = -- | a + b
    PlusOp
  | -- | a - b
    MinusOp
  | -- | a | b
    BitOrOp
  | -- | a ^ b
    BitXorOp
  deriving (Show)

data MulOp
  = -- | a * b
    MultOp
  | -- | a / b
    DivOp
  | -- | a % b
    ModOp
  | -- | a << b
    BitShiftLeftOp
  | -- | a >> b
    BitShiftRightOp
  | -- | a &^ b
    BitClearOp
  | -- | a & b
    BitAndOp
  deriving (Show)

data UnaryOp
  = -- | +a
    UnaryPlusOp
  | -- | -a
    UnaryMinusOp
  | -- | !a
    NotOp
  | -- | ^a
    BitwiseComplementOp
  deriving (Show)

-- Type

data Type
  = -- | Integer type: Go's equvilent of 64-bit`int` type.
    TInt
  | -- | Boolean type: Go's equvilent of `bool` type.
    TBool
  | -- | String type: Go's equvilent of `string` type.
    TString
  | -- TODO
    TArray ArrayType
  | -- TODO
    TFunction {parameters :: [Type], result :: [Type]}
  deriving (Show)

-- Function definition

data FunctionDef = FunctionDef {name :: Identifier, signature :: FuncSignature, body :: [Stmt]}
  deriving (Show)

data FuncSignature = FuncSignature {parameters :: [(Identifier, Type)], result :: [Type]}
  deriving (Show)

-- Statements

data Stmt
  = -- | TODO
    StmtReturn (Maybe [Expression])
  | -- | TODO
    StmtBreak
  | -- | TODO
    StmtContinue
  | -- | TODO
    StmtFor For
  | -- | TODO
    StmtVarDecl VarDecl
  | -- | TODO
    StmtIfElse IfElse
  | -- | { ... }
    StmtBlock [Stmt]
  | -- | TODO
    StmtSimple SimpleStmt
  deriving (Show)

data For
  = -- | TODO
    For
      { preStament :: Maybe SimpleStmt,
        forCondition :: Maybe Expression,
        postStament :: Maybe SimpleStmt,
        block :: [Stmt]
      }
  | -- | TODO
    While {whileCondition :: Expression, block :: [Stmt]}
  | -- | TODO
    Loop {block :: [Stmt]}
  deriving (Show)

newtype VarDecl = VarDecl [VarSpec]
  deriving (Show)

data VarSpec = VarSpec {identifiers :: [Identifier], t :: Maybe Type, expressions :: [Expression]}
  deriving (Show)

data IfElse = IfElse
  { simpleStmt :: Maybe SimpleStmt,
    condition :: Expression,
    block :: [Stmt],
    elseStmt :: Either IfElse [Stmt]
  }
  deriving (Show)

data SimpleStmt
  = StmtAssignment {lhs :: [AssignmentLhs], rhs :: [Expression]}
  | StmtInc Expression
  | StmtDec Expression
  | StmtShortVarDecl {identifiers :: [Identifier], expressions :: [Expression]}
  | StmtExpression Expression
  deriving (Show)

data AssignmentLhs
  = AssignmentLhsVar Identifier
  | AssignmentLhsArrayElement Identifier [Int]
  | AssignmentLhsBlank
  deriving (Show)

-- Array

data ArrayType = ArrayType {elementType :: Type, length :: Int}
  deriving (Show)

-- Literal

data Literal
  = LitInt Int
  | LitBool Bool
  | LitString Text
  | LitArray {t :: ArrayType, value :: [Element]}
  | LitFunction {signature :: FuncSignature, body :: [Stmt]}
  deriving (Show)

-- | Represents runtime value of the calculated expression
type Value = Literal

data Element = ElementExpr Expression | ElementElements [Element]
  deriving (Show)

-- Identifier

newtype Identifier = Identifier Text
  deriving (Show)
