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
  | ExprFuncCall Expression [Expression]
  | ExprArrayAccessByIndex Expression Int
  deriving (Show)

-- Operators

-- | Binary operators
data BinaryOp
  = -- | a || b
    OrOp
  | -- | a && b
    AndOp
  | -- | Relation operator
    RelOp RelOp
  | -- | Additive operator
    AddOp AddOp
  | -- | Multiplicative operator
    MulOp MulOp
  deriving (Show)

-- | Relation operators
data RelOp
  = -- | a == b
    EqOp
  | -- | a != b
    NeOp
  | -- | a <= b
    LeOp
  | -- | a < b
    LtOp
  | -- | a >= b
    MeOp
  | -- | a > b
    MtOp
  deriving (Show)

-- | Additive operators
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

-- | Multiplicative operators
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

-- | Unary operators
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
  = -- | Integer type: Go's equivalent of 64-bit `int` type.
    TInt
  | -- | Boolean type: Go's equivalent of `bool` type.
    TBool
  | -- | String type: Go's equivalent of `string` type.
    TString
  | -- TODO
    TArray ArrayType
  | -- TODO
    TFunction {parameters :: [Type], result :: [Type]}
  deriving (Show)

-- Function definition

data FunctionDef = FunctionDef {name :: Identifier, signature :: FunctionSignature, body :: [Statement]}
  deriving (Show)

data FunctionSignature = FunctionSignature {parameters :: [(Identifier, Type)], result :: [Type]}
  deriving (Show)

-- Statements

data Statement
  = -- | TODO
    StmtReturn [Expression]
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
    StmtBlock [Statement]
  | -- | TODO
    StmtSimple SimpleStmt
  deriving (Show)

data For
  = -- | TODO
    For
      { preStatement :: Maybe SimpleStmt,
        forCondition :: Maybe Expression,
        postStatement :: Maybe SimpleStmt,
        block :: [Statement]
      }
  | -- | TODO
    While {whileCondition :: Expression, block :: [Statement]}
  | -- | TODO
    Loop {block :: [Statement]}
  deriving (Show)

newtype VarDecl = VarDecl [VarSpec]
  deriving (Show)

data VarSpec = VarSpec {identifiers :: [Identifier], t :: Maybe Type, expressions :: [Expression]}
  deriving (Show)

data IfElse = IfElse
  { simpleStmt :: Maybe SimpleStmt,
    condition :: Expression,
    block :: [Statement],
    elseStmt :: Either IfElse [Statement]
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
  | LitFunction {signature :: FunctionSignature, body :: [Statement]}
  deriving (Show)

-- | Represents runtime value of the calculated expression
type Value = Literal

data Element = ElementExpr Expression | ElementElements [Element]
  deriving (Show)

-- Identifier

newtype Identifier = Identifier Text
  deriving (Show)
