{-# LANGUAGE DuplicateRecordFields #-}

module Ast where

import Data.Text (Text)

-- TODO : Add NIL support !!!

-- Program

-- | The head of the AST.
data Program = Program
  { -- | Top level variable declarations.
    topLevelVarDecls :: [VarDecl],
    -- | Top level function definitions.
    topLevelFunctionDefs :: [FunctionDef]
  }
  deriving (Show)

-- Expressions

-- | Expression.
data Expression
  = -- | Literal expression (e.g., `"Hello!"`, `17`, `true`).
    ExprLiteral Literal
  | -- | Identifier expression (e.g., `x`, `println`).
    ExprIdentifier Identifier
  | -- | Unary operation expression (e.g., `!x`, `-4`).
    ExprUnaryOp UnaryOp Expression
  | -- | Binary operation expression (e.g., `x + 7`).
    ExprBinaryOp BinaryOp Expression Expression
  | -- | Function call expression.
    -- E.g., `foo(17, x, bar())`, `(func (x int) int { return x * x; })(3)`.
    ExprFuncCall Expression [Expression]
  | -- | Array access by index expression.
    -- E.g., `a[3]`, `([2] int {3, 5})[1 + foo()]`, assuming that `foo()` returns `int`.
    ExprArrayAccessByIndex Expression Int
  deriving (Show)

-- Operators

-- | Binary operators.
data BinaryOp
  = -- | a || b
    OrOp
  | -- | a && b
    AndOp
  | -- | Relation operator.
    RelOp RelOp
  | -- | Additive operator.
    AddOp AddOp
  | -- | Multiplicative operator.
    MulOp MulOp
  deriving (Show)

-- | Relation operators.
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

-- | Additive operators.
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

-- | Multiplicative operators.
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

-- | Unary operators.
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

-- Types

data Type
  = -- | 64-bit (???) integer type.
    TInt
  | -- | Boolean type.
    TBool
  | -- | String type.
    TString
  | -- TODO
    TArray ArrayType
  | -- TODO
    TFunction FunctionType
  deriving (Show, Eq)

data ArrayType = ArrayType
  { elementType :: Type,
    length :: Int
  }
  deriving (Show, Eq)

data FunctionType = FunctionType
  { parameters :: [Type],
    result :: Maybe Type
  }
  deriving (Show, Eq)

-- Function definition

data FunctionDef = FunctionDef
  { name :: Identifier,
    signature :: FunctionSignature,
    body :: [Statement]
  }
  deriving (Show)

data FunctionSignature = FunctionSignature
  { parameters :: [(Identifier, Type)],
    result :: Maybe Type
  }
  deriving (Show)

-- Statements

-- | TODO
data Statement
  = -- | TODO
    StmtReturn (Maybe Expression)
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
    While
      { whileCondition :: Expression,
        block :: [Statement]
      }
  | -- | TODO
    Loop
      { block :: [Statement]
      }
  deriving (Show)

newtype VarDecl = VarDecl [VarSpec]
  deriving (Show)

data VarSpec = VarSpec
  { identifier :: Identifier,
    t :: Maybe Type,
    value :: Expression
  }
  deriving (Show)

data IfElse = IfElse
  { simpleStmt :: Maybe SimpleStmt,
    condition :: Expression,
    block :: [Statement],
    elseStmt :: Either IfElse [Statement]
  }
  deriving (Show)

data SimpleStmt
  = StmtAssignment AssignmentLhs Expression
  | StmtInc Expression
  | StmtDec Expression
  | StmtShortVarDecl Identifier Expression
  | StmtExpression Expression
  deriving (Show)

data AssignmentLhs
  = AssignmentLhsVar Identifier
  | AssignmentLhsArrayElement Identifier [Int]
  | AssignmentLhsBlank
  deriving (Show)

-- Literals

data Literal
  = LitInt Int
  | LitBool Bool
  | LitString Text
  | LitArray
      { t :: ArrayType,
        value :: [Element]
      }
  | LitFunction
      { signature :: FunctionSignature,
        body :: [Statement]
      }
  deriving (Show)

-- | Represents runtime value of the calculated expression
type Value = Literal

data Element = ElementExpr Expression | ElementElements [Element]
  deriving (Show)

-- Identifier

type Identifier = Text
