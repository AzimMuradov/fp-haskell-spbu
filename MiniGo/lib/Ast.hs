{-# LANGUAGE DuplicateRecordFields #-}

module Ast where

import Data.Text (Text)

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
  | -- | Identifier expression (e.g., `x`, `foo`).
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
  = -- | Or operator (a || b), works only for `bool`.
    OrOp
  | -- | And operator (a && b), works only for `bool`.
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
  = -- | Equality operator (a == b).
    EqOp
  | -- | Inequality operator (a != b).
    NeOp
  | -- | Less than or equal operator (a <= b), works only for `int`.
    LeOp
  | -- | Less than operator (a < b), works only for `int`.
    LtOp
  | -- | More than or equal operator (a >= b), works only for `int`.
    MeOp
  | -- | More than operator (a > b), works only for `int`.
    MtOp
  deriving (Show)

-- | Additive operators.
data AddOp
  = -- | Plus operator (a + b), works only for `int` and `string`.
    PlusOp
  | -- | Minus operator (a - b), works only for `int`.
    MinusOp
  | -- | Bitwise or operator (a | b), works only for `int`.
    BitOrOp
  | -- | Bitwise xor operator (a ^ b), works only for `int`.
    BitXorOp
  deriving (Show)

-- | Multiplicative operators.
data MulOp
  = -- | Multiply operator (a * b), works only for `int`.
    MultOp
  | -- | Divide operator (a / b), works only for `int`.
    DivOp
  | -- | Module operator (a % b), works only for `int`.
    ModOp
  | -- | Bitwise left shift operator (a << b), works only for `int`.
    BitShiftLeftOp
  | -- | Bitwise right shift operator (a >> b), works only for `int`.
    BitShiftRightOp
  | -- | Bitwise clear (and not) operator (a &^ b), works only for `int`.
    BitClearOp
  | -- | Bitwise and operator (a & b), works only for `int`.
    BitAndOp
  deriving (Show)

-- | Unary operators.
data UnaryOp
  = -- | Unary plus operator (+a), works only for `int`.
    UnaryPlusOp
  | -- | Unary minus operator (-a), works only for `int`.
    UnaryMinusOp
  | -- | Not operator (!a), works only for `bool`.
    NotOp
  | -- | Bitwise complement operator (^a), works only for `int`.
    BitwiseComplementOp
  deriving (Show)

-- Types

-- | All existing types.
data Type
  = -- | 32-bit/64-bit (depending on the machine) integer type.
    TInt
  | -- | Boolean type.
    TBool
  | -- | String type.
    TString
  | -- | Array type.
    TArray ArrayType
  | -- | Function type.
    TFunction FunctionType
  deriving (Show, Eq)

-- | Array type, it contains the length of the array and its elements type.
data ArrayType = ArrayType
  { elementType :: Type,
    length :: Int
  }
  deriving (Show, Eq)

-- | Function type,
-- it contains the result of the function (which can be `void` if the result is equal to `Nothing`)
-- and its parameters types.
data FunctionType = FunctionType
  { parameters :: [Type],
    result :: Maybe Type
  }
  deriving (Show, Eq)

-- Function definition

-- TODO : Docs
data FunctionDef = FunctionDef
  { name :: Identifier,
    signature :: FunctionSignature,
    body :: [Statement]
  }
  deriving (Show)

-- TODO : Docs
data FunctionSignature = FunctionSignature
  { parameters :: [(Identifier, Type)],
    result :: Maybe Type
  }
  deriving (Show)

-- Statements

-- | Statement.
data Statement
  = -- | Return statement with optional return value (in the case of `Nothing` we assume, that its `void`).
    StmtReturn (Maybe Expression)
  | -- | Break statement, should be inside `for`.
    StmtBreak
  | -- | Continue statement, should be inside `for`.
    StmtContinue
  | -- | For statement, can represent any of the 3 possible `for` variants (see `For` data type).
    StmtFor For
  | -- | Var declaration statement.
    StmtVarDecl VarDecl
  | -- | If-else statement.
    StmtIfElse IfElse
  | -- | Block statement (e.g., `{ 34; foo(34); if true {} else {}; return 17; }`).
    StmtBlock [Statement]
  | -- | Simple statement.
    StmtSimple SimpleStmt
  deriving (Show)

-- TODO : Docs
data For
  = -- TODO : Docs
    For
      { preStatement :: Maybe SimpleStmt,
        forCondition :: Maybe Expression,
        postStatement :: Maybe SimpleStmt,
        block :: [Statement]
      }
  | -- TODO : Docs
    While
      { whileCondition :: Expression,
        block :: [Statement]
      }
  | -- TODO : Docs
    Loop
      { block :: [Statement]
      }
  deriving (Show)

-- TODO : Docs
newtype VarDecl = VarDecl [VarSpec]
  deriving (Show)

-- TODO : Docs
data VarSpec = VarSpec
  { identifier :: Identifier,
    t :: Maybe Type,
    value :: Expression
  }
  deriving (Show)

-- TODO : Docs
data IfElse = IfElse
  { simpleStmt :: Maybe SimpleStmt,
    condition :: Expression,
    block :: [Statement],
    elseStmt :: Either IfElse [Statement]
  }
  deriving (Show)

-- | Simple statement, its main difference between other statements is that it can be used inside `if` condition.
--
-- E.g., `if i := foo(14); i < 42 { return "hello"; } else { return "goodbye"; }`.
data SimpleStmt
  = -- | Assignment statement (e.g., `x = 17`, `a[3] = "42"`).
    StmtAssignment UpdatableElement Expression
  | -- | Increment statement (e.g., `x++`, `a[3]++`).
    StmtInc UpdatableElement
  | -- | Decrement statement (e.g., `x--`, `a[3]--`).
    StmtDec UpdatableElement
  | -- | Short var declaration statement (e.g., `x := 3`, `y := true`).
    StmtShortVarDecl Identifier Expression
  | -- | Expression statement.
    StmtExpression Expression
  deriving (Show)

-- | Any element that can be updated.
data UpdatableElement
  = -- | Any variable can be updated (e.g., `x = 3`, `x++`).
    UpdVar Identifier
  | -- | Any array element can be updated (e.g., `a[5][7] = 3`, `a[0]++`).
    UpdArrEl Identifier [Int]
  deriving (Show)

-- Literals

-- | Literal value.
data Literal
  = -- | Int literal (e.g., `17`, `0xFF`, `0b101001`).
    LitInt Int
  | -- | Boolean literal (e.g., `true`, `false`).
    LitBool Bool
  | -- | String literal (e.g., `"Hello"`, `""`, `"Some\ntext"`).
    LitString Text
  | -- | Array literal (e.g., `[3] int {1, 2}`, `[10] bool`).
    LitArray
      { t :: ArrayType,
        value :: [Element]
      }
  | -- | Function literal (e.g., `func (x int) int { return x * x; }`, `func () {}`).
    LitFunction
      { signature :: FunctionSignature,
        body :: [Statement]
      }
  | -- | Null (nil) literal (e.g., `nil`).
    LitNil
  deriving (Show)

-- | Represents runtime value of the calculated expression.
type Value = Literal

-- TODO : Docs
data Element
  = -- TODO : Docs
    ElementExpr Expression
  | -- TODO : Docs
    ElementElements [Element]
  deriving (Show)

-- Identifier

-- | Any valid identifier (e.g., `he42llo`, `_42`).
type Identifier = Text
