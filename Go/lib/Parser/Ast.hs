{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Parser.Ast where

import Data.Text (Text)

--------------------------------------------------------Program---------------------------------------------------------

-- | The head of the AST.
data Program = Program
  { -- | Top level variable declarations.
    topLevelVarDecls :: [VarDecl],
    -- | Top level function definitions.
    topLevelFunctionDefs :: [FunctionDef]
  }
  deriving (Show)

-- | Function definition.
data FunctionDef = FunctionDef {funcName :: Identifier, funcVal :: FunctionValue}
  deriving (Show)

------------------------------------------------------Expressions-------------------------------------------------------

-- | Expression.
data Expression
  = -- | Value expression (e.g., `"Hello!"`, `17`, `true`).
    ExprValue Value
  | -- | Identifier expression (e.g., `x`, `foo`).
    ExprIdentifier Identifier
  | -- | Unary operation expression (e.g., `!x`, `-4`).
    ExprUnaryOp UnaryOp Expression
  | -- | Binary operation expression (e.g., `x + 7`).
    ExprBinaryOp BinaryOp Expression Expression
  | -- | Array access by index expression.
    -- E.g., `a[3]`, `([2] int {3, 5})[1 + foo()]`, assuming that `foo()` returns `int`.
    ExprArrayAccessByIndex Expression Expression
  | -- | Function call expression.
    -- E.g., `foo(17, x, bar())`, `(func (x int) int { return x * x; })(3)`.
    ExprFuncCall Expression [Expression]
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
  | -- | Less than or equal operator (a <= b), works only for `int` and `string`.
    LeOp
  | -- | Less than operator (a < b), works only for `int` and `string`.
    LtOp
  | -- | More than or equal operator (a >= b), works only for `int` and `string`.
    MeOp
  | -- | More than operator (a > b), works only for `int` and `string`.
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

---------------------------------------------------------Types----------------------------------------------------------

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
  deriving (Show)

-- | Array type, it contains the length of the array and its elements type.
data ArrayType = ArrayType {elementType :: Type, length :: Expression}
  deriving (Show)

-- | Function type,
-- it contains the result of the function (which can be `void` if the result is equal to `Nothing`)
-- and its parameters types.
data FunctionType = FunctionType {parameters :: [Type], result :: Maybe Type}
  deriving (Show)

-------------------------------------------------------Statements-------------------------------------------------------

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
data For = For {kind :: ForKind, block :: [Statement]}
  deriving (Show)

-- TODO : Docs
data ForKind
  = -- TODO : Docs
    ForKindFor {preStmt :: Maybe SimpleStmt, condition :: Maybe Expression, postStmt :: Maybe SimpleStmt}
  | -- TODO : Docs
    ForKindWhile {whileCondition :: Expression}
  | -- TODO : Docs
    ForKindLoop
  deriving (Show)

-- | Var declaration, one var declaration may contain many var specifications (e.g., `var x int = 3`, `var (x int = 3; y string = "")`).
newtype VarDecl = VarDecl [VarSpec]
  deriving (Show)

-- | Var specification (e.g., `x int = 3`, `y = "hello"`, `z int`).
data VarSpec
  = VarSpec Identifier (Maybe Type) Expression
  | DefaultedVarSpec Identifier Type
  deriving (Show)

-- If-else statement.
-- E.g., `if i := foo(14); i < 42 { return "hello"; } else { return "goodbye"; }`, `if true { println("hello"); }`.
data IfElse = IfElse
  { simpleStmt :: Maybe SimpleStmt,
    condition :: Expression,
    block :: [Statement],
    elseStmt :: Maybe (Either IfElse [Statement])
  }
  deriving (Show)

-- | Simple statement, its main difference between other statements is that it can be used inside `if` condition and `for` "pre" and "post" statements.
--
-- E.g., `if i := foo(14); i < 42 { return "hello"; } else { return "goodbye"; }`, `for i := 0; i < n; i++ { println(i); }`.
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
    UpdArrEl Identifier [Expression]
  deriving (Show)

--------------------------------------------------------Literals--------------------------------------------------------

-- | Literal value.
data Value
  = -- | Int literal (e.g., `17`, `0xFF`, `0b101001`).
    ValInt Integer
  | -- | Boolean literal (e.g., `true`, `false`).
    ValBool Bool
  | -- | String literal (e.g., `"Hello"`, `""`, `"Some\ntext"`).
    ValString Text
  | -- | Array literal.
    ValArray ArrayLiteral
  | -- | Function literal.
    ValFunction FunctionValue
  deriving (Show)

-- | Array literal (e.g., `[3] int {1, 2}`, `[10] bool`).
data ArrayLiteral = ArrayLiteral {t :: ArrayType, value :: [Expression]}
  deriving (Show)

-- | Function literal.
data FunctionValue
  = -- | Function literal (e.g., `func (x int) int { return x * x; }`, `func () {}`).
    Function {signature :: FunctionSignature, body :: [Statement]}
  | -- | Null (nil) literal (e.g., `nil`).
    Nil
  deriving (Show)

-- | Function signature,
-- it contains the result of the function (which can be `void` if the result is equal to `Nothing`)
-- and its parameters.
data FunctionSignature = FunctionSignature {parameters :: [(Identifier, Type)], result :: Maybe Type}
  deriving (Show)

-- Identifier

-- | Any valid identifier (e.g., `he42llo`, `_42`).
type Identifier = Text
