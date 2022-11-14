{-# LANGUAGE DuplicateRecordFields #-}

module Ast where

import Data.Text (Text)

newtype Program = Program [TopLevelDecl]
  deriving (Show)

data TopLevelDecl = TopLevelVarDecl [VarSpec] | TopLevelFunctionDecl FunctionDecl
  deriving (Show)

-- Expression

data Expression = Expression {x :: UnaryExpr, xs :: [(BinaryOp, UnaryExpr)]}
  deriving (Show)

data UnaryExpr = UnaryExpr {operators :: [UnaryOp], expr :: PrimaryExpr}
  deriving (Show)

data PrimaryExpr = PrimaryExpr {op :: Operand, postEls :: [Expression]}
  deriving (Show)

data Operand = OperandLiteral Literal | OperandName Identifier | OperandExpression Expression
  deriving (Show)

-- Operators

data AssignOp = AssignOp | ComplexAssignOp (Either AddOp MulOp)
  deriving (Show)

data BinaryOp = OrOp | AndOp | RelOp RelOp | AddOp AddOp | MulOp MulOp
  deriving (Show)

data RelOp = EqOp | NeOp | LtOp | LeOp | MtOp | MeOp
  deriving (Show)

data AddOp = PlusOp | MinusOp | BitOrOp | BitXorOp
  deriving (Show)

data MulOp = MultOp | DivOp | ModOp | BitShiftLeftOp | BitShiftRightOp | BitAndOp | BitClearOp
  deriving (Show)

data UnaryOp = UnaryPlusOp | UnaryMinusOp | NotOp | BitwiseComplementOp
  deriving (Show)

-- Type

data Type
  = IntT
  | BoolT
  | StringT
  | ArrayT ArrayType
  | FunctionT NamelessFuncSignature
  deriving (Show)

-- Function declaration (definition)

data FunctionDecl = FunctionDecl {name :: Identifier, signature :: FuncSignature, body :: [Statement]}
  deriving (Show)

data NamelessFuncSignature = NamelessFuncSignature {parameters :: NamelessFuncParameters, result :: Maybe Result}
  deriving (Show)

data FuncSignature = FuncSignature {parameters :: FuncParameters, result :: Maybe Result}
  deriving (Show)

data Result = NamelessParameters NamelessFuncParameters | Type Type
  deriving (Show)

data NamelessFuncParameters = OneNPar Type | MultipleNPar Type
  deriving (Show)

data FuncParameters = OnePar Identifier Type | MultiplePar Identifier Type
  deriving (Show)

-- Statements

data Statement
  = ReturnStmt (Maybe [Expression])
  | BreakStmt
  | ContinueStmt
  | ForStmt
  | VarDecl [VarSpec]
  | IfElseStmt
  | Block [Statement]
  | SimpleStmt SimpleStmt
  deriving (Show)

-- data ForStmt   = ForStmt{ ( ForClause | Condition )? ~ block::[Statement] }
-- data ForClause = ForClause { preStament::Maybe SimpleStmt, condition:: Maybe Condition, postStament::Maybe SimpleStm }

-- Condition = { Expression }

data VarSpec = VarSpec {identifiers :: [Identifier], t :: Maybe Type, expressions :: [Expression]}
  deriving (Show)

-- IfElseStmt = { "if" ~ ( SimpleStmt ~ ";" )? ~ Expression ~ block::[Statement] ~ ("else" ~ ( IfElseStmt | block::[Statement] ))? }

data SimpleStmt
  = AssignmentStmt {lhs :: [Expression], op :: AssignOp, rhs :: [Expression]}
  | IncStmt Expression
  | DecStmt Expression
  | ShortVarDeclStmt {identifiers :: [Identifier], exprs :: [Expression]}
  | ExpressionStmt Expression
  deriving (Show)

-- Array

data ArrayType = ArrayType {elementType :: Type, length :: Integer}
  deriving (Show)

-- Literal

data Literal
  = StringLiteral Text
  | IntLiteral Int
  | BoolLiteral Bool
  | ArrayLiteral {t :: ArrayType, value :: [Element]}
  | FunctionLiteral {signature :: FuncSignature, body :: [Statement]}
  deriving (Show)

data Element = ElementExpr Expression | ElementElements [Element]
  deriving (Show)

-- Identifier

newtype Identifier = Identifier Text
  deriving (Show)