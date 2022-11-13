{-# LANGUAGE DuplicateRecordFields #-}

module Ast where

import Data.Text (Text)

-- newtype Program = Program [TopLevelDecl]
--   deriving (Show)

-- data TopLevelDecl = TopLevelVarDecl [VarSpec] | TopLevelFunctionDecl FunctionDecl
--   deriving (Show)

data Type = TBool Bool
  | TInt Int
  | TChar Char
  | TString String
  | TTuple Type Type
  | TArray [Type]
  deriving (Show)

-- Identifier

newtype Identifier = Identifier Text deriving (Show)

newtype IsMutable = IsMutable Bool deriving (Show)

data ForArgs = To | DownTo | In deriving (Show)
 

-- Expression

data Expr = Const Identifier -- Identifier Identifier
  | Null
  | EOperations
  deriving (Show)

-- Statements

data Statement
  = SExpr Expr
  | SVarDecl Identifier IsMutable Expr -- TODO: Add recursion
  | SVar (Maybe Identifier)
  | SFunDecl Bool Identifier [Identifier] Expr
  | SFunction Identifier
  | SFun [Identifier] Expr
  | SApplication Expr Expr
  | SIf Expr Expr Expr
  | SMatchWith Expr Expr Expr
  | LetIn Identifier Expr Statement
--  | LetRec
  | SBlock [Statement]
  | SReturn (Maybe [Expr])
  | SBreakStmt
  | SContinue
  | SWhile Expr Statement
  | SForTo Identifier (Maybe Expr) ForArgs Expr Statement
  deriving (Show)

-- Operators

data Operations = BooleanOp BooleanOp
  | BitwiseOp BitwiseOp
  | ArithmeticOp ArithmeticOp 
  | ComparisonOp ComparisonOp
  deriving (Show)

data BooleanOp = AndOp Expr Expr
  | OrOp Expr Expr
  | NotOp Expr
  deriving (Show)

data BitwiseOp = BitOrOp Expr Expr
  | BitXorOp Expr Expr
  | BitAndOp Expr Expr
  | BitShiftLeftOp Expr Expr
  | BitShiftRightOp Expr Expr
  deriving (Show)

data ArithmeticOp = PlusOp Expr Expr
  | MinusOp Expr Expr
  | MultOp Expr Expr
  | DivOp Expr Expr
  | ModOp Expr Expr
  | ExpOp Expr Expr
  deriving (Show)

data ComparisonOp = EqOp Expr Expr
  | NeOp Expr Expr
  | LtOp Expr Expr
  | LeOp Expr Expr
  | MtOp Expr Expr
  | MeOp Expr Expr
  deriving (Show)
