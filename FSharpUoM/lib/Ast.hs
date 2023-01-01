{-# LANGUAGE DuplicateRecordFields #-}

module Ast where

import Data.Text (Text)

newtype Program = Program [Statement]
  deriving (Show)

-- Declarations

data VarDecl = VarDecl (Identifier, Maybe Type) Expr deriving (Show)
data FunDecl = FunDecl Identifier [(Identifier, Maybe Type)] [Statement] deriving (Show)
data RecFunDecl = RecFunDecl Identifier [(Identifier, Maybe Type)] [Statement] deriving (Show)
data MeasureDecl = MeasureDecl Identifier (Maybe MeasureTypeExpr) deriving (Show)

-- Statements

data Statement
  = SExpr Expr
  | SMeasureDecl MeasureDecl
  | SVarDecl VarDecl --      ( let x = 5 )
  | SFunDecl FunDecl --      ( let f x y = x + y )
  | SRecFunDecl RecFunDecl
  deriving (Show)

-- Expression

data Expr
  = EIdentifier Identifier--                       ( "x", "5"                       )
  | EValue Value --                                ( 5                              )
  | EOperations Operations --                      ( +, -                           )
  | EApplication Expr [Expr] --                    ( f x y                          )
  | EIf Expr [Statement] [Statement] --            ( if cond then expr' else expr'' )
  | ELetIn Identifier Expr [Statement] --          ( f x y = let p = x + y in p     )
  deriving (Show)

-- Types

-- BasicTypes

data Type
  = TBool
  | TInt (Maybe MeasureTypeExpr)
  | TDouble (Maybe MeasureTypeExpr)
  | TFun [Type]
  deriving (Show)

-- MeasureType

data MeasureTypeExpr
  = MIdentifier Identifier
  | MTypesMul MeasureTypeExpr MeasureTypeExpr
  | MTypesDiv MeasureTypeExpr MeasureTypeExpr
  | MTypesExp MeasureTypeExpr Integer
  deriving (Show)

-- Values

data Value
  = VBool Bool
  | VInt Integer (Maybe MeasureTypeExpr)
  | VDouble Double (Maybe MeasureTypeExpr)
  | VFun [(Identifier, Maybe Type)] [Statement] -- ( fun x y -> x + y )
  deriving (Show)

-- Operators

data Operations
  = BooleanOp BooleanOp
  | UnaryOp UnaryOp --                ( not )
  | ArithmeticOp ArithmeticOp
  | ComparisonOp ComparisonOp
  deriving (Show)

data UnaryOp
  = NotOp Expr
  | UnaryMinus Expr
  deriving (Show)

data BooleanOp
  = AndOp Expr Expr --           (  && )
  | OrOp Expr Expr --            (  || )
  deriving (Show)

data ArithmeticOp
  = PlusOp Expr Expr --          (  +  )
  | MinusOp Expr Expr --         (  -  )
  | MulOp Expr Expr --           (  *  )
  | DivOp Expr Expr --           (  /  )
  | ModOp Expr Expr --           (  %  )
  | ExpOp Expr Expr --           (  ** )
  deriving (Show)

data ComparisonOp
  = EqOp Expr Expr --            (  == )
  | NeOp Expr Expr --            (  <> )
  | LtOp Expr Expr --            (  <  )
  | LeOp Expr Expr --            (  <= )
  | MtOp Expr Expr --            (  >  )
  | MeOp Expr Expr --            (  >= )
  deriving (Show)

-- Identifier

newtype Identifier = Identifier Text deriving (Show)