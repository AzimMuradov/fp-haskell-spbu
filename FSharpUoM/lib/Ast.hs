{-# LANGUAGE DuplicateRecordFields #-}

module Ast where

import Data.Text (Text)

newtype Program = Program [Statement]
  deriving (Show)

-- Declarations

data VarDecl = VarDecl (Identifier, Maybe Type) [Expr] deriving (Show, Eq)

data FunDecl = FunDecl Identifier Value deriving (Show, Eq)

data RecFunDecl = RecFunDecl Identifier Value deriving (Show, Eq)

data MeasureDecl = MeasureDecl Identifier (Maybe MeasureTypeExpr) deriving (Show, Eq)

-- Statements

data Statement
  = SExpr Expr
  | SMeasureDecl MeasureDecl
  | SVarDecl VarDecl --      ( let x = 5 )
  | SFunDecl FunDecl --      ( let f x y = x + y )
  | SRecFunDecl RecFunDecl
  deriving (Show, Eq)

-- Expression

data Expr
  = EIdentifier Identifier --                       ( "x", "5"                       )
  | EValue Value --                                ( 5                              )
  | EOperations Operations --                      ( +, -                           )
  | EApplication Expr Expr --                    ( f x y                          )
  | EIf Expr [Expr] [Expr] --            ( if cond then expr' else expr'' )
  | ELetInV (Identifier, Maybe Type) [Expr] [Expr]
  | ELetInF Identifier Value [Expr] --          ( f x y = let p = x + y in p     )
  deriving (Show, Eq)

-- Types

-- BasicTypes

data Type
  = TBool
  | TInt (Maybe MeasureTypeExpr)
  | TDouble (Maybe MeasureTypeExpr)
  | TFun Type Type
  deriving (Show, Eq)

-- MeasureType

data MeasureTypeExpr
  = MIdentifier Identifier
  | MTypesMul MeasureTypeExpr MeasureTypeExpr
  | MTypesDiv MeasureTypeExpr MeasureTypeExpr
  | MTypesExp MeasureTypeExpr Integer
  deriving (Show, Eq)

-- Values

data Value
  = VBool Bool
  | VInt Integer (Maybe MeasureTypeExpr)
  | VDouble Double (Maybe MeasureTypeExpr)
  | VFun [(Identifier, Maybe Type)] [Expr] -- ( fun x y -> x + y )
  deriving (Show, Eq)

-- Operators

data Operations
  = BooleanOp BooleanOp
  | NotOp Expr --                ( not )
  | ArithmeticOp ArithmeticOp
  | ComparisonOp ComparisonOp
  deriving (Show, Eq)

data BooleanOp
  = AndOp {bL :: Expr, bR :: Expr} --           (  && )
  | OrOp {bL :: Expr, bR :: Expr} --            (  || )
  deriving (Show, Eq)

data ArithmeticOp
  = PlusOp {aL :: Expr, aR :: Expr} --          (  +  )
  | MinusOp {aL :: Expr, aR :: Expr} --         (  -  )
  | MulOp {aL :: Expr, aR :: Expr} --           (  *  )
  | DivOp {aL :: Expr, aR :: Expr} --           (  /  )
  | ModOp {aL :: Expr, aR :: Expr} --           (  %  )
  | ExpOp {aL :: Expr, aR :: Expr} --           (  ** )
  deriving (Show, Eq)

data ComparisonOp
  = EqOp {cL :: Expr, cR :: Expr} --            (  == )
  | NeOp {cL :: Expr, cR :: Expr} --            (  <> )
  | LtOp {cL :: Expr, cR :: Expr} --            (  <  )
  | LeOp {cL :: Expr, cR :: Expr} --            (  <= )
  | MtOp {cL :: Expr, cR :: Expr} --            (  >  )
  | MeOp {cL :: Expr, cR :: Expr} --            (  >= )
  deriving (Show, Eq)

-- Identifier

type Identifier = Text -- deriving (Show, Eq, Ord)