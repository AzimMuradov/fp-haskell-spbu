{-# LANGUAGE DuplicateRecordFields #-}

module Ast where

import Data.Text (Text)

newtype Program = Program [Statement]
  deriving (Show)

-- Declarations

data VarDecl = VarDecl (Identifier, Maybe Type) [Statement] deriving (Show)

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
  = EIdentifier Identifier --                       ( "x", "5"                       )
  | EValue Value --                                ( 5                              )
  | EOperations Operations --                      ( +, -                           )
  | EApplication Expr Expr --                    ( f x y                          )
  | EIf Expr [Statement] [Statement] --            ( if cond then expr' else expr'' )
  | ELetIn (Identifier, Maybe Type) Expr [Statement] --          ( f x y = let p = x + y in p     )
  deriving (Show)

-- Types

-- BasicTypes

data Type
  = TBool
  | TInt (Maybe MeasureTypeExpr)
  | TDouble (Maybe MeasureTypeExpr)
  | TFun Type Type
  deriving (Show)

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
  | VFun [(Identifier, Maybe Type)] [Statement] -- ( fun x y -> x + y )
  deriving (Show)

-- Operators

data Operations
  = BooleanOp BooleanOp
  | NotOp Expr --                ( not )
  | ArithmeticOp ArithmeticOp
  | ComparisonOp ComparisonOp
  deriving (Show)

data BooleanOp
  = AndOp {bL :: Expr, bR :: Expr} --           (  && )
  | OrOp {bL :: Expr, bR :: Expr} --            (  || )
  deriving (Show)

data ArithmeticOp
  = PlusOp {aL :: Expr, aR :: Expr} --          (  +  )
  | MinusOp {aL :: Expr, aR :: Expr} --         (  -  )
  | MulOp {aL :: Expr, aR :: Expr} --           (  *  )
  | DivOp {aL :: Expr, aR :: Expr} --           (  /  )
  | ModOp {aL :: Expr, aR :: Expr} --           (  %  )
  | ExpOp {aL :: Expr, aR :: Expr} --           (  ** )
  deriving (Show)

data ComparisonOp
  = EqOp {cL :: Expr, cR :: Expr} --            (  == )
  | NeOp {cL :: Expr, cR :: Expr} --            (  <> )
  | LtOp {cL :: Expr, cR :: Expr} --            (  <  )
  | LeOp {cL :: Expr, cR :: Expr} --            (  <= )
  | MtOp {cL :: Expr, cR :: Expr} --            (  >  )
  | MeOp {cL :: Expr, cR :: Expr} --            (  >= )
  deriving (Show)

-- Identifier

type Identifier = Text -- deriving (Show, Eq, Ord)