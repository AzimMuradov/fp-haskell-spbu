{-# LANGUAGE DuplicateRecordFields #-}

module Ast where

import Data.Text (Text)

newtype Program = Program [Statement]
  deriving (Show)

-- Statements

data Statement
  = SExpr Expr
  | SVarDecl VarDecl --                 ( let x = 5                             )
  | SFunDecl FunDecl --    ( let f x y = x + y                     )
  | SRecFunDecl RecFunDecl {- ( let rec fib n =                       )
                                                (   match n with                        )
                                                (   | 0 | 1 -> n                        )
                                                (   | n -> fib (n-1) + fib (n-2)        )
                                              -}
  | SBlock [Statement] --                       ( block which contain list of statement )
  deriving (Show)

-- Expression

data Expr
  = ENull
  | EIdentifier Identifier--                   ( "x", "5"                             )
  | EValue Value --                             ( 5                                    )
  | EOperations Operations --                   ( +, -                                 )
  | EFun [(Identifier, Maybe Type)] [Statement] --              ( fun x y -> x + y                     )
  | EApplication Expr [Expr] --           ( f x y                                )
  | EIf Expr Statement Statement --             ( if cond then expr' else expr''       )
  | ELetIn Identifier Expr [Expr] --          ( f x y = let p = x + y in p           )
  deriving (Show)

-- [<Measure>] type unit-name [ = measure ]
-- [<Measure>] type cm
-- [<Measure>] type ml = cm^3

-- Declarations

data VarDecl = VarDecl (Identifier, Maybe Type) Expr deriving (Show)
data FunDecl = FunDecl Identifier [(Identifier, Maybe Type)] [Statement] deriving (Show)
data RecFunDecl = RecFunDecl Identifier [(Identifier, Maybe Type)] [Statement] deriving (Show)
data MeasureDecl = MeasureDecl Identifier (Maybe Expr)

-- Measure

-- Types

data Type
  = TBool
  | TInt
  -- | TFloat
  -- | TDecimal
  -- | Measure
  | TFun
  deriving (Show)

-- Values

data Value
  = VBool Bool
  | VInt Integer
  -- | VFloat Float
  -- | VDecimal Double
--  | VFun 
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
  | MulOp Expr Expr --          (  *  )
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