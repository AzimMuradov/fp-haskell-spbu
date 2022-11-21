{-# LANGUAGE DuplicateRecordFields #-}

module Ast where

import Data.Text (Text)

newtype Program = Program [TopLevelDecl]
  deriving (Show)

data TopLevelDecl = TopLevelVarDecl VarDecl
  | TopLevelFunDecl FunDecl
  | TopLevelRecFunDecl RecFunDecl
  deriving (Show)

data Type
  = TBool
  | TInt
  | TFun
  deriving (Show)

data Value
  = VBool Bool
  | VInt Integer
--  | VFun 
  deriving (Show)

{--

  | TString
  | TTuple
  | TArray

  | VString String
  | VTuple Type Type
  | VArray Expr

  newtype Measure = Measure Expr
  | VMeasure Measure
  | TMeasure

--}

-- Identifier

newtype Identifier = Identifier Text deriving (Show)

-- Expression

data Expr
  = ENull
  | EIdentifier Identifier--                   ( "x", "5"                             )
  | EValue Value --                             ( 5                                    )
  | EOperations Operations --                   ( +, -                                 )
  | EFun [(Identifier, Maybe Type)] Statement --              ( fun x y -> x + y                     )
  | EApplication Identifier [Expr] --           ( f x y                                )
  | EIf Expr Statement Statement --             ( if cond then expr' else expr''       )
  | ELetIn Identifier Expr Statement --          ( f x y = let p = x + y in p           )
  deriving (Show)

-- [<Measure>] type unit-name [ = measure ]
-- [<Measure>] type cm
-- [<Measure>] type ml = cm^3
--

-- | EMatchWith Expr Expr Statement {-           ( match x with                         )
--                                               (   | pattern'  -> expr'               )
--                                               (   | pattern'' | pattern''' -> expr'' )
--                                   -}

-- Statements

data VarDecl = VarDecl (Identifier, Maybe Type) Expr deriving (Show)

data FunDecl = FunDecl Identifier [(Identifier, Maybe Type)] Expr deriving (Show)
data RecFunDecl = RecFunDecl Identifier [(Identifier, Maybe Type)] Expr deriving (Show)

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

-- Operators

data Operations
  = BooleanOp BooleanOp
  | UnaryOp UnaryOp --                ( not )
  | BitwiseOp BitwiseOp
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

data BitwiseOp
  = BitOrOp Expr Expr --         ( ||| )
  | BitXorOp Expr Expr --        ( ^^^ )
  | BitAndOp Expr Expr --        ( &&& )
  | BitShiftLeftOp Expr Expr --  ( <<< )
  | BitShiftRightOp Expr Expr -- ( >>> )
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
