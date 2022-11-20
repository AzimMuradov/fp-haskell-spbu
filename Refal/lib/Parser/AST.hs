module AST where

data FName
  = Usr String -- done
  | Op BinOp

instance Show FName where
  show (Usr a)    = show a
  show (Op Add) = " + "
  show (Op Sub) = " - "
  show (Op Mul) = " * "

instance Eq FName where
  Usr a == Usr b       = a == b
  Op Add == Op Add = True
  Op Sub == Op Sub = True
  Op Mul == Op Mul = True
  _ == _               = False

-- 's' or 't' or 'e' before "."  
type TypeInd = Char 

---------------------------------------Expressions-----------------------------------

-- language main structure, like a JSON 
-- ('a' 'y') 'd' 'd' (('g') 'j' ('k' 'f') ()) 'p' ((('t')))
data Expr
  = Empt
  | Cons Term Expr

instance Eq Expr where
  Empt == Empt           = True
  Cons t ex == Cons s ey = t == s && ex == ey
  _ == _ = False

instance Show Expr where
  show Empt       = ""
  show (Cons a b) = show a ++ show b

-- Term can be Symbol, Var (s/t/e) or Expression in parens.
data Term 
  = Sym Symbol
  | Var Var 
  | Par Expr

instance Eq Term where
  (Sym a) == (Sym b)          = a == b
  (Var a) == (Var b)          = a == b
  (Par ex) == (Par ey)        = ex == ey
  (Sym _) == (Var (SVar _))   = True
  _ == _                      = True

instance Show Term where
  show (Sym a)      = show a ++ " "
  show (Var a)      = show a ++ " "
  show (Par a)      = "( " ++ show a ++ ") "

-- S var is equal to a single symbol, T is anything in parens, E is many or 0
data Var
  = SVar FName -- s.asd228
  | TVar FName -- t.qwe123
  | EVar FName -- e.zxc322

instance Eq Var where
  SVar a == SVar b = a == b
  TVar a == TVar b = a == b
  EVar a == EVar b = a == b
  _ == _ = False

instance Show Var where
  show (SVar a) = "s." ++ show a
  show (TVar a) = "t." ++ show a
  show (EVar a) = "e." ++ show a

data Symbol
  = Ch Char -- 'This is many symbols' = 'T' 'h' ...
  | Comp String -- "This is one symbol =)" Atom
  | ID FName -- Identifier
  | MDig Integer -- 2543 88918 9 is a sequence of three macrodigits
  deriving (Eq)

instance Show Symbol where
  show (Ch a)   = show a
  show (Comp a) = show a
  show (ID a)   = "id_" ++ show a
  show (MDig a) = show a

---------------------------------------Program---------------------------------------------
--Program on refal 5 is list of function defined 
type Program = [FDefinition]

-- Bin operators for math
data BinOp
  = Add
  | Sub
  | Mul

-- Function can be an Entry point of program, or not
-- foo { code Block }
-- ENTRY Go { code Block }
data FDefinition
  = NEntry FName [Sentence] 
  | Entry FName [Sentence] 
  deriving (Show)

--TO IMPLEMENT (Modules)
-- data external-decl = $EXTERNAL  f-name-list
--                   $EXTERN  f-name-list
--                   $EXTRN  f-name-list
-- data f-name-list = f-name
--                 f-name , f-name-list

-- Where are might be tricky expressions which differ in that they are not pure
data FExpr
  = FEmpt
  | FTCons Term FExpr
  | FACons FApp FExpr

instance Show FExpr where
  show (FEmpt)      = ""
  show (FTCons t e) = show t ++ show e
  show (FACons t e) = show t ++ show e


-- Sentence is one "line" in block, establishing a correspondence 
-- between the range of values ​​and function definitions 
-- left side <condition> (optrional)  = right side
data Sentence
      =
  Cond LSide Cond RSide

-- Calls of functions cant be on left side
type LSide = Expr

-- And possibly on the right
type RSide = FExpr

instance Show Sentence where
  show (Cond a b c) = show a ++ "= " ++ show b ++ show c


-- Call of function with its name and 'args', which are expression with 
-- other functions applications
data FApp =
  FApp FName FExpr

instance Show FApp where
  show (FApp n e) = "<" ++ show n ++ " " ++ show e ++ ">"


-- Condition might sort app some matches
-- [, Arg : pattern [, ’aeiou’: e.3 s.c e.4]] and = smth
data Cond
  = Nil
  | WIs Arg Expr Cond -- 

instance Show Cond where
  show Nil         = ""
  show (WIs a e c) = ", " ++ show a ++ " : " ++ show e ++ show c

type Arg = Expr
