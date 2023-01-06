{-# LANGUAGE InstanceSigs #-}

module AST where

type FName = String -- done


-- 's' or 't' or 'e' before "."
type TypeInd = Char

---------------------------------------Expressions-----------------------------------

-- language main structure, like a JSON
-- ('a' 'y') 'd' 'd' (('g') 'j' ('k' 'f') ()) 'p' ((('t')))
type Pattern = [Term]


-- Term can be Symbol, Var (s/t/e) or Expression in parens.
data Term
  = Sym Symbol
  | Var Var
  | Par Pattern

instance Eq Term
 where
  Sym a == Sym b = a == b
  Var a == Var b = a == b
  Par _ == Par _ = True
  _ == _         = False

instance Show Term where
  show :: Term -> String
  show (Sym a) = show a
  show (Var a) = show a
  show (Par a) = "(-" ++ show a ++ "-)"


-- S var is equal to a single symbol, T is anything in parens, E is many or 0
data Var
  = SVar String -- s.asd228
  | TVar String -- t.qwe123
  | EVar String -- e.zxc322

instance Eq Var
 where
  SVar a == SVar b = a == b
  TVar a == TVar b = a == b
  EVar a == EVar b = a == b
  _ == _           = False

instance Show Var
 where
  show (SVar a) = "s." ++ a
  show (TVar a) = "t." ++ a
  show (EVar a) = "e." ++ a

data Symbol
  = Ch Char -- 'This is many symbols' = 'T' 'h' ...
  | Comp String -- "This is one symbol =)" Atom
  | ID FName -- Identifier
  | MDig Integer -- 2543 88918 9 is a sequence of three macrodigits
  deriving (Eq)

instance Show Symbol
 where
  show (Ch a)   = show a
  show (Comp a) = show a
  show (ID a)   = "id_" ++ show a
  show (MDig a) = show a

---------------------------------------Program---------------------------------------------
--Program on refal 5 is list of function defined
type Program = [FDefinition]



-- Function can be an Entry point of program, or not
-- foo { code Block }
-- ENTRY Go { code Block }
data FDefinition
  = NEntry FName [Sentence]
  | Entry FName [Sentence]
  deriving (Show, Eq)


--TO IMPLEMENT (Modules)
-- data external-decl = $EXTERNAL  f-name-list
--                   $EXTERN  f-name-list
--                   $EXTRN  f-name-list
-- data f-name-list = f-name
--                 f-name , f-name-list

-- Where are might be tricky expressions which differ in that they are not pure
data FTerm
  = Term Term
  | FAct FApp
  deriving (Eq)

instance Show FTerm
 where
  show (Term t) = show t
  show (FAct a) = show a

type FExpr = [FTerm]


-- Sentence is one "line" in block, establishing a correspondence
-- between the range of values ​​and function definitions
-- left side <condition> (optrional)  = right side
data Sentence =
  Stc LSide Cond RSide
  deriving (Eq)


-- Calls of functions cant be on left side
type LSide = Pattern


-- And possibly on the right
type RSide = FExpr

instance Show Sentence
 where
  show (Stc a b c) = show a ++ "= " ++ show b ++ show c


-- Call of function with its name and 'args', which are expression with
-- other functions applications
data FApp =
  FApp FName FExpr
  deriving (Eq)

instance Show FApp
 where
  show (FApp n e) = "<" ++ show n ++ " " ++ show e ++ ">"


-- Condition might sort app some matches
-- [, Arg : pattern [, ’aeiou’: e.3 s.c e.4]] and = smth
data Cond
  = Nil
  | WIs Arg Pattern Cond
  deriving (Eq)

instance Show Cond where
  show :: Cond -> String
  show Nil         = ""
  show (WIs a e c) = ", " ++ show a ++ " : " ++ show e ++ show c

type Arg = FExpr
