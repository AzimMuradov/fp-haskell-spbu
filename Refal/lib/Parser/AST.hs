module AST where

import qualified Data.Map as Map

type FName = String -- +

type TypeInd = Char -- +

data Expr
  = Empty
  | Cons Term Expr
  deriving(Show)

data Term
  = Sym Symbol -- 'a'
  | Variable Var -- t/s/e
  | Par Expr -- ( __ )
  deriving(Show)
{-
e.K '=' e.V ~ Cons e.K (Cons = (Cons e.V (Empty)))
e.K ~ Variable e.K ~ Variable (EVar e K) ~ Variable (EVar (TypeInd 'e') (Identifier "K"))
=   ~ Symbol '='

( ('abc') 'd' (('gh') 'j' ()) )

Cons ('abc')           (Cons 'd'        (Cons ( ('gh') 'j' ()) )         Empty) ~
Cons Parenthesis 'abc' (Cons Symbol 'd' (Cons Parenthesis ('gh') 'j' ()) Empty) ~

'abc' = Cons Symbol 'a' (Cons Symbol 'b' (Cons Symbol 'c' Empty))
-}
data Var
  = SVar FName -- s.c
  | TVar FName -- t.213some
  | EVar FName -- e.2
  deriving(Show)

data Symbol
  = Ch Char -- 'This is many symbols' = 'T' 'h' ...
  | Comp String -- "This is one symbol =)" Atom
  | ID FName --
  | MacroDigit Integer -- 2543 88918 9 is a sequence of three macrodigits

instance Show Symbol where
  show (Ch a)         = "Char " ++ show a
  show (Comp a)       = "Comp " ++ show a
  show (ID a)         = "ID " ++ show a
  show (MacroDigit a) = "MDig " ++ show a



data Program
  = Fdef FDefinition
  | FdefAndProg FDefinition Program

data FDefinition
  = NEntry FName Block -- foo { code Block }
  | Entry FName Block -- ENTRY Go { code Block }
              --TO IMPLEM

-- data external-decl = $EXTERNAL  f-name-list
--                   $EXTERN  f-name-list
--                   $EXTRN  f-name-list
-- data f-name-list = f-name
--                 f-name , f-name-list
data Block
  = Sent Sentence -- '0' '1' = '1'
  | SentSemi Sentence -- '0' '0' = '0';
  | SentBlock Sentence Block --  0' '0' = '0'; ...

{-
BinAdd {
'0' '0' = '0';
'0' '1' = '1'
}
-}
data Sentence
  = Default LSide Cond RSide -- = 'True' or s.1 = sadasd 21321 asd;
  | Conditional LSide Cond BlockEnd

type LSide = Expr

data Cond
  = Nil
  | WhereIs Arg Expr Cond -- , Arg : pattern Cond, ’aeiou’: e.3 s.c e.4 = s.c

{-
BinAdd {
e.1 s.c s.c e.2 , 'aeiou': e.3 s.c e.4, 'e': s.c = s.c;
}

Cond is [ [, 'aeiou': e.3 s.c e.4] [, 'e' : s.c] ]
WhereIs (Arg{'aeiou'} Expr{e.3 s.c e.4} (WhereIs Arg{'e'} Expr{s.c} (Empty) ) )
-}
type Arg = Expr

type RSide = Expr

data BlockEnd =
  Bl Arg Block -- arg : { block }
-- joinOrSplit :: Pretty a => [String] -> a -> [String]
-- joinOrSplit s e =
--   case prettify e of
--     [r]   -> addToLast s (" (" ++ r ++ ")")
--     listR -> addToLast s " (" ++ ["  " ++ r' | r' <- listR] ++ [")"]
-- prettifyAST :: Pretty e => [e] -> [String]
-- prettifyAST = map (joinN . prettify)
-- joinedPrettyAST :: Pretty e => [e] -> String
-- joinedPrettyAST = joinN . prettifyAST
-- instance Pretty e => Pretty (CodeBlock e) where
--   prettify exprs = concatMap tabExpr exprs
--     where
--       tabExpr e = map ("  " ++) (prettify e)
-- smartJoin :: [String] -> [String]
-- smartJoin strs =
--   if sum (map length strs) < 40
--     then [joinS (map (dropWhile isSpace) strs)]
--     else strs
-- instance Pretty Expr where
--   prettify expr =
--     case expr of
--       (Int i) -> [joinS ["Int", show i]]
--       (Var n) -> [joinS ["Var", show n]]
--       (Def t n) -> [joinS ["Def", show t, show n]]
--       (Block es) -> smartJoin ("Block {" : prettify es ++ ["}"])
--       (Call f es) ->
--         smartJoin (joinS ["Call", show f, "("] : prettify es ++ [")"])
--       (Function t n a r body) ->
--         joinS
--           [ "Function"
--           , show n
--           , show t
--           , "; args"
--           , show a
--           , "; returns"
--           , show r
--           , "{"
--           ] :
--         prettify body ++ ["}"]
--       (BinaryOp op e1 e2) -> joinOrSplit (joinOrSplit ["BinaryOp " ++ op] e1) e2
--       (If eq bl1 bl2) ->
--         addToLast (joinOrSplit ["If"] eq) " {" ++
--         prettify bl1 ++ ["}", "else {"] ++ prettify bl2 ++ ["}"]
