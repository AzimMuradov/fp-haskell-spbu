module Interpreter where

import           AST

interpret :: Program -> IO ()
interpret pr = print $ runARM (fEntryP pr) pr

-- find entry point
fEntryP :: [FDefinition] -> FApp 
fEntryP (pr:prs) =
  case pr of
    NEntry _ _ -> fEntryP prs
    Entry n _  -> FApp n FEmpt
fEntryP [] = error "No functions founded"
-- find function definition
fFunD :: FName -> Program -> [Sentence]
fFunD n [] = error ("Cant find fun: " ++ show n)
fFunD n (Entry fn sen:xpr) = if n == fn then sen else fFunD n xpr
fFunD n (NEntry fn sen:xpr) = if n == fn then sen else fFunD n xpr

-- trying to match whole function 
match :: FExpr -> [Sentence] -> RSide
match FEmpt (Cond _ _ rs:_) = rs
match f [] = error (show f ++ "recognition impossible")
match ex (Cond pat _ rs:xs) =
  case mOne ex pat of
    Nothing ->  match ex xs
    Just con -> replace rs con
  where
    -- trying to match with 1 expr. Also provides conformity(con)
    mOne :: FExpr -> Expr -> Maybe [(Term, Term)]
    mOne FEmpt Empt = Just []
    mOne FEmpt _ = Nothing
    mOne _ Empt = Nothing
    mOne (FTCons tx exx) (Cons t exs) =
        if tx == t then
            (case mOne exx exs of
               Nothing -> Nothing
               Just con -> Just ((t, tx) : con))
        else
            Nothing
    mOne (FACons _ _) (Cons _ _) = Nothing
    -- substituting using conformity 
    replace :: RSide -> [(Term, Term)] -> RSide
    replace ex con =
      case ex of
        FEmpt -> FEmpt
        FTCons (Var t) fex -> FTCons (findV t con) (replace fex con)
        FTCons s fex -> FTCons s (replace fex con)
        FACons (FApp n ex) fex ->
          FACons (FApp n (replace ex con)) (replace fex con)
    findV :: Var -> [(Term, Term)] -> Term
    findV g [] = error $ "Can not find pattern for " ++ show g
    findV v (x:con) =
      case fst x of
        Var u -> if u == v then snd x else findV v con
        _ -> findV v con

-- Check for absence of function expressions
isExp :: FExpr -> Bool
isExp fex =
  case fex of
    FEmpt         -> True
    FTCons _ fex  -> isExp fex
    FACons _ _ -> False

-- merge of two expressions (like lists)
mrg :: FExpr -> FExpr -> FExpr
mrg xs FEmpt         = xs
mrg FEmpt ys         = ys
mrg (FACons a xs) ys = FACons a (xs `mrg` ys)
mrg (FTCons t xs) ys = FTCons t (xs `mrg` ys)

-- Iteration of Abstract Refal Mashine
rIter :: FExpr -> Program -> FExpr
rIter fex pr =
  case fex of
    FEmpt        -> FEmpt
    FTCons t fex -> FTCons t (rIter fex pr)
    FACons a fex -> runARM a pr `mrg` rIter fex pr

--main runARM of ARM    
runARM :: FApp -> Program -> FExpr
runARM (FApp n fex) pr = if isExp fex then
    (case n of
       Usr n' -> rIter (match fex (fFunD n pr)) pr
       Op a -> appBin a fex)
else
    runARM (FApp n (replFirstApp fex)) pr
  where
    replFirstApp :: FExpr -> FExpr
    replFirstApp fex =
      case fex of
        FEmpt -> FEmpt
        FTCons t fex -> FTCons t (replFirstApp fex)
        FACons (FApp n args) fex ->
            if isExp args then
                (case n of
                   Usr n' -> match args (fFunD n pr) `mrg` fex
                   Op a -> appBin a args `mrg` fex)
            else
                FACons (FApp n (replFirstApp args)) fex
-- Applying binary operation (Add, Mul, Sub)
appBin :: BinOp -> FExpr -> FExpr
appBin a (FTCons (Sym (MDig t)) (FTCons (Sym (MDig s)) FEmpt)) =
  FTCons
    (Sym
       (MDig
          (case a of
             Add -> (+) t s
             Sub -> (-) t s
             Mul -> (*) t s)))
    FEmpt
appBin _ _ = error "Too many aruments"
