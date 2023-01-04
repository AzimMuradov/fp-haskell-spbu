module Interpreter where

import           AST
import           Control.Monad.Except
import           Data.List
import           Stdlib

type Congruity = (Var, Pattern)

type Conformity = [Congruity]

type ARMMonad = Either ARMError

type ARMError = String

interpret :: Program -> IO ()
interpret pr =
  case fEntryP pr of
    Right entry ->
      case runARM entry pr of
        Left err -> putStrLn err
        Right v  -> print v
    Left err -> putStrLn err


-- find entry point
fEntryP :: [FDefinition] -> ARMMonad FApp
fEntryP (pr:prs) =
  case pr of
    NEntry _ _ -> fEntryP prs
    Entry n _  -> Right $ FApp n []
fEntryP [] = Left "No functions founded"


-- find function definition
fFunD :: FName -> Program -> Maybe [Sentence]
fFunD n [] = Nothing
fFunD n (Entry fn sen:xpr) =
  if n == fn
    then Just sen
    else fFunD n xpr
fFunD n (NEntry fn sen:xpr) =
  if n == fn
    then Just sen
    else fFunD n xpr


-- trying to match whole function
match :: Pattern -> [Sentence] -> ARMMonad FExpr
match [] (Stc _ _ rs:_) = Right rs
match f [] = Left "recognition impossible"
match ex (Stc pat _ rs:xs) =
  case mOne ex pat [] of
    Nothing  -> match ex xs
    Just con -> replace rs con
    -- matching one sentence with pattern.
  where
    mOne :: Pattern -> Pattern -> Conformity -> Maybe Conformity
    mOne [] [] con = Just con
    mOne [] _ _ = Nothing
    mOne _ [] _ = Nothing
    -- splitting into 2 branches
    mOne ex pat con =
      case (a, b)
        -- two parts are successfully matched => main part matched
            of
        (Just con, Just noc) -> Just $ nub $ con ++ noc
        -- one or more parts fails => match failed
        _                    -> Nothing
      where
        (a, b)
          -- if sym in pattern, it should be in expression
          {-
          let pattern = 0 s.n 'a'
            when in matching expression should be 0 and 'a'
            (never matches expressions without syms 0 and 'a')
          -}
         =
          case findSym ([], pat) of
            Just (pat1, tap1, sym) -> split ex (pat1, tap1) sym
            -- if there is no syms ('a' or 0 or "asd") in expression
            Nothing
              -- next are svars (in pattern)
             ->
              case findVar ([], pat) SVar
                -- if founded one, we should match it with sym in expr (resolve)
                    of
                Just splitPat -> resSVar ex splitPat con {-(pat1, tap1, svar)-}
                -- if exp has no syms and svars, searching for tvars
                Nothing
                   -- same as with tvars, but resolve is different.
                 ->
                  case findVar (pat, []) TVar of
                    Just splitPat -> resTVar ex splitPat con {-(pat1, tap1, tvar)-}
                    Nothing
                      -- at last resolve all evars, only they are in expr now
                     ->
                      case findVar ([], pat) EVar of
                        Just (pat1, tap1, evar)
                          -- if already in conformity, then split by it
                         ->
                          case findSpecTerm (ex, []) (Var evar) of
                            Just (ex1, xe1, terms) ->
                              split ex (pat1, tap1) terms
                            -- if its new evar, then resolve it
                            Nothing ->
                              resEVar
                                ex
                                (pat1, tap1, con)
                                evar
                                (genAll ex (length ex) [])
                        Nothing -> (Just [], Just [])
        -- tvar is term in (). If tvar exist in pattern, it should be in expr
        resTVar ::
             Pattern
          -> (Pattern, Pattern, Var)
          -> Conformity
          -> (Maybe Conformity, Maybe Conformity)
        resTVar ex (pat1, tap1, tvar) con
          -- tvar might be already matched, finding in conformity
         =
          case findV tvar con
            -- if founded, just split
                of
            Just [Par p] -> split ex (pat1, tap1) (Par p)
            Nothing
              {- else finding first () in expr. If found,
                looking on result of two parts matched with given tvar in con -}
             ->
              case findSpecTerm (ex, []) (Var tvar) of
                Just (ex1, xe1, par)
                  -- matching first part
                 ->
                  case mOne ex1 pat1 ((tvar, [par]) : con) of
                    Nothing -> (Nothing, Nothing)
                    Just con1
                      -- matching second part
                     ->
                      case mOne ex1 pat1 con1
                        -- success
                            of
                        Just con2 -> (Just con1, Just con2)
                        {- if one of parts fails => tvar not matched,
                          expr doesnt fit -}
                        Nothing   -> (Nothing, Nothing)
                Nothing -> (Nothing, Nothing)
        {- same as resolving tvar, but svar is not bounded, so
           failing on given sym => taking other and so on. Fails
           then they are ended -}
        resSVar ::
             Pattern
          -> (Pattern, Pattern, Var)
          -> Conformity
          -> (Maybe Conformity, Maybe Conformity)
        resSVar ex (pat1, tap1, svar) con =
          case findV svar con of
            Just [Sym s] -> split ex (pat1, tap1) (Sym s)
            Nothing ->
              case mOne [] pat1 ((svar, [head ex]) : con) of
                Just con1 ->
                  case mOne (tail ex) tap1 con1 of
                    Nothing ->
                      (mOne ex pat ((svar, [head $ tail ex]) : con), Just [])
                    Just con2 -> (Just con1, Just con2)
                Nothing ->
                  (mOne ex pat ((svar, [head $ tail ex]) : con), Just [])
        {- Also similar. Given generated possible value of evar as last arg,
           trying match with them in order of rising length -}
        resEVar ::
             Pattern
          -> (Pattern, Pattern, Conformity)
          -> Var
          -> [(Pattern, Pattern, [Term])]
          -> (Maybe Conformity, Maybe Conformity)
        resEVar mainEx (pat1, pat2, con) evar ((ex, xe, terms):all) =
          case mOne ex pat1 ((evar, terms) : con) of
            Nothing -> resEVar mainEx (pat1, pat2, con) evar all
            Just con1 ->
              case mOne xe pat1 con1 of
                Just con2 -> (Just con1, Just con2)
                Nothing   -> resEVar mainEx (pat1, pat2, con) evar all
        {- Generates all possible evar values (all lengths) in given expression (with
          split by it value
          -}
        genAll ::
             Pattern
          -> Int
          -> [(Pattern, Pattern, [Term])]
          -> [(Pattern, Pattern, [Term])]
        genAll ex 0 acc   = genOne [] ex 0 ++ acc
        genAll ex int acc = genAll ex (int - 1) (genOne [] ex int ++ acc)
        {- Generates all subsequences of given length with split -}
        genOne :: Pattern -> Pattern -> Int -> [(Pattern, Pattern, [Term])]
        genOne ex [] _ = []
        genOne ex xe int =
          if length xe < int
            then []
            else (reverse ex, drop int xe, take int xe) :
                 genOne (head xe : ex) (tail xe) int
        {- splitting function. Takes expression to split with two parts
          of pattern to match with split and term, that should be in
          expression. -}
        split ::
             Pattern
          -> (Pattern, Pattern)
          -> Term
          -> (Maybe Conformity, Maybe Conformity)
        split ex (pat1, pat2) t =
          case findSpecTerm ([], ex) t
            -- If there is no sym => no matching
                of
            Nothing            -> (Nothing, Nothing)
            -- Found one => matched => split
            Just (ex1, xe1, _) -> (mOne ex1 pat1 con, mOne xe1 pat2 con)
        findSym :: (Pattern, Pattern) -> Maybe (Pattern, Pattern, Term)
        findSym (_, [])          = Nothing
        findSym (pat, Sym p:tap) = Just (reverse pat, tap, Sym p)
        findSym (pat, p:tap)     = findSym (p : pat, tap)
        {- finding term in expression, and provides split by this term
          expression
          if we want to find 0 in expr '1' 0 'a', the result be ('1','a',0)
          if there is no term => nothing
          -}
        findSpecTerm ::
             (Pattern, Pattern) -> Term -> Maybe (Pattern, Pattern, Term)
        findSpecTerm (_, []) _ = Nothing
        findSpecTerm (xe, e:ex) term =
          if term == e
            then Just (reverse xe, ex, term)
            else findSpecTerm (e : xe, ex) term
        {- finding var (s, t or e) in pattern also provide two parts of
          pattern split by this var. Nothing in case of no 's' var in pattern
          ( or 't' or 'e', it takes constructor of needed var as scn arg )
          -}
        findVar ::
             (Pattern, Pattern)
          -> (String -> Var)
          -> Maybe (Pattern, Pattern, Var)
        findVar (_, []) _ = Nothing
        findVar (pat, Var p:tap) varT =
          if isVar varT p
            then Just (reverse pat, tap, p)
            else findVar (Var p : pat, tap) varT
          where
            isVar :: (String -> Var) -> Var -> Bool
            isVar typ var =
              case (typ "", var) of
                (SVar _, SVar _) -> True
                (TVar _, TVar _) -> True
                (EVar _, EVar _) -> True
                _                -> False
    -- if match succeed, substituting rside using conformity
    replace :: RSide -> Conformity -> ARMMonad FExpr
    replace [] con = Right []
    replace (f:fex) con =
      case replace fex con of
        Right repl ->
          case f of
            (Term (Var t)) ->
              case findV t con of
                Nothing   -> Left "No replacement found"
                Just term -> Right $ map Term term ++ repl
            (Term s) -> Right $ Term s : repl
            (FAct (FApp n ex)) ->
              case replace ex con of
                Right appRepl -> Right $ FAct (FApp n appRepl) : repl
                Left err      -> Left err
        Left err -> Left err
    -- searching value of var in conformity. Nothing if no such var
    findV :: Var -> Conformity -> Maybe Pattern
    findV g [] = Nothing
    findV v (x:con) =
      if v == fst x
        then Just (snd x)
        else findV v con


-- Check for absence of function expressions
isExp :: FExpr -> Bool
isExp fex =
  case fex of
    []           -> True
    (Term _):fex -> isExp fex
    (FAct _):_   -> False


-- Iteration of Abstract Refal Machine
rIter :: FExpr -> Program -> ARMMonad FExpr
rIter [] _ = Right []
rIter (f:fex) pr =
  case rIter fex pr of
    Right rit ->
      case f of
        Term t -> Right $ f : rit
        FAct a ->
          case runARM a pr of
            Right ex -> Right $ ex ++ rit
            Left err -> Left err
    Left err -> Left err


--main runARM of ARM
runARM :: FApp -> Program -> ARMMonad FExpr
runARM (FApp n fex) pr =
  if isExp fex
    then case calcFun (map (\(Term x) -> x) fex) n pr of
           Right calc -> rIter calc pr
           Left e     -> Left e
    else case replFirstApp fex of
           Right repl -> runARM (FApp n repl) pr
           Left e     -> Left e
  where
    replFirstApp :: FExpr -> ARMMonad FExpr
    replFirstApp fex =
      case fex of
        [] -> Right []
        (Term t):fex ->
          case replFirstApp fex of
            Right repl -> Right $ Term t : repl
            Left e     -> Left e
        (FAct (FApp n args)):fex ->
          if isExp args
            then case calcFun (map (\(Term x) -> x) args) n pr of
                   Right calc -> Right $ calc ++ fex
                   Left e     -> Left e
            else case replFirstApp args of
                   Right repl -> Right $ FAct (FApp n repl) : fex
                   Left e     -> Left e

calcFun :: Pattern -> FName -> Program -> ARMMonad FExpr
calcFun pat name pr =
  case fFunD name pr of
    Nothing    -> Right $ descriptor name pat
    (Just snt) -> match pat snt
