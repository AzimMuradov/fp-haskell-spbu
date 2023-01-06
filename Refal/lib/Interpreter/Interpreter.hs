module Interpreter where

import           AST
import           Control.Monad.Except
import           Data.List
import           Data.List.Split
import           Data.Maybe
import           Stdlib

type Congruity = (Var, Pattern)

type Conformity = [Congruity]

type ARMMonad = Either ARMError

type ARMError = String

type SplitMatch = (Maybe Conformity, Maybe Conformity)

data SplitPattern
  = SpVar Pattern Var Pattern
  | SpTerm Pattern Term Pattern
  | SpPat Pattern [Term] Pattern
  deriving (Show)

data FirstFounded
  = Paren SplitPattern
  | Symbol SplitPattern
  | Svar SplitPattern
  | Tvar SplitPattern
  | Evar SplitPattern
  | Empty

type ResolvingFunction = Pattern -> SplitPattern -> Conformity -> SplitMatch

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
match f [] = Left "recognition impossible"
match ex (Stc pat _ rs:xs) =
  case mOne ex pat [] of
    Nothing  -> match ex xs
    Just con -> replace rs con
  where
    mOne :: Pattern -> Pattern -> Conformity -> Maybe Conformity
    mOne [] [] con = Just con
    mOne _ [] _ = Nothing
    mOne ex pat con =
      case (a, b) of
        (Just con, Just noc) -> Just $ nub $ con ++ noc
        _                    -> Nothing
      where
        (a, b) =
          case classifier pat of
            Paren splitP  -> split ex splitP con
            Symbol splitP -> split ex splitP con
            Svar splitP   -> resSVar ex splitP con
            Tvar splitP   -> resTVar ex splitP con
            Evar splitP   -> resEVar ex splitP con
            Empty         -> (Just [], Just [])
        classifier :: Pattern -> FirstFounded
        classifier pat =
          case findPar ([], pat) of
            Just (SpTerm pat1 par tap1) -> Paren (SpTerm pat1 par tap1)
            Nothing ->
              case findSym ([], pat) of
                Just (SpTerm pat1 sym tap1) -> Symbol (SpTerm pat1 sym tap1)
                Nothing ->
                  case findVar ([], pat) SVar of
                    Just splitPat -> Svar splitPat
                    Nothing ->
                      case findVar ([], pat) TVar of
                        Just splitPat -> Tvar splitPat
                        Nothing -> maybe Empty Evar (findVar ([], pat) EVar)
        resTVar :: ResolvingFunction
        resTVar ex (SpVar pat1 tvar tap1) con =
          case findV tvar con of
            Just [Par p] -> split ex (SpTerm pat1 (Par p) tap1) con
            Nothing ->
              case findSpecTerm (ex, []) (Var tvar) of
                Just (SpTerm ex1 par xe1) ->
                  case mOne ex1 pat1 ((tvar, [par]) : con) of
                    Nothing -> (Nothing, Nothing)
                    Just con1 ->
                      case mOne ex1 pat1 con1 of
                        Just con2 -> (Just con1, Just con2)
                        Nothing   -> (Nothing, Nothing)
                Nothing -> (Nothing, Nothing)
        resSVar :: ResolvingFunction
        resSVar ex (SpVar pat1 svar tap1) con =
          case findV svar con of
            Just [Sym s] -> split ex (SpTerm pat1 (Sym s) tap1) con
            Nothing      -> newVar ex (pat1, tap1, con) svar (genOne [] ex 1)
        resEVar :: ResolvingFunction
        resEVar ex (SpVar pat1 evar tap1) con =
          case findV evar con of
            Just terms -> split ex (SpPat pat1 terms tap1) con
            Nothing ->
              newVar ex (pat1, tap1, con) evar (genAll ex (length ex) [])
        newVar ::
             Pattern
          -> (Pattern, Pattern, Conformity)
          -> Var
          -> [SplitPattern]
          -> SplitMatch
        newVar ex a svar [] = (Nothing, Nothing)
        newVar mainEx (pat1, pat2, con) var (SpPat ex term xe:all) =
          branching (ex, xe) (pat1, pat2, (var, term) : con) def
          where
            def = newVar mainEx (pat1, pat2, con) var all
        genAll :: Pattern -> Int -> [SplitPattern] -> [SplitPattern]
        genAll ex 0 acc   = genOne [] ex 0 ++ acc
        genAll ex int acc = genAll ex (int - 1) (genOne [] ex int ++ acc)
        genOne :: Pattern -> Pattern -> Int -> [SplitPattern]
        genOne ex [] 0 = singleton $ SpPat (reverse ex) [] []
        genOne ex [] _ = []
        genOne ex xe int =
          if length xe < int
            then []
            else SpPat (reverse ex) (take int xe) (drop int xe) :
                 genOne (head xe : ex) (tail xe) int
        split :: Pattern -> SplitPattern -> Conformity -> SplitMatch
        split ex (SpTerm pat1 t pat2) con =
          case findSpecTerm ([], ex) t of
            Nothing -> (Nothing, Nothing)
            Just (SpTerm ex1 term xe1) ->
              case term of
                Par par ->
                  case mOne par ((\(Par pat) -> pat) t) con of
                    Just newcon ->
                      branching
                        (ex1, xe1)
                        (pat1, pat2, newcon)
                        (Nothing, Nothing)
                    Nothing -> (Nothing, Nothing)
                _ -> branching (ex1, xe1) (pat1, pat2, con) (Nothing, Nothing)
        split ex (SpPat pat1 pat pat2) con =
          branching
            (head sPat, concat $ tail sPat)
            (pat1, pat2, con)
            (Nothing, Nothing)
          where
            sPat = splitOn pat ex
        findSym :: (Pattern, Pattern) -> Maybe SplitPattern
        findSym (_, [])          = Nothing
        findSym (pat, Sym p:tap) = Just (SpTerm (reverse pat) (Sym p) tap)
        findSym (pat, p:tap)     = findSym (p : pat, tap)
        findPar :: (Pattern, Pattern) -> Maybe SplitPattern
        findPar (_, [])          = Nothing
        findPar (pat, Par p:tap) = Just (SpTerm (reverse pat) (Par p) tap)
        findPar (pat, p:tap)     = findPar (p : pat, tap)
        findSpecTerm :: (Pattern, Pattern) -> Term -> Maybe SplitPattern
        findSpecTerm (_, []) _ = Nothing
        findSpecTerm (xe, e:ex) term =
          if term == e
            then Just (SpTerm (reverse xe) e ex)
            else findSpecTerm (e : xe, ex) term
        findVar :: (Pattern, Pattern) -> (String -> Var) -> Maybe SplitPattern
        findVar (_, []) _ = Nothing
        findVar (pat, Var p:tap) varT =
          if isVar varT p
            then Just (SpVar (reverse pat) p tap)
            else findVar (Var p : pat, tap) varT
          where
            isVar :: (String -> Var) -> Var -> Bool
            isVar typ var =
              case (typ "", var) of
                (SVar _, SVar _) -> True
                (TVar _, TVar _) -> True
                (EVar _, EVar _) -> True
                _                -> False
        branching ::
             (Pattern, Pattern)
          -> (Pattern, Pattern, Conformity)
          -> SplitMatch
          -> SplitMatch
        branching (ex, xe) (pat1, pat2, con) def =
          case mOne ex pat1 con of
            Nothing -> def
            Just con1 ->
              case mOne xe pat2 con1 of
                Just con2 -> (Just con1, Just con2)
                Nothing   -> def
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
            (Term (Par p)) -> Right $ Term (Par $ replPar p con) : repl
            (Term s) -> Right $ Term s : repl
            (FAct (FApp n ex)) ->
              case replace ex con of
                Right appRepl -> Right $ FAct (FApp n appRepl) : repl
                Left err      -> Left err
        Left err -> Left err
    replPar :: Pattern -> Conformity -> Pattern
    replPar [] _ = []
    replPar (Var v:pat) con =
      case findV v con of
        Just term -> term ++ replPar pat con
    replPar (Par p:pat) con = Par (replPar p con) : replPar pat con
    replPar (other:pat) con = other : replPar pat con
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
