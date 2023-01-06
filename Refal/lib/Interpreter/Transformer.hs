module Transformer where

import           AST

transform :: Program -> Program
transform = map trFun

trFun :: FDefinition -> FDefinition
trFun (Entry name block)  = Entry name (trBlock block)
trFun (NEntry name block) = NEntry name (trBlock block)

trBlock :: [Sentence] -> [Sentence]
trBlock = map trStc

trStc :: Sentence -> Sentence
trStc (Stc ls c rs) = Stc  ls c (trFexpr rs)

trFexpr :: FExpr -> FExpr
trFexpr = map trFterm

trFterm :: FTerm -> FTerm
trFterm ft =
  case ft of
    Term t   -> ft
    FAct fap -> trFApp fap

trFApp :: FApp -> FTerm
trFApp (FApp name args) =
  case map trFterm args of
    trArgs ->
      if isFit trArgs 
        then case name of
               "Mul" -> bin (*) trArgs
               "Sub" -> bin (-) trArgs
               "Add" -> bin (+) trArgs
               _     -> FAct $ FApp name trArgs
        else FAct $ FApp name trArgs

bin :: (Integer -> Integer -> Integer) -> FExpr -> FTerm
bin op [Term (Sym (MDig a)), Term (Sym (MDig b))] = Term $ Sym $ MDig (op a b)

isFit :: FExpr -> Bool
isFit exp =
  case exp of
    [Term (Sym (MDig a)), Term (Sym (MDig b))] -> True
    _                                          -> False
