{-# LANGUAGE OverloadedStrings #-}

module Analyzer.AnalyzerRuntime where

import Analyzer.AnalysisResult
import qualified Analyzer.AnalyzedAst as AAst
import qualified Analyzer.AnalyzedType as AType
import Control.Monad.State (get, put)
import Data.Map ((!?))
import qualified Data.Map as Map
import qualified Parser.Ast as Ast

-- TODO : Support globals

-- ** Get a variable type

getVarType :: Result Ast.Identifier -> Result AType.Type
getVarType res = do
  name <- res
  env <- get
  case searchVar name env of
    Just (t, _) -> return t
    _ -> throw UnexpectedError

-- ** Add a new variable

addNewVar :: Result ScopeEntry -> Result ()
addNewVar res = do
  entry <- res
  env <- get
  maybe (throw IdentifierRedeclaration) put (addNewVar' entry env)

addNewVar' :: ScopeEntry -> Env -> Maybe Env
addNewVar' (name, t) (Env gSc funcScs) = case funcScs of
  (FuncScope (Scope names : scs) : rFuncScs) -> case names !? name of
    Just _ -> Nothing
    Nothing -> Just (Env gSc (FuncScope (Scope (Map.insert name t names) : scs) : rFuncScs))
  (FuncScope [] : rFuncScs) -> Just $ Env gSc (FuncScope [Scope $ Map.singleton name t] : rFuncScs)
  [] -> Just $ Env gSc [FuncScope [Scope $ Map.singleton name t]]

-- ** Add or update a variable

addOrUpdateVar :: Result ScopeEntry -> Result ()
addOrUpdateVar res = do
  (name, _) <- res
  env <- get
  case searchVar name env of
    -- If found in this scope, update the variable
    Just (_, ([], _, _)) -> updateVar res
    -- Otherwise add new variable
    _ -> addNewVar res

-- ** Update a variable

updateVar :: Result ScopeEntry -> Result ()
updateVar res = do
  (name, t) <- res
  env <- get
  case searchVar name env of
    Just (t', _) | t == t' -> return ()
    Just _ -> throw MismatchedTypes
    Nothing -> throw IdentifierNotFound

-- ** Search for a variable

searchVar :: AAst.Identifier -> Env -> Maybe (AType.Type, ([Scope], Scope, [Scope]))
searchVar name (Env gSc funcScs) = case funcScs of
  FuncScope (sc : scs) : _ -> searchVar' name [] sc (scs ++ [gSc])
  FuncScope [] : _ -> searchVar' name [] gSc []
  _ -> searchVar' name [] gSc []
  where
    searchVar' :: AAst.Identifier -> [Scope] -> Scope -> [Scope] -> Maybe (AType.Type, ([Scope], Scope, [Scope]))
    searchVar' n leftScs sc@(Scope ns) rightScs@(rSc : rScs) = case ns !? n of
      Just t -> Just (t, (reverse leftScs, sc, rightScs))
      Nothing -> searchVar' n (sc : leftScs) rSc rScs
    searchVar' n leftScs sc@(Scope ns) [] = case ns !? n of
      Just t -> Just (t, (reverse leftScs, sc, []))
      Nothing -> Nothing

getTypeDefault :: AType.Type -> AAst.Expression
getTypeDefault t = AAst.ExprValue $ case t of
  AType.TInt -> AAst.ValInt 0
  AType.TBool -> AAst.ValBool False
  AType.TString -> AAst.ValString ""
  AType.TArray elT len -> AAst.ValArray $ replicate len (getTypeDefault elT)
  AType.TFunction _ -> AAst.ValFunction AAst.Nil
  AType.TNil -> AAst.ValFunction AAst.Nil
