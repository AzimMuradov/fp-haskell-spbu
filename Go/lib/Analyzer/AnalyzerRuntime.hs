{-# LANGUAGE OverloadedStrings #-}

-- TODO : Docs
module Analyzer.AnalyzerRuntime where

import Analyzer.AnalysisResult
import qualified Analyzer.AnalyzedAst as AAst
import qualified Analyzer.AnalyzedType as AType
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.State (get, put)
import Data.Map ((!?))
import qualified Data.Map as Map

-- ** Get a variable type

-- TODO : Docs
getVarType :: AAst.Identifier -> Result AType.Type
getVarType name = do
  env <- get
  maybe (throwError IdentifierNotFound) (return . fst) (searchVar name env)

-- ** Add a new variable

-- TODO : Docs
addNewVar :: AAst.Identifier -> AType.Type -> Result ()
addNewVar name t = do
  env <- get
  either throwError put (addNewVar' name t env)

-- TODO : Docs
addNewVar' :: AAst.Identifier -> AType.Type -> Env -> ResultValue Env
addNewVar' _ _ (Env []) = Left UnexpectedError
addNewVar' _ _ (Env [_]) = Left UnexpectedError
addNewVar' n t (Env (Scope scT ns : scs)) = case ns !? n of
  Just _ -> Left IdentifierRedeclaration
  Nothing -> Right $ Env $ Scope scT (Map.insert n t ns) : scs

-- ** Add or update a variable

-- TODO : Docs
addOrUpdateVar :: AAst.Identifier -> AType.Type -> Result ()
addOrUpdateVar name t = do
  env <- get
  case searchVar name env of
    Just (_, Curr) -> updateVar name t
    _ -> addNewVar name t

-- ** Update a variable

-- TODO : Docs
updateVar :: AAst.Identifier -> AType.Type -> Result ()
updateVar name t = do
  env <- get
  case searchVar name env of
    Just (t', _) | t == t' -> return ()
    Just _ -> throwError MismatchedTypes
    Nothing -> throwError IdentifierNotFound

-- ** Search for a variable

-- TODO : Docs
searchVar :: AAst.Identifier -> Env -> Maybe (AType.Type, ScopeLocation)
searchVar name (Env scs) = searchVar' name scs Curr
  where
    searchVar' n (Scope _ ns : outerScs) loc = case ns !? n of
      Just t -> Just (t, loc)
      Nothing -> searchVar' n outerScs Outer
    searchVar' _ _ _ = Nothing

-- TODO : Docs
data ScopeLocation = Curr | Outer

-- TODO : Docs
getTypeDefault :: AType.Type -> AAst.Expression
getTypeDefault t = AAst.ExprValue $ case t of
  AType.TInt -> AAst.ValInt 0
  AType.TBool -> AAst.ValBool False
  AType.TString -> AAst.ValString ""
  AType.TArray elT len -> AAst.ValArray $ replicate len (getTypeDefault elT)
  AType.TFunction _ -> AAst.ValFunction AAst.Nil
  AType.TNil -> AAst.ValFunction AAst.Nil

-- ** Scopes manipulation

-- TODO : Docs
pushScope :: Scope -> Env -> Env
pushScope initScope (Env scs) = Env $ initScope : scs

-- TODO : Docs
popScope :: Env -> Env
popScope (Env (_ : scs)) = Env scs
popScope env = env

-- TODO : Docs
getCurrScopeType :: Env -> ResultValue ScopeType
getCurrScopeType (Env (Scope scT _ : _)) = Right scT
getCurrScopeType _ = Left UnexpectedError
