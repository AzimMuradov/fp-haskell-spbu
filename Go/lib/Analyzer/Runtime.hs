{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Analyzer.Runtime where

import qualified Analyzer.AnalyzedAst as AAst
import qualified Analyzer.AnalyzedType as AType
import Analyzer.Result
import Control.Applicative ((<|>))
import Control.Lens (ix, (%~), (?~), (^?!))
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.State (get, gets, modify)
import Data.Map ((!?))
import Data.Maybe (isJust)

-- ** Get a variable type

getVarType :: AAst.Identifier -> Result AType.Type
getVarType name = gets (searchVar name) >>= maybe (throwError $ IdentifierNotFound name) (return . fst)

-- ** Add a new variable

addNewVar :: AAst.Identifier -> AType.Type -> Result ()
addNewVar name t = do
  env <- get
  if isJust (env ^?! var 0 name)
    then throwError $ IdentifierRedeclaration name
    else modify $ var 0 name ?~ t

-- ** Add or update a variable

addOrUpdateVar :: AAst.Identifier -> AType.Type -> Result ()
addOrUpdateVar name t = do
  env <- get
  case searchVar name env of
    Just (_, Curr) -> updateVar name t
    _ -> addNewVar name t

-- ** Update a variable

updateVar :: AAst.Identifier -> AType.Type -> Result ()
updateVar name t = do
  env <- get
  case searchVar name env of
    Just (t', _) | t == t' -> return ()
    Just _ -> throwError MismatchedTypes
    Nothing -> throwError $ IdentifierNotFound name

-- ** Search for a variable

searchVar :: AAst.Identifier -> Env -> Maybe (AType.Type, ScopeLocation)
searchVar name (Env scs) = searchVar' name scs Curr
  where
    searchVar' n (Scope _ ns : outerScs) loc = ((,loc) <$> (ns !? n)) <|> searchVar' n outerScs Outer
    searchVar' _ _ _ = Nothing

data ScopeLocation = Curr | Outer

getTypeDefault :: AType.Type -> AAst.Expression
getTypeDefault t = AAst.ExprValue $ case t of
  AType.TInt -> AAst.ValInt 0
  AType.TBool -> AAst.ValBool False
  AType.TString -> AAst.ValString ""
  AType.TArray elT len -> AAst.ValArray $ replicate len (getTypeDefault elT)
  AType.TFunction _ -> AAst.ValFunction AAst.Nil
  AType.TNil -> AAst.ValFunction AAst.Nil

-- ** Scopes manipulation

pushScope :: Scope -> Env -> Env
pushScope initScope = scopes %~ (initScope :)

popScope :: Env -> Env
popScope = scopes %~ tail

getCurrScopeType :: Env -> ScopeType
getCurrScopeType env = env ^?! (scopes . ix 0 . scopeType)
