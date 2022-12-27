{-# LANGUAGE TupleSections #-}

-- TODO : Docs
module Interpreter.InterpreterRuntime where

import qualified Analyzer.AnalyzedAst as Ast
import Control.Applicative ((<|>))
import Control.Lens
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.State (get, modify, put)
import Data.Map ((!?))
import qualified Data.Map as Map
import Interpreter.InterpretationResult
import Interpreter.RuntimeValue

-- ** Get a variable value

-- TODO : Docs
getVarValue :: Ast.Identifier -> Result RuntimeValue
getVarValue name = fst <$> (get >>= unwrapJust . searchVar name)

-- ** Add a new variable

-- TODO : Docs
addNewVar :: Ast.Identifier -> RuntimeValue -> Result ()
addNewVar name value = modify $ var 0 0 name ?~ value

-- ** Add or update a variable

-- TODO : Docs
addOrUpdateVar :: Ast.Identifier -> RuntimeValue -> Result ()
addOrUpdateVar name value = do
  env <- get
  case searchVar name env of
    Just (_, Curr) -> updateVar name value
    _ -> addNewVar name value

-- ** Update a variable

-- TODO : Docs
updateVar :: Ast.Identifier -> RuntimeValue -> Result ()
updateVar name value = get >>= (unwrapJust . varUpdater name) >>= (\updater -> put $ updater value)

-- ** Search for a variable

-- TODO : Docs
searchVar :: Ast.Identifier -> Env -> Maybe (RuntimeValue, ScopeLocation)
searchVar name (Env fs fScs _) = case fScs of
  FuncScope scs : _ -> searchVar' name Curr (scs ++ [fsScope])
  _ -> searchVar' name Outer [fsScope]
  where
    searchVar' n loc (Scope ns : scs) = ((,loc) <$> (ns !? n)) <|> searchVar' n Outer scs
    searchVar' _ _ _ = Nothing

    fsScope = Scope $ Map.map (ValFunction . Ast.AnonymousFunction) fs

-- TODO : Docs
varUpdater :: Ast.Identifier -> Env -> Maybe (RuntimeValue -> Env)
varUpdater name env@(Env _ fScs _) = case fScs of
  FuncScope scs : _ -> update <$> findScopeIndex name 0 scs
  _ -> Nothing
  where
    update i v = env & var 0 i name ?~ v

    findScopeIndex n i (Scope ns : scs) = (i <$ (ns !? n)) <|> findScopeIndex n (i + 1) scs
    findScopeIndex _ _ _ = Nothing

-- TODO : Docs
data ScopeLocation = Curr | Outer

-- ** Scopes manipulation

-- TODO : Docs
pushFuncScope :: Scope -> Env -> Env
pushFuncScope initScope = funcScopes %~ (FuncScope [initScope] :)

-- TODO : Docs
popFuncScope :: Env -> Env
popFuncScope = funcScopes %~ tail

-- TODO : Docs
pushBlockScope :: Scope -> Env -> Env
pushBlockScope initScope = (funcScopes . ix 0 . scopes) %~ (initScope :)

-- TODO : Docs
popBlockScope :: Env -> Env
popBlockScope = (funcScopes . ix 0 . scopes) %~ tail

-- * Utils

-- TODO : Docs
unwrapJust :: Maybe a -> Result a
unwrapJust = maybe (throwError UnexpectedError) return
