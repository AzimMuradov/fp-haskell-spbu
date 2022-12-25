-- TODO : Docs
{-# LANGUAGE TupleSections #-}

module Interpreter.InterpreterRuntime where

import qualified Analyzer.AnalyzedAst as Ast
import Control.Lens
import Control.Monad.State (get, modify, put)
import Data.Map ((!?))
import qualified Data.Map as Map
import Data.Tuple.Extra (fst3)
import Interpreter.InterpretationResult
import Interpreter.RuntimeValue

-- ** Get a variable value

-- TODO : Docs
getVarValue :: Ast.Identifier -> Result RuntimeValue
getVarValue name = fst3 <$> (get >>= unwrapJust . searchVar name)

-- ** Add a new variable

-- TODO : Docs
addNewVar :: Ast.Identifier -> RuntimeValue -> Result ()
addNewVar name value = modify $ var 0 0 name .~ value

-- ** Add or update a variable

-- TODO : Docs
addOrUpdateVar :: Ast.Identifier -> RuntimeValue -> Result ()
addOrUpdateVar name value = do
  env <- get
  case searchVar name env of
    Just (_, Curr, _) -> updateVar name value
    _ -> addNewVar name value

-- ** Update a variable

-- TODO : Docs
updateVar :: Ast.Identifier -> RuntimeValue -> Result ()
updateVar name value = do
  env <- get
  (_, _, updater) <- unwrapJust $ searchVar name env
  put $ updater value

-- ** Search for a variable

-- TODO : Docs
-- TODO : Make more safe
searchVar :: Ast.Identifier -> Env -> Maybe (RuntimeValue, ScopeLocation, Updater)
searchVar name env@(Env fs fScs _) = case fScs of
  FuncScope (sc : scs) : _ -> searchVar' name 0 sc (scs ++ [fsScope])
  _ -> searchVar' name 1 fsScope []
  where
    searchVar' n i sc outerScs = uncurry searchMapper <$> searchVar'' n i sc outerScs

    searchMapper val i = (val, if i == 0 then Curr else Outer, \v -> env & var 0 i name .~ v)

    searchVar'' n i (Scope ns) (outerSc : outerScs) = case ns !? n of
      Just v -> Just (v, i)
      Nothing -> searchVar'' n (i + 1) outerSc outerScs
    searchVar'' n i (Scope ns) [] = (,i) <$> ns !? n

    fsScope = Scope $ Map.map (ValFunction . Ast.AnonymousFunction) fs

-- TODO : Docs
data ScopeLocation = Curr | Outer

-- TODO : Docs
type Updater = RuntimeValue -> Env

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
unwrapJust = maybe (throw UnexpectedError) return
