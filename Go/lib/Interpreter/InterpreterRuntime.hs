{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Interpreter.InterpreterRuntime where

import qualified Analyzer.AnalyzedAst as Ast
import Control.Applicative ((<|>))
import Control.Lens
import Control.Monad ((>=>))
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.State (get, modify)
import Data.Map ((!?))
import qualified Data.Map.Strict as Map
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef)
import Interpreter.InterpretationResult

-- ** Get a variable value

getVarValue :: Ast.Identifier -> Result s (RuntimeValue s)
getVarValue name = getVar name >>= lift2 . readSTRef

-- ** Add a new variable

addNewVar :: Ast.Identifier -> RuntimeValue s -> Result s ()
addNewVar name value = do
  ref <- lift2 $ newSTRef value
  modify $ var 0 0 name ?~ ref

-- ** Add or update a variable

addOrUpdateVar :: Ast.Identifier -> RuntimeValue s -> Result s ()
addOrUpdateVar name value =
  searchVar name >>= \case
    Just (_, Curr) -> updateVar name value
    _ -> addNewVar name value

-- ** Update a variable

updateVar :: Ast.Identifier -> RuntimeValue s -> Result s ()
updateVar name value = do
  ref <- getVar name
  lift2 (writeSTRef ref value)

-- ** Search for a variable

getVar :: Ast.Identifier -> Result s (STRef s (RuntimeValue s))
getVar name = fst <$> (searchVar name >>= unwrapJust)

searchVar :: Ast.Identifier -> Result s (Maybe (STRef s (RuntimeValue s), ScopeLocation))
searchVar name = do
  Env fs fScs _ <- get
  fsSc <- lift2 $ Scope <$> mapM ((\(f, fSc) -> (,fSc) <$> readSTRef f) >=> \(f, fSc) -> newSTRef $ ValFunction (Ast.AnonymousFunction f) fSc) fs
  return $ case fScs of
    FuncScope scs : _ -> searchVar' name Curr (scs ++ [fsSc])
    _ -> searchVar' name Outer [fsSc]
  where
    searchVar' n loc (Scope ns : scs) = ((,loc) <$> (ns !? n)) <|> searchVar' n Outer scs
    searchVar' _ _ _ = Nothing

data ScopeLocation = Curr | Outer

flattenFuncScope :: FuncScope s -> Scope s
flattenFuncScope (FuncScope scs) = flattenFuncScope' scs
  where
    flattenFuncScope' (Scope ns : Scope ns' : scs') = flattenFuncScope' (Scope (Map.union ns ns') : scs')
    flattenFuncScope' [sc] = sc
    flattenFuncScope' [] = emptyScope

-- ** Scopes manipulation

pushFuncScope :: Scope s -> Env s -> Env s
pushFuncScope initScope = funcScopes %~ (FuncScope [initScope] :)

popFuncScope :: Env s -> Env s
popFuncScope = funcScopes %~ tail

pushBlockScope :: Scope s -> Env s -> Env s
pushBlockScope initScope = (funcScopes . ix 0 . scopes) %~ (initScope :)

popBlockScope :: Env s -> Env s
popBlockScope = (funcScopes . ix 0 . scopes) %~ tail

-- * Utils

unwrapJust :: Maybe a -> Result s a
unwrapJust = maybe (throwError UnexpectedError) return
