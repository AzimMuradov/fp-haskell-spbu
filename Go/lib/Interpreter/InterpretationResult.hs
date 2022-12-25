{-# LANGUAGE TemplateHaskell #-}

-- TODO : Docs
module Interpreter.InterpretationResult where

import qualified Analyzer.AnalyzedAst as Ast
import Control.Lens (ix, makeLenses, makePrisms)
import Control.Monad.State (StateT, lift)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import Interpreter.RuntimeValue

-- Interpretation result

-- * Result

-- | Represents the result of interpretation.
type Result a = StateT Env ResultValue a

-- ** State

-- TODO : Docs
data Env = Env
  { _funcs :: Map Ast.Identifier Ast.Function,
    _funcScopes :: [FuncScope],
    _accumulatedOutput :: AccOut
  }
  deriving (Show)

-- TODO : Docs
emptyEnv :: Env
emptyEnv = Env Map.empty [] []

-- TODO : Docs
newtype FuncScope = FuncScope {_scopes :: [Scope]}
  deriving (Show)

-- | Scope contains identifiers mapped to their types.
newtype Scope = Scope {_vars :: Map Ast.Identifier RuntimeValue}
  deriving (Show)

-- | Create @Scope@ from its elements.
scope :: [(Ast.Identifier, RuntimeValue)] -> Scope
scope elements = Scope (Map.fromList elements)

-- | Create empty @Scope@.
emptyScope :: Scope
emptyScope = scope []

-- ** Result Value

-- TODO : Docs
type ResultValue = Either Err

-- ** Error

-- | Represents unsuccessful interpretation.
data Err
  = -- | Division by zero error.
    DivisionByZero
  | -- | Index out of bounds error.
    IndexOutOfBounds
  | -- | No return error.
    NoReturn
  | -- | Null dereference error (happens when trying to call the `nil` as a regular function).
    Npe
  | -- TODO : Docs
    Panicked Text
  | -- | Unexpected error, this type of errors must never happen.
    UnexpectedError
  deriving (Show)

-- | Accumulated out (every element is a text printed to the stdout).
type AccOut = [Text]

-- | Statement interpretation result, its either unit (`void`) or some result of the return statement.
data StmtResult = Unit | Ret (Maybe RuntimeValue)
  deriving (Show, Eq)

-- TODO : Docs
throw :: Err -> Result a
throw err = lift $ Left err

-- Optics

makeLenses ''Env
makeLenses ''FuncScope
makeLenses ''Scope

makePrisms ''FuncScope
makePrisms ''Scope

var :: Applicative f => Int -> Int -> Ast.Identifier -> (RuntimeValue -> f RuntimeValue) -> Env -> f Env
var i j name = funcScopes . ix i . scopes . ix j . vars . ix name
