{-# LANGUAGE TemplateHaskell #-}

-- TODO : Docs
module Interpreter.InterpretationResult where

import Analyzer.AnalyzedAst (Function, Identifier)
import Control.Lens (At (at), ix, makeLenses)
import Control.Monad.Except (ExceptT)
import Control.Monad.State (State)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text, unpack)
import Interpreter.RuntimeValue (RuntimeValue)

-- Interpretation result

-- * Result

-- | Represents the result of interpretation.
type Result a = ExceptT Err (State Env) a

-- ** State

-- TODO : Docs
data Env = Env
  { _funcs :: Map Identifier Function,
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
newtype Scope = Scope {_vars :: Map Identifier RuntimeValue}
  deriving (Show)

-- | Create empty @Scope@.
emptyScope :: Scope
emptyScope = Scope Map.empty

-- | Accumulated out (every element is a text printed to the stdout).
type AccOut = [Text]

-- ** Result Value

-- TODO : Docs
type ResultValue = Either Err

-- ** Error

-- | Represents unsuccessful interpretation.
data Err
  = -- | Division by zero error.
    DivisionByZero
  | -- | Index out of bounds error, contains info about index and array length.
    IndexOutOfRange Int Int
  | -- | No return error.
    NoReturn
  | -- | Null dereference error (happens when trying to call the `nil` as a regular function).
    Npe
  | -- TODO : Docs
    Panic Text
  | -- | Unexpected error, this type of errors must never happen.
    UnexpectedError

instance Show Err where
  show DivisionByZero = "panic: runtime error: integer divide by zero"
  show (IndexOutOfRange i len) = "panic: runtime error: index out of range [" ++ show i ++ "] with length " ++ show len
  show NoReturn = "panic: runtime error: no return"
  show Npe = "panic: runtime error: nil pointer dereference"
  show (Panic msg) = "panic: " ++ unpack msg
  show UnexpectedError = "panic: unexpected error"

-- Optics

makeLenses ''Env
makeLenses ''FuncScope
makeLenses ''Scope

var :: Applicative f => Int -> Int -> Identifier -> (Maybe RuntimeValue -> f (Maybe RuntimeValue)) -> Env -> f Env
var i j name = funcScopes . ix i . scopes . ix j . vars . at name
