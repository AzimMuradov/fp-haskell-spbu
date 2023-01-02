{-# LANGUAGE TemplateHaskell #-}

-- TODO : Docs
module Interpreter.InterpretationResult where

import Analyzer.AnalyzedAst (Function, Identifier)
import Control.Lens (At (at), LensLike', ix, makeLenses)
import Control.Monad.Except (ExceptT)
import Control.Monad.ST (ST)
import Control.Monad.State (MonadTrans (lift), StateT)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.STRef (STRef)
import Data.Text (Text, unpack)
import Interpreter.RuntimeValue (RuntimeValue)

-- Interpretation result

-- * Result

-- | Represents the result of interpretation.
type Result s a = ExceptT Err (StateT (Env s) (ST s)) a

-- TODO : Docs
lift2 :: ST s a -> Result s a
lift2 = lift . lift

-- ** Result value

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

-- ** State

-- TODO : Docs
data Env s = Env
  { _funcs :: Map Identifier (STRef s Function),
    _funcScopes :: [FuncScope s],
    _accumulatedOutput :: AccOut
  }

-- TODO : Docs
emptyEnv :: Env s
emptyEnv = Env Map.empty [] []

-- TODO : Docs
newtype FuncScope s = FuncScope {_scopes :: [Scope s]}

-- | Scope contains identifiers mapped to their types.
newtype Scope s = Scope {_vars :: Map Identifier (STRef s RuntimeValue)}

-- | Create empty @Scope@.
emptyScope :: Scope s
emptyScope = Scope Map.empty

-- | Accumulated out (every element is a text printed to the stdout).
type AccOut = [Text]

-- *** Optics

makeLenses ''Env
makeLenses ''FuncScope
makeLenses ''Scope

-- TODO : Docs
var :: Applicative f => Int -> Int -> Text -> LensLike' f (Env s) (Maybe (STRef s RuntimeValue))
var i j name = funcScopes . ix i . scopes . ix j . vars . at name

-- ** Pure state

-- TODO : Docs
data Env' = Env'
  { _funcs' :: Map Identifier Function,
    _funcScopes' :: [FuncScope'],
    _accumulatedOutput' :: AccOut
  }
  deriving (Show)

-- TODO : Docs
newtype FuncScope' = FuncScope' {_scopes' :: [Scope']}
  deriving (Show)

-- TODO : Docs
newtype Scope' = Scope' {_vars' :: Map Identifier RuntimeValue}
  deriving (Show)

-- *** Optics

makeLenses ''Env'
makeLenses ''FuncScope'
makeLenses ''Scope'
