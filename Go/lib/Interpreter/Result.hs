{-# LANGUAGE TemplateHaskell #-}

module Interpreter.Result where

import Analyzer.AnalyzedAst (Function (OrdinaryFunction, StdLibFunction), FunctionValue (Function, Nil), Identifier)
import Control.Lens (At (at), LensLike', ix, makeLenses, (^.))
import Control.Monad.Except (ExceptT)
import Control.Monad.ST (ST)
import Control.Monad.State (MonadTrans (lift), StateT)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.STRef (STRef, readSTRef)
import Data.Text (Text, unpack)

-- * Interpretation result

-- | Represents the result of interpretation.
type Result s a = ExceptT Err (StateT (Env s) (ST s)) a

lift2 :: ST s a -> Result s a
lift2 = lift . lift

-- ** Result value

type ResultValue = Either Err

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
  | -- | Panic error (happens when calling `panic` stdlib function).
    Panic Text
  | -- | Unexpected error, this type of errors must never happen.
    UnexpectedError

instance Show Err where
  show DivisionByZero = "panic: runtime error: integer divide by zero"
  show (IndexOutOfRange i len) = "panic: runtime error: index out of range [" <> show i <> "] with length " <> show len
  show NoReturn = "panic: runtime error: no return"
  show Npe = "panic: runtime error: nil pointer dereference"
  show (Panic msg) = "panic: " <> unpack msg
  show UnexpectedError = "panic: unexpected error"

-- ** State

-- *** Environment

data Env s = Env
  { _funcs :: Map Identifier (STRef s Function, Scope s),
    _funcScopes :: [FuncScope s],
    _accumulatedOutput :: AccOut
  }

-- | Accumulated output (every element is a text printed to the stdout).
type AccOut = [Text]

emptyEnv :: Env s
emptyEnv = Env Map.empty [] []

-- *** Function scope

newtype FuncScope s = FuncScope {_scopes :: [Scope s]}

-- *** Scope

-- | Scope contains identifiers mapped to their types.
newtype Scope s = Scope {_vars :: Map Identifier (STRef s (RuntimeValue s))}

-- | Create empty 'Scope'.
emptyScope :: Scope s
emptyScope = Scope Map.empty

-- *** Runtime value

-- | Represents runtime value of the calculated expression.
data RuntimeValue s
  = -- | Int value.
    ValInt Int
  | -- | Boolean value.
    ValBool Bool
  | -- | String value.
    ValString Text
  | -- | Array value.
    ValArray [RuntimeValue s]
  | -- | Function value.
    ValFunction FunctionValue (Scope s)

instance Eq (RuntimeValue s) where
  ValInt lhs == ValInt rhs = lhs == rhs
  ValBool lhs == ValBool rhs = lhs == rhs
  ValString lhs == ValString rhs = lhs == rhs
  ValArray lhs == ValArray rhs = lhs == rhs
  ValFunction _ _ == ValFunction _ _ = False
  _ == _ = False

-- *** Optics

makeLenses ''Env
makeLenses ''FuncScope
makeLenses ''Scope

var :: Applicative f => Int -> Int -> Identifier -> LensLike' f (Env s) (Maybe (STRef s (RuntimeValue s)))
var i j name = funcScopes . ix i . scopes . ix j . vars . at name

-- ** Evaluated state

-- *** Environment

data Env' = Env'
  { _funcs' :: Map Identifier Function,
    _funcScopes' :: [FuncScope'],
    _accumulatedOutput' :: AccOut
  }
  deriving (Show)

evalEnv :: Env s -> ST s Env'
evalEnv (Env fs fScs accOut) = do
  fs' <- mapM (readSTRef . fst) fs
  fScs' <- mapM evalFuncScope fScs
  return $ Env' fs' fScs' accOut

-- *** Function scope

newtype FuncScope' = FuncScope' {_scopes' :: [Scope']}
  deriving (Show)

evalFuncScope :: FuncScope s -> ST s FuncScope'
evalFuncScope (FuncScope scs) = FuncScope' <$> mapM evalScope scs

-- *** Scope

newtype Scope' = Scope' {_vars' :: Map Identifier RuntimeValue'}
  deriving (Show)

evalScope :: Scope s -> ST s Scope'
evalScope sc = Scope' <$> mapM (fmap evalRuntimeValue . readSTRef) (sc ^. vars)

-- *** Runtime value

-- | Represents runtime value of the calculated expression.
data RuntimeValue'
  = -- | Int value.
    ValInt' Int
  | -- | Boolean value.
    ValBool' Bool
  | -- | String value.
    ValString' Text
  | -- | Array value.
    ValArray' [RuntimeValue']
  | -- | Function value.
    ValFunction' FunctionValue

instance Show RuntimeValue' where
  show (ValInt' int) = show int
  show (ValBool' bool) = if bool then "true" else "false"
  show (ValString' string) = unpack string
  show (ValArray' vs) = "[" <> unwords (show <$> vs) <> "]"
  show (ValFunction' (Function OrdinaryFunction {})) = "function"
  show (ValFunction' (Function (StdLibFunction name))) = unpack name
  show (ValFunction' Nil) = "nil"

evalRuntimeValue :: RuntimeValue s -> RuntimeValue'
evalRuntimeValue rv = case rv of
  ValInt v -> ValInt' v
  ValBool v -> ValBool' v
  ValString v -> ValString' v
  ValArray v -> ValArray' $ evalRuntimeValue <$> v
  ValFunction v _ -> ValFunction' v

unevalRuntimeValue :: RuntimeValue' -> RuntimeValue s
unevalRuntimeValue rv = case rv of
  ValInt' v -> ValInt v
  ValBool' v -> ValBool v
  ValString' v -> ValString v
  ValArray' v -> ValArray $ unevalRuntimeValue <$> v
  ValFunction' v -> ValFunction v emptyScope

-- *** Optics

makeLenses ''Env'
makeLenses ''FuncScope'
makeLenses ''Scope'
