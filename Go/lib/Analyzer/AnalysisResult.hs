module Analyzer.AnalysisResult where

import qualified Analyzer.AnalyzedAst as AnalyzedAst
import qualified Analyzer.AnalyzedType as AnalyzedType
import Control.Monad.State (MonadTrans (lift), StateT)
import Data.Map (Map)
import qualified Data.Map as Map

-- Program checker result

-- * Result

-- | Represents the result of checking.
type Result a = StateT Env ResultValue a

-- ** State

data Env = Env {globals :: Scope, funcsScopes :: [FuncScope]}
  deriving (Show)

emptyEnv :: Env
emptyEnv = Env (Scope Map.empty) []

newtype FuncScope = FuncScope {scopes :: [Scope]}
  deriving (Show)

-- | Scope contains identifiers mapped to their types.
newtype Scope = Scope (Map AnalyzedAst.Identifier AnalyzedType.Type)
  deriving (Show)

type ScopeEntry = (AnalyzedAst.Identifier, AnalyzedType.Type)

-- ** Result Value

type ResultValue = Either Err

-- ** Error

-- | Represents unsuccessful analyzing.
data Err
  = -- | No entry point for the interpreter error.
    NoMain
  | -- | Identifier not found error.
    IdentifierNotFound
  | -- | Identifier redeclaration error.
    IdentifierRedeclaration
  | -- | Mismatched types error.
    MismatchedTypes
  | -- | Constant integer expression types not in `int` bounds error.
    NotInIntBounds
  | -- | Division by 0 in constant integer expression error.
    DivByZero
  | -- | Unexpected error, this type of errors must never happen.
    UnexpectedError
  deriving (Show)

throw :: Err -> Result a
throw err = lift $ Left err
