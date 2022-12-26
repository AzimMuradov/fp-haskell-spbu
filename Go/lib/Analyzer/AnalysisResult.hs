-- | This module contains types and functions needed for representing analysis result.
module Analyzer.AnalysisResult where

import qualified Analyzer.AnalyzedAst as AnalyzedAst
import qualified Analyzer.AnalyzedType as AnalyzedType
import Control.Monad.Except (ExceptT)
import Control.Monad.State (State)
import Data.Map (Map)
import qualified Data.Map as Map

-- Program checker result

-- * Result

-- | Represents the result of checking.
type Result a = ExceptT Err (State Env) a

-- ** State

-- *** Environment

-- | Analyzer environment.
newtype Env = Env {scopes :: [Scope]}
  deriving (Show)

-- | Create empty environment.
emptyEnv :: Env
emptyEnv = Env []

-- *** Scope

-- | Scope contains identifiers mapped to their types.
data Scope = Scope
  { scopeType :: ScopeType,
    names :: Map AnalyzedAst.Identifier AnalyzedType.Type
  }
  deriving (Show)

-- | @Scope@ type.
data ScopeType = ForScope | OrdinaryScope
  deriving (Show, Eq)

-- | Create @Scope@ from its type and elements.
scope :: ScopeType -> [(AnalyzedAst.Identifier, AnalyzedType.Type)] -> Scope
scope t elements = Scope t (Map.fromList elements)

-- | Create empty @Scope@ from its type.
emptyScope :: ScopeType -> Scope
emptyScope t = scope t []

-- ** Result Value

-- | @Result@ value (in other words, it is stateless @Result@).
type ResultValue = Either Err

-- ** Error

-- | Represents an unsuccessful analysis.
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
    DivisionByZero
  | -- | @break@ or @continue@ statement used outside of @ForScope@.
    BreakOrContinueOutsideOfForScope
  | -- | Unexpected error, this type of errors must never happen.
    UnexpectedError
  deriving (Show, Eq)
