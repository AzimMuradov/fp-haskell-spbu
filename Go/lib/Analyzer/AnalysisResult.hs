{-# LANGUAGE TemplateHaskell #-}

-- | This module contains types and functions needed for representing analysis result.
module Analyzer.AnalysisResult where

import Analyzer.AnalyzedAst (Identifier)
import Analyzer.AnalyzedType (Type)
import Control.Lens (At (at), ix, makeLenses)
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
newtype Env = Env {_scopes :: [Scope]}
  deriving (Show)

-- | Create empty environment.
emptyEnv :: Env
emptyEnv = Env []

-- *** Scope

-- | Scope contains identifiers mapped to their types.
data Scope = Scope
  { _scopeType :: ScopeType,
    _vars :: Map Identifier Type
  }
  deriving (Show)

-- | @Scope@ type.
data ScopeType = ForScope | OrdinaryScope
  deriving (Show, Eq)

-- | Create @Scope@ from its type and elements.
scope :: ScopeType -> [(Identifier, Type)] -> Scope
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
    IdentifierNotFound Identifier
  | -- | Identifier redeclaration error.
    IdentifierRedeclaration Identifier
  | -- | Mismatched types error.
    MismatchedTypes
  | -- | Constant integer expression types not in `int` bounds error.
    NotInIntBounds
  | -- | Division by 0 in constant integer expression error.
    DivisionByZero
  | -- | @break@ or @continue@ statement used outside of @ForScope@.
    BreakOrContinueOutsideOfForScope
  deriving (Show, Eq)

-- ** Optics

makeLenses ''Env
makeLenses ''Scope
makeLenses ''ScopeType

var :: Applicative f => Int -> Identifier -> (Maybe Type -> f (Maybe Type)) -> Env -> f Env
var i name = scopes . ix i . vars . at name
