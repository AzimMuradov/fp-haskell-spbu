{-# LANGUAGE TemplateHaskell #-}

-- | This module contains types and functions needed for representing analysis result.
module Analyzer.Result where

import Analyzer.AnalyzedAst (Identifier)
import Analyzer.AnalyzedType (Type)
import Control.Lens (At (at), LensLike', ix, makeLenses)
import Control.Monad.Except (ExceptT)
import Control.Monad.State (State)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (unpack)

-- * Analysis result

-- | Represents the result of analysis.
type Result a = ExceptT Err (State Env) a

-- ** Result value

-- | 'Result' value (in other words, it is stateless 'Result').
type ResultValue = Either Err

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
  | -- | Constant integer expression not in `int` bounds error.
    NotInIntBounds
  | -- | Division by 0 in constant integer expression error.
    DivisionByZero
  | -- | @break@ or @continue@ statement used outside of 'ForScope' error.
    BreakOrContinueOutsideOfForScope

instance Show Err where
  show NoMain = "panic: analysis error: no main"
  show (IdentifierNotFound name) = "panic: analysis error: identifier " ++ unpack name ++ " not found"
  show (IdentifierRedeclaration name) = "panic: analysis error: identifier " ++ unpack name ++ " redeclared"
  show MismatchedTypes = "panic: analysis error: mismatched types"
  show NotInIntBounds = "panic: analysis error: not in int bounds"
  show DivisionByZero = "panic: analysis error: integer divide by zero"
  show BreakOrContinueOutsideOfForScope = "panic: analysis error: break or continue is not in a loop"

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

-- | 'Scope' type.
data ScopeType = ForScope | OrdinaryScope
  deriving (Show, Eq)

-- | Create 'Scope' from its type and elements.
scope :: ScopeType -> [(Identifier, Type)] -> Scope
scope t elements = Scope t (Map.fromList elements)

-- | Create empty 'Scope' from its type.
emptyScope :: ScopeType -> Scope
emptyScope t = scope t []

-- *** Optics

makeLenses ''Env
makeLenses ''Scope
makeLenses ''ScopeType

var :: Applicative f => Int -> Identifier -> LensLike' f Env (Maybe Type)
var i name = scopes . ix i . vars . at name
