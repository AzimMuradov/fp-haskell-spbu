-- TODO : Docs
module Analyzer.AnalysisResult where

import qualified Analyzer.AnalyzedAst as AnalyzedAst
import qualified Analyzer.AnalyzedType as AnalyzedType
import Control.Monad.State (MonadTrans (lift), StateT)
import Data.Map (Map)

-- Program checker result

-- * Result

-- | Represents the result of checking.
type Result a = StateT Env ResultValue a

-- ** State

-- TODO : Docs
newtype Env = Env {scopes :: [Scope]}
  deriving (Show)

-- TODO : Docs
emptyEnv :: Env
emptyEnv = Env []

-- | Scope contains identifiers mapped to their types.
data Scope = Scope
  { scopeType :: ScopeType,
    names :: Map AnalyzedAst.Identifier AnalyzedType.Type
  }
  deriving (Show)

-- TODO : Docs
data ScopeType = ForScope | OrdinaryScope
  deriving (Show, Eq)

-- TODO : Docs
type ScopeEntry = (AnalyzedAst.Identifier, AnalyzedType.Type)

-- ** Result Value

-- TODO : Docs
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
  | -- TODO : Docs
    BreakOrContinueInOrdinaryScope
  | -- | Unexpected error, this type of errors must never happen.
    UnexpectedError
  deriving (Show, Eq)

-- TODO : Docs
throw :: Err -> Result a
throw err = lift $ Left err
