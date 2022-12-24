-- | Values representation during the interpretation process.
module Interpreter.RuntimeValue where

import Analyzer.AnalyzedAst (FunctionValue)
import Data.Text (Text)

-- | Represents runtime value of the calculated expression.
data RuntimeValue
  = -- | Int value.
    ValInt Int
  | -- | Boolean value.
    ValBool Bool
  | -- | String value.
    ValString Text
  | -- | Array value.
    ValArray [RuntimeValue]
  | -- | Function value.
    ValFunction FunctionValue
  deriving (Show)

instance Eq RuntimeValue where
  ValInt lhs == ValInt rhs = lhs == rhs
  ValBool lhs == ValBool rhs = lhs == rhs
  ValString lhs == ValString rhs = lhs == rhs
  ValArray lhs == ValArray rhs = lhs == rhs
  ValFunction _ == ValFunction _ = False
  _ == _ = False
