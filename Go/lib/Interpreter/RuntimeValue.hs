-- | Values representation during the interpretation process.
module Interpreter.RuntimeValue where

import Analyzer.AnalyzedAst (FunctionValue)
import qualified Analyzer.AnalyzedAst as Ast
import Data.Text (Text, unpack)

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

instance Show RuntimeValue where
  show (ValInt int) = show int
  show (ValBool bool) = if bool then "true" else "false"
  show (ValString string) = unpack string
  show (ValArray vs) = "[" ++ unwords (show <$> vs) ++ "]"
  show (ValFunction Ast.Nil) = "nil"
  show (ValFunction (Ast.AnonymousFunction Ast.Function {})) = "function"
  show (ValFunction (Ast.AnonymousFunction (Ast.StdLibFunction name))) = unpack name

instance Eq RuntimeValue where
  ValInt lhs == ValInt rhs = lhs == rhs
  ValBool lhs == ValBool rhs = lhs == rhs
  ValString lhs == ValString rhs = lhs == rhs
  ValArray lhs == ValArray rhs = lhs == rhs
  ValFunction _ == ValFunction _ = False
  _ == _ = False
