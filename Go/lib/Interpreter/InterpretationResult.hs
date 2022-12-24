-- TODO : Docs
module Interpreter.InterpretationResult where

import Control.Monad.State (StateT)

-- Interpretation result

-- * Result

-- | Represents the result of interpretation.
type Result a = StateT Env ResultValue a

-- TODO : Docs
data Env = Env
  deriving (Show)

-- TODO : Docs
type ResultValue = Either Err

-- | Represents unsuccessful interpretation.
data Err
  = -- | Division by zero error.
    DivisionByZero
  | -- | Index out of bounds error.
    IndexOutOfBounds
  | -- | No return error.
    NoReturn
  | -- | Null dereference error (happens when trying to call the `nil` as a regular function).
    Npe
  | -- | Unexpected error, this type of errors must never happen.
    UnexpectedError
  deriving (Show)
