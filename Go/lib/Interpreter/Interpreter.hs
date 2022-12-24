-- TODO : Docs
module Interpreter.Interpreter where

import qualified Analyzer.AnalyzedAst as Ast
import Errors (todo')
import Interpreter.InterpretationResult

------------------------------------------------------Interpreter-------------------------------------------------------

-- | Interpreter entry point. Assumes that program is checked.
interpret :: Ast.Program -> ResultValue (Ast.Program, Env)
interpret = todo'
