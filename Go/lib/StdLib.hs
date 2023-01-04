{-# LANGUAGE OverloadedStrings #-}

-- | Module, that provides data necessary for the standard library support.
module StdLib where

import Analyzer.AnalyzedAst (Identifier)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text, append, pack)
import qualified Data.Text as T
import Interpreter.Result (Err (Panic, UnexpectedError), ResultValue, RuntimeValue' (..))

---------------------------------------------------------StdLib---------------------------------------------------------

-- * StdLib functions

-- | StdLib function.
data StdLibFunction = StdLibFunction
  { name :: Identifier,
    impl :: StdLibFuncImpl
  }

-- | Convenient type alias for stdlib function implementation.
type StdLibFuncImpl = [RuntimeValue'] -> StdLibFuncResult

-- | Convenient type alias for stdlib function result.
type StdLibFuncResult = ResultValue (Maybe RuntimeValue', Text)

-- | All available stdlib functions.
stdLibFunctions :: [StdLibFunction]
stdLibFunctions = [lenFunction, printFunction, printlnFunction, panicFunction]

-- | 'stdLibFunctions' given in map representation for convenience.
stdLibFunctionsMap :: Map Identifier StdLibFuncImpl
stdLibFunctionsMap = Map.fromList $ (\f -> (name f, impl f)) <$> stdLibFunctions

-------------------------------------------------------Functions--------------------------------------------------------

-- * StdLib functions implementation

-- ** @len@

-- | @len@ function.
lenFunction :: StdLibFunction
lenFunction = StdLibFunction {name = "len", impl = lenImpl}

-- | @len@ implementation.
lenImpl :: StdLibFuncImpl
lenImpl args = case args of
  [ValString' x] -> ok $ T.length x
  [ValArray' xs] -> ok $ length xs
  _ -> Left UnexpectedError
  where
    ok int = Right (Just $ ValInt' int, "")

-- ** @print@

-- | @print@ function.
printFunction :: StdLibFunction
printFunction = StdLibFunction {name = "print", impl = printImpl}

-- | @print@ implementation.
printImpl :: StdLibFuncImpl
printImpl args = Right (Nothing, T.concat (pack . show <$> args))

-- ** @println@

-- | @println@ function.
printlnFunction :: StdLibFunction
printlnFunction = StdLibFunction {name = "println", impl = printlnImpl}

-- | @println@ implementation.
printlnImpl :: StdLibFuncImpl
printlnImpl args = Right (Nothing, append (T.unwords (pack . show <$> args)) "\n")

-- ** @panic@

-- | @panic@ function.
panicFunction :: StdLibFunction
panicFunction = StdLibFunction {name = "panic", impl = panicImpl}

-- | @panic@ implementation.
panicImpl :: StdLibFuncImpl
panicImpl args = case args of
  [ValString' msg] -> Left $ Panic msg
  _ -> Left UnexpectedError
