{-# LANGUAGE OverloadedStrings #-}

-- | Module, that provides data necessary for the standard library support.
module StdLib where

import Analyzer.AnalyzedAst (Identifier)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text, pack)
import qualified Data.Text as Text
import Interpreter.RuntimeValue (RuntimeValue (..))

-- * StdLib functions

---------------------------------------------------------StdLib---------------------------------------------------------

-- | StdLib function.
data StdLibFunction = StdLibFunction
  { name :: Identifier,
    impl :: FunctionImplementation
  }

-- | Convenient type alias for stdlib function implementation.
type FunctionImplementation = [RuntimeValue] -> (Maybe RuntimeValue, [Text])

-- | All available stdlib functions.
stdLibFunctions :: [StdLibFunction]
stdLibFunctions = [lenFunction, printlnFunction, panicFunction]

-- | @stdLibFunctions@ given in map representation for convenience.
stdLibFunctionsMap :: Map Identifier FunctionImplementation
stdLibFunctionsMap = Map.fromList $ (\f -> (name f, impl f)) <$> stdLibFunctions

-- * StdLib functions implementation

-------------------------------------------------------Functions--------------------------------------------------------

-- ** @len@

-- | @len@ function.
lenFunction :: StdLibFunction
lenFunction = StdLibFunction {name = "len", impl = lenImpl}

-- | @len@ implementation.
lenImpl :: [RuntimeValue] -> (Maybe RuntimeValue, [Text])
lenImpl [ValString x] = (Just $ ValInt $ Text.length x, [])
lenImpl [ValArray xs] = (Just $ ValInt $ length xs, [])
lenImpl _ = undefined

-- ** @println@

-- | @println@ function.
printlnFunction :: StdLibFunction
printlnFunction = StdLibFunction {name = "println", impl = printlnImpl}

-- | @println@ implementation.
printlnImpl :: [RuntimeValue] -> (Maybe RuntimeValue, [Text])
printlnImpl [] = (Nothing, [""])
printlnImpl [ValInt x] = (Nothing, [pack $ show x])
printlnImpl [ValBool x] = (Nothing, [pack $ show x])
printlnImpl [ValString x] = (Nothing, [pack $ show x])
printlnImpl _ = undefined

-- ** @panic@

-- | @panic@ function.
panicFunction :: StdLibFunction
panicFunction = StdLibFunction {name = "panic", impl = panicImpl}

-- | @panic@ implementation.
panicImpl :: [RuntimeValue] -> (Maybe RuntimeValue, [Text])
panicImpl [ValString _] = (Nothing, []) -- TODO
panicImpl _ = undefined
