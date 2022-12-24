{-# LANGUAGE OverloadedStrings #-}

-- | Module, that provides data necessary for the standard library support.
module StdLib where

import Analyzer.AnalyzedAst (FunctionValue (Nil), Identifier)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text, pack)
import qualified Data.Text as Text
import Interpreter.RuntimeValue (RuntimeValue (..))

---------------------------------------------------------StdLib---------------------------------------------------------

-- * StdLib functions

-- | StdLib function.
data StdLibFunction = StdLibFunction
  { name :: Identifier,
    impl :: FunctionImplementation
  }

-- | Convenient type alias for stdlib function implementation.
type FunctionImplementation = [RuntimeValue] -> (Maybe RuntimeValue, Text)

-- | All available stdlib functions.
stdLibFunctions :: [StdLibFunction]
stdLibFunctions = [lenFunction, printFunction, printlnFunction, panicFunction]

-- | @stdLibFunctions@ given in map representation for convenience.
stdLibFunctionsMap :: Map Identifier FunctionImplementation
stdLibFunctionsMap = Map.fromList $ (\f -> (name f, impl f)) <$> stdLibFunctions

-------------------------------------------------------Functions--------------------------------------------------------

-- * StdLib functions implementation

-- ** @len@

-- | @len@ function.
lenFunction :: StdLibFunction
lenFunction = StdLibFunction {name = "len", impl = lenImpl}

-- | @len@ implementation.
lenImpl :: [RuntimeValue] -> (Maybe RuntimeValue, Text)
lenImpl [ValString x] = (Just $ ValInt $ Text.length x, "")
lenImpl [ValArray xs] = (Just $ ValInt $ length xs, "")
lenImpl _ = undefined

-- ** @print@

-- | @print@ function.
printFunction :: StdLibFunction
printFunction = StdLibFunction {name = "print", impl = printImpl}

-- | @print@ implementation.
printImpl :: [RuntimeValue] -> (Maybe RuntimeValue, Text)
printImpl [] = (Nothing, "")
printImpl [ValInt x] = (Nothing, pack $ show x)
printImpl [ValBool x] = (Nothing, pack $ show x)
printImpl [ValString x] = (Nothing, pack $ show x)
printImpl [ValFunction Nil] = (Nothing, "nil")
printImpl _ = undefined

-- ** @println@

-- | @println@ function.
printlnFunction :: StdLibFunction
printlnFunction = StdLibFunction {name = "println", impl = printlnImpl}

-- | @println@ implementation.
printlnImpl :: [RuntimeValue] -> (Maybe RuntimeValue, Text)
printlnImpl [] = (Nothing, "\n")
printlnImpl [ValInt x] = (Nothing, pack $ show x ++ "\n")
printlnImpl [ValBool x] = (Nothing, pack $ show x ++ "\n")
printlnImpl [ValString x] = (Nothing, pack $ show x ++ "\n")
printlnImpl [ValFunction Nil] = (Nothing, "nil\n")
printlnImpl _ = undefined

-- ** @panic@

-- | @panic@ function.
panicFunction :: StdLibFunction
panicFunction = StdLibFunction {name = "panic", impl = panicImpl}

-- | @panic@ implementation.
panicImpl :: [RuntimeValue] -> (Maybe RuntimeValue, Text)
panicImpl [ValString _] = (Nothing, "") -- TODO
panicImpl _ = undefined
