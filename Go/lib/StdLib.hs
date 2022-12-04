{-# LANGUAGE OverloadedStrings #-}

-- | Module, that provides data necessary for standard library support.
module StdLib where

import Analyzer.AnalyzedAst (Identifier)
import Data.Text (Text, pack)
import qualified Data.Text as Text
import Interpreter.RuntimeValue (RuntimeValue (..))

---------------------------------------------------------StdLib---------------------------------------------------------

-- | Stdlib function.
data StdLibFunction = StdLibFunction
  { name :: Identifier,
    impl :: [RuntimeValue] -> (Maybe RuntimeValue, [Text])
  }

-- | All available stdlib functions.
stdLibFunctions :: [StdLibFunction]
stdLibFunctions = [lenFunction, printlnFunction, panicFunction]

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
