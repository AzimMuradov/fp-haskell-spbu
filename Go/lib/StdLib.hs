{-# LANGUAGE OverloadedStrings #-}

module StdLib (stdLibFuncs) where

import qualified Ast
import Data.Text (Text, pack)

-- Definitions

stdLibFuncs :: [Ast.FunctionDef]
stdLibFuncs = [printlnInt, printlnBool, printlnStr]

-- TODO : Add `lenArr` and `lenStr` functions

-- TODO : Add `panic` function

-- TODO : Add `printInt` function
-- TODO : Add `printBool` function
-- TODO : Add `printStr` function
-- TODO : Add `printFunc` function
-- TODO : Add `printArr` function

printlnInt :: Ast.FunctionDef
printlnInt = Ast.FunctionDef "printlnInt" $ Ast.StdLibFunction (Ast.FunctionType [Ast.TInt] Nothing) printlnImpl

printlnBool :: Ast.FunctionDef
printlnBool = Ast.FunctionDef "printlnBool" $ Ast.StdLibFunction (Ast.FunctionType [Ast.TBool] Nothing) printlnImpl

printlnStr :: Ast.FunctionDef
printlnStr = Ast.FunctionDef "printlnStr" $ Ast.StdLibFunction (Ast.FunctionType [Ast.TString] Nothing) printlnImpl

-- TODO : Add `printlnFunc` function
-- TODO : Add `printlnArr` function

-- Implementations

printlnImpl :: [Ast.Value] -> (Maybe Ast.Value, [Text])
printlnImpl [Ast.LitInt x] = (Nothing, [pack $ show x])
printlnImpl [Ast.LitBool x] = (Nothing, [pack $ show x])
printlnImpl [Ast.LitString x] = (Nothing, [pack $ show x])
printlnImpl _ = undefined
