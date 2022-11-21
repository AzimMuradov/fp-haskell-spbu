{-# LANGUAGE OverloadedStrings #-}

module Interpreter where

import qualified Ast
import Data.Map ((!))
import qualified Data.Map as Map
import Result (AccOut, Env (..), IResult, ISuccess (..), Scope (..))
import Prelude hiding (id)

-- | Interpreter entry point.
--
-- Assumes that program is checked.
interpretProgram :: Ast.Program -> IResult ()
interpretProgram (Ast.Program _ funcs) = interpretFunc (Env [Scope Map.empty] funcsMap) [] mainFStmts
  where
    funcSelector f = let Ast.FunctionDef id _ _ = f in id
    funcsMap = Map.fromList $ (funcSelector <$> funcs) `zip` funcs
    mainF = funcsMap ! "main"
    Ast.FunctionDef _ _ mainFStmts = mainF

interpretFunc :: Env -> AccOut -> [Ast.Statement] -> IResult ()
interpretFunc (Env scopeStack funcs) accOut stmts = do
  ISuccess (Env scopeStack funcs) newOut _ <- foldl fff init stmts
  undefined
  where
    fff :: IResult [Scope] -> Ast.Statement -> IResult [Scope]
    fff acc stmt = acc >>= (\a -> interpretStmt (env a) stmt)

    init :: IResult [Scope]
    init = Right (ISuccess (Env scopeStack funcs) accOut scopeStack)

interpretStmt :: Env -> Ast.Statement -> IResult [Scope]
interpretStmt env stmt = case stmt of
  Ast.StmtReturn exprs -> undefined
  Ast.StmtBreak -> undefined
  Ast.StmtContinue -> undefined
  Ast.StmtFor for -> undefined
  Ast.StmtVarDecl varDecl -> undefined
  Ast.StmtIfElse ifElse -> undefined
  Ast.StmtBlock stmts -> undefined
  Ast.StmtSimple simpleStmt -> undefined

interpretExpr :: Env -> Ast.Expression -> IResult ()
interpretExpr env expr = case expr of
  Ast.ExprLiteral lit -> undefined
  Ast.ExprIdentifier id -> undefined
  Ast.ExprUnaryOp unOp expr -> do
    e <- interpretExpr env expr
    undefined
  Ast.ExprBinaryOp binOp lhs rhs -> undefined
  Ast.ExprFuncCall func [args] -> undefined
  Ast.ExprArrayAccessByIndex arr index -> undefined

-- >>> True
-- True

-- TODO : Support

-- vars (identifiers)
-- func
-- globals
-- closures
-- output <- string
-- stack
