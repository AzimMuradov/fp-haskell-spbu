{-# LANGUAGE OverloadedStrings #-}

module ProgramChecker where

import qualified Ast
import Data.Map (Map, (!))
import qualified Data.Map as Map
import qualified Data.Set as Set

-- Also check for name collisions

checkProgram :: Ast.Program -> IResult ()
checkProgram (Ast.Program _ funcs) = undefined
  where
    -- TODO
    funcSelector f = let Ast.FunctionDef id _ _ = f in id
    uniqueFuncs = checkIfHasDuplicates funcs funcSelector
    funcsMap = Map.fromList $ (funcSelector <$> uniqueFuncs) `zip` uniqueFuncs
    mainF = funcsMap ! "main"
    Ast.FunctionDef _ _ mainFStmts = mainF

checkFunc :: Env -> [Ast.Statement] -> IResult ()
checkFunc (Env scopeStack funcs) stmts = do
  -- ISuccess (Env scopeStack funcs) _ <- foldl fff init stmts
  undefined
  where
    check' = undefined

    -- fff :: IResult [Scope] -> Ast.Statement -> IResult [Scope]
    -- fff acc stmt = acc >>= (\a -> checkStmt (env a) stmt)

    -- init :: IResult [Scope]
    -- init = Right (ISuccess (Env scopeStack funcs) scopeStack)

    _ = undefined

checkStmt :: Env -> Ast.Type -> Ast.Statement -> IResult [Scope]
checkStmt env retType stmt = case stmt of
  Ast.StmtReturn exprs ->
    undefined -- TODO : Now
  Ast.StmtBreak -> undefined -- TODO
  Ast.StmtContinue -> undefined -- TODO
  Ast.StmtFor for -> undefined -- TODO
  Ast.StmtVarDecl varDecl ->
    undefined -- TODO : Now
  Ast.StmtIfElse ifElse -> do
    -- TODO : Else, SimpleStmt
    let Ast.IfElse _ condition block _ = ifElse
    undefined -- TODO : Now
  Ast.StmtBlock stmts -> undefined -- TODO
  Ast.StmtSimple simpleStmt -> case simpleStmt of
    Ast.StmtAssignment _ _ -> undefined -- TODO
    Ast.StmtInc _ -> undefined -- TODO
    Ast.StmtDec _ -> undefined -- TODO
    Ast.StmtShortVarDecl _ _ -> undefined -- TODO
    Ast.StmtExpression expr -> do
      ISuccess e t <- checkExpr env expr
      return $ ISuccess e $ scopeStack e

checkExpr :: Env -> Ast.Expression -> IResult Ast.Type
checkExpr env expr = case expr of
  Ast.ExprLiteral lit -> do
    let ok t = Right $ ISuccess env t
    case lit of
      Ast.LitInt _ -> ok Ast.TInt
      Ast.LitBool _ -> ok Ast.TBool
      Ast.LitString _ -> ok Ast.TString
      Ast.LitArray _ _ -> undefined -- TODO
      Ast.LitFunction _ _ -> undefined -- TODO
  Ast.ExprIdentifier id -> do
    undefined -- TODO : Now
  Ast.ExprUnaryOp unOp expr -> do
    ISuccess e t <- checkExpr env expr
    let ok = Right $ ISuccess e t
    let err = Left UndefinedError
    case (unOp, t) of
      (Ast.NotOp, Ast.TBool) -> ok
      (_, Ast.TInt) -> ok
      _ -> err
  Ast.ExprBinaryOp binOp lhs rhs -> do
    ISuccess env' t' <- checkExpr env lhs
    ISuccess env'' t'' <- checkExpr env' rhs
    let ok' t = Right $ ISuccess env'' t
    let ok = ok' t''
    let err = Left UndefinedError
    case (binOp, t', t'') of
      (Ast.OrOp, Ast.TBool, Ast.TBool) -> ok
      (Ast.AndOp, Ast.TBool, Ast.TBool) -> ok
      (Ast.RelOp Ast.EqOp, _, _) | t' == t'' -> ok' Ast.TBool
      (Ast.RelOp Ast.NeOp, _, _) | t' == t'' -> ok' Ast.TBool
      (Ast.RelOp _, Ast.TInt, Ast.TInt) -> ok' Ast.TBool
      (Ast.RelOp _, Ast.TString, Ast.TString) -> ok' Ast.TBool
      (Ast.AddOp Ast.PlusOp, Ast.TString, Ast.TString) -> ok
      (Ast.AddOp _, Ast.TInt, Ast.TInt) -> ok
      (Ast.MulOp _, Ast.TInt, Ast.TInt) -> ok
      _ -> err
  Ast.ExprFuncCall func [args] -> do
    undefined -- TODO : Now
  Ast.ExprArrayAccessByIndex _ _ -> undefined -- TODO

-- >>> True
-- True

-- Utils

checkIfHasDuplicates :: Ord b => [a] -> (a -> b) -> [a]
checkIfHasDuplicates a selector = if hasDuplicates $ selector <$> a then error "" else a

hasDuplicates :: (Ord a) => [a] -> Bool
hasDuplicates list = length list /= length (Set.fromList list)

-- TODO : Support

-- vars (identifiers)
-- func
-- globals
-- closures
-- output <- string
-- stack

type IResult a = Either IError (ISuccess a)

data IError = UndefinedError

data ISuccess a = ISuccess
  { env :: Env,
    result :: a
  }

-- | Environment
data Env = Env
  { scopeStack :: [Scope],
    funcs :: Map Ast.Identifier Ast.FunctionType
  }

-- | Scope
newtype Scope = Scope {vars :: Map Ast.Identifier Ast.Type}

type AccOut = [String]
