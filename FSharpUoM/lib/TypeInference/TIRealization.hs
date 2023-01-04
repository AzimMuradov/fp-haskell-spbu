{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module TypeInference.TIRealization where

import Ast
import Control.Monad.Except
import Data.Maybe
import TypeInference.HindleyMilner
import Prelude hiding (lookup)
import qualified Data.Map as M

check :: Expr -> UType -> Infer UType
check e ty = do
  ty' <- infer e
  ty =:= ty'

checkStatement :: [Statement] -> UType -> Infer UType
checkStatement st ty = do
  ty' <- inferStatement st
  ty =:= ty'

binOpInfer :: Expr -> Expr -> Infer UType
binOpInfer e1 e2 = do
  t <- infer e1
  case t of
    (UTyInt m) -> do
      _ <- check e2 (UTyInt m)
      return (UTyInt m)
    (UTyDouble m) -> do
      _ <- check e2 (UTyDouble m)
      return (UTyDouble m)
    _ -> do
      check e2 t

booleanOpInfer :: Expr -> Expr -> Infer UType
booleanOpInfer e1 e2 = do
  t <- infer e1
  case t of
    (UTyInt m) -> do
      _ <- check e2 (UTyInt m)
      return UTyBool
    (UTyDouble m) -> do
      _ <- check e2 (UTyDouble m)
      return UTyBool
    _ -> do
      check e2 t

helper :: [Statement] -> Infer UType -> Infer UType
helper [] pr = pr
helper (x : xs) _ = case x of
  (SExpr e) -> do
    res <- infer e
    helper xs (return res)
  -- (SVarDecl (VarDecl (ident, t) sts)) -> do
  --   varType <- case t of
  --     Just ty -> return $ fromTypeToUType ty
  --     Nothing -> do fresh

  --   res <- checkStatement sts varType
  --   _ <- M.insert ident varType
  --   return res

    
-- TODO : Activate non-complite warnings

inferStatement :: [Statement] -> Infer UType
inferStatement x = helper x (throwError EmptyList)

infer :: Expr -> Infer UType
infer (EIdentifier x) = lookup x

infer (EValue (VBool _)) = return UTyBool

infer (EValue (VInt _ m)) = return (UTyInt m)

infer (EValue (VDouble _ m)) = return (UTyDouble m)

infer (EValue (VFun xs body)) = he xs body
  where
    he args st =
      case args of
        [] -> inferStatement body
        ((ident, Just t) : ys) -> do
          let ut = fromTypeToUType t
           in withBinding ident (Forall [] ut) $ do UTyFun ut <$> he ys st
        ((ident, Nothing) : ys) -> do
          argTy <- fresh
          withBinding ident (Forall [] argTy) $ do UTyFun argTy <$> he ys st

infer (EIf e1 e2 e3) = do
  _ <- check e1 UTyBool
  e2' <- inferStatement e2
  e3' <- inferStatement e3
  e2' =:= e3'

infer (EOperations (NotOp x)) = do
  _ <- check x UTyBool
  return UTyBool

infer (EOperations (BooleanOp x)) =
  case x of
    (AndOp e1 e2) -> booleanOpInfer e1 e2
    (OrOp e1 e2) -> booleanOpInfer e1 e2

infer (EOperations (ComparisonOp x)) =
  case x of
    (EqOp e1 e2) -> booleanOpInfer e1 e2
    (NeOp e1 e2) -> booleanOpInfer e1 e2
    (LtOp e1 e2) -> booleanOpInfer e1 e2
    (LeOp e1 e2) -> booleanOpInfer e1 e2
    (MtOp e1 e2) -> booleanOpInfer e1 e2
    (MeOp e1 e2) -> booleanOpInfer e1 e2
infer (EOperations (ArithmeticOp x)) = binOpInfer (aL x) (aR x)

infer (ELetIn (x, Just pty) xdef body) = do
  let upty = toUPolytype (Forall [] $ toTypeF pty)
  upty' <- skolemize upty
  _ <- check xdef upty'
  withBinding x upty $ inferStatement body

infer (ELetIn (x, Nothing) xdef body) = do
  ty <- infer xdef
  pty <- generalize ty
  withBinding x pty $ inferStatement body

infer (EApplication e1 e2) = do
  funTy <- infer e1
  argTy <- infer e2
  resTy <- fresh
  _ <- funTy =:= UTyFun argTy resTy
  return resTy