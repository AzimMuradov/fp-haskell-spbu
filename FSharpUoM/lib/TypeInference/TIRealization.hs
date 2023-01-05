{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module TypeInference.TIRealization where

import Ast
import Control.Monad.Except
import Control.Unification (UTerm (UTerm))
import Data.Maybe
import TypeInference.HindleyMilner
import Prelude hiding (lookup)

check :: Expr -> UType -> Infer UType
check e ty = do
  ty' <- inferSingle e
  ty =:= ty'

checkStatement :: [Statement] -> UType -> Infer UType
checkStatement st ty = do
  ty' <- inferStatement st
  ty =:= ty'

binOpInfer :: Expr -> Expr -> Infer UType
binOpInfer e1 e2 = do
  t <- inferSingle e1
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
  t <- inferSingle e1
  case t of
    (UTyInt m) -> do
      _ <- check e2 (UTyInt m)
      return UTyBool
    (UTyDouble m) -> do
      _ <- check e2 (UTyDouble m)
      return UTyBool
    _ -> do
      check e2 t

helpInferStatements :: [Statement] -> Infer UType -> Infer UType
helpInferStatements [] pr = pr
helpInferStatements ((SVarDecl (VarDecl (ident, t) st)) : xs) _ = do
  res <- inferBlock st -- Dup
  _ <- checkForDuplicate (Var ident)
  case t of
    Just ty -> do
      let types = fromTypeToUType ty
      r <- res =:= types
      withBinding ident (Forall [] r) (return r)
    Nothing -> withBinding ident (Forall [] res) (helpInferStatements xs $ return res)
helpInferStatements ((SFunDecl (FunDecl ident val)) : xs) _ = do
  -- TODO : mb chanhe smt
  res <- inferSingle (EValue val)
  tp' <- do
    _ <- checkForDuplicate (Var ident)
    return $ UTyFunDecl ident res
  withBinding ident (Forall [] tp') (helpInferStatements xs $ return res)
helpInferStatements ((SRecFunDecl (RecFunDecl ident val)) : xs) _ = do
  preT <- fresh
  next <- do
    _ <- checkForDuplicate (Var ident)
    withBinding ident (Forall [] preT) $ inferSingle (EValue val)
  withBinding ident (Forall [] next) (helpInferStatements xs $ return next)
-- return $ UTyFunDecl ident next
-- tp' <- tp
-- withBinding ident (Forall [] res) (helpInferStatements xs $ return res)
-- where
--   tp = do
--     st' <- infer (EValue val)
--     UTyFunDecl ident <$> (return st')
helpInferStatements ((SMeasureDecl (MeasureDecl ident mexpr)) : xs) _ = do
  _ <- checkForDuplicate (Measure ident)
  t <- case mexpr of
    Just m -> inferMeasure m
    Nothing -> return $ UTyMeasure ident
  withBinding ident (Forall [] t) (helpInferStatements xs (return t))

-- let res = inferMeasure mexpr -- Dup
-- tp' <- tp
-- withBinding ident (Forall [] tp') (helpInferStatements xs res)
-- where
--   tp = do
--     _ <- checkForDuplicate ident
--     st' <- helpInferStatements st pr
--     case t of
--       Just ty -> do
--         let types = fromTypeToUType ty
--         UTyMeasure ident <$> st' =:= types
--       Nothing -> return $ UTyMeasure ident st'

helpInferStatements ((SExpr e) : xs) _ = do
  res <- inferSingle e
  helpInferStatements xs (return res)

-- topLevelInference :: [Statement] -> [Infer UType]
-- topLevelInference [] = throwError EmptyList : []
-- topLevelInference (x : xs) = inferStatement ((: []) x) : topLevelInference xs

inferStatement :: [Statement] -> Infer UType
inferStatement x = helpInferStatements x (throwError EmptyList)

inferMeasure' :: Maybe MeasureTypeExpr -> Infer UType
inferMeasure' = maybe (return UMPure) inferMeasure

inferMeasure :: MeasureTypeExpr -> Infer UType
inferMeasure (MIdentifier m) = lookup (Measure m)
inferMeasure (MTypesMul m1 m2) = do
  m1' <- inferMeasure m1
  m2' <- inferMeasure m2
  return $ UTyMulMeasureExpr m1' m2'
inferMeasure (MTypesDiv m1 m2) = do
  m1' <- inferMeasure m1
  m2' <- inferMeasure m2
  return $ UTyMulMeasureExpr m1' m2'
inferMeasure (MTypesExp m1 _) = do
  m1' <- inferMeasure m1
  return $ UTyMulMeasureExpr m1' (UTerm $ TyIntF UMPure)

inferBlock :: [Expr] -> Infer UType -- TODO : mb wrong
inferBlock [] = throwError EmptyList
inferBlock [x] = inferSingle x
inferBlock (x : xs) = inferSingle x >> inferBlock xs

inferSingle :: Expr -> Infer UType
inferSingle (EIdentifier x) = lookup (Var x)
inferSingle (EValue (VBool _)) = return UTyBool
inferSingle (EValue (VInt _ m)) = do
  res <- inferMeasure' m
  return $ UTyInt res
inferSingle (EValue (VDouble _ m)) = do
  res <- inferMeasure' m
  return $ UTyDouble res
inferSingle (EValue (VFun xs body)) = infer' xs body
  where
    infer' args st = case args of
      [] -> inferBlock body
      ((ident, Just t) : ys) ->
        let ut = fromTypeToUType t
         in withBinding ident (Forall [] ut) $ UTyFun ut <$> infer' ys st
      ((ident, Nothing) : ys) -> do
        argTy <- fresh
        withBinding ident (Forall [] argTy) $ UTyFun argTy <$> infer' ys st
inferSingle (EIf e1 e2 e3) = do
  _ <- check e1 UTyBool
  e2' <- inferBlock e2
  e3' <- inferBlock e3
  e2' =:= e3'
inferSingle (EOperations (NotOp x)) = do
  _ <- check x UTyBool
  return UTyBool
inferSingle (EOperations (BooleanOp x)) = booleanOpInfer (bL x) (bR x)
inferSingle (EOperations (ComparisonOp x)) = booleanOpInfer (cL x) (cR x)
inferSingle (EOperations (ArithmeticOp x)) = binOpInfer (aL x) (aR x)
inferSingle (ELetInV (x, Just pty) xdef body) = do
  let upty = toUPolytype (Forall [] $ toTypeF pty)
  upty' <- skolemize upty
  bl <- inferBlock xdef
  _ <- bl =:= upty'
  withBinding x upty $ inferBlock body
inferSingle (ELetInV (x, Nothing) xdef body) = do
  ty <- inferBlock xdef
  pty <- generalize ty
  withBinding x pty $ inferBlock body

-- inferSingle (ELetInF x xdef body) = do
--   let upty = toUPolytype (Forall [] $ toTypeF pty)
--   upty' <- skolemize upty
--   bl <- inferBlock xdef
--   _ <- bl =:= upty'
--   withBinding x upty $ inferBlock body

-- inferSingle (ELetInF x xdef body) = do
--   ty <- inferBlock xdef
--   pty <- generalize ty
--   withBinding x pty $ inferBlock body

-- TODO : Fix bug with application
inferSingle (EApplication e1 e2) = do
  funTy <- inferSingle e1
  argTy <- inferSingle e2
  resTy <- fresh
  _ <- funTy =:= UTyFun argTy resTy
  -- ctx <- ask
  -- error $ show ctx <> show funTy <> " --- "  <> show argTy <> " --- "  <> show resTy <> " --- "  <> show kek
  return resTy
inferSingle _ = undefined
