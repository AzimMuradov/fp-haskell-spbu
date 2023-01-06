{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module TypeInference.HindleyMilner where

import Ast
import Control.Monad.Except
import Control.Monad.Reader
import Control.Unification hiding (applyBindings, (=:=))
import qualified Control.Unification as U
import Control.Unification.IntVar
import Data.Foldable (fold)
import Data.Functor.Fixedpoint
import Data.Functor.Identity
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Set (Set, (\\))
import qualified Data.Set as S
import Data.Text (pack)
import GHC.Generics (Generic1)
import Prelude hiding (lookup)


data HType a
  = TyVarF Identifier
  | TyBoolF
  | TyIntF a
  | TyDoubleF a
  | TyFunF a a
  | TyVarDeclF Identifier a
  | TyFunDeclF Identifier a
  | MPureF
  | TyMeasureF Identifier
  | TyMulMeasureExprF a a
  | TyDivMeasureExprF a a
  | TyExpMeasureExprF a a
  deriving (Show, Eq, Functor, Foldable, Traversable, Generic1, Unifiable)

type TypeF = Fix HType

type UType = UTerm HType IntVar

data Poly t = Forall [Identifier] t
  deriving (Eq, Show, Functor)

type Polytype = Poly TypeF

type UPolytype = Poly UType

pattern TyVar :: Identifier -> TypeF
pattern TyVar v = Fix (TyVarF v)

pattern TyMeasure :: Identifier -> TypeF
pattern TyMeasure  m = Fix (TyMeasureF m)

pattern TyInt :: TypeF -> TypeF
pattern TyInt x = Fix (TyIntF x)

pattern MPure :: TypeF
pattern MPure = Fix MPureF

pattern TyDouble :: TypeF -> TypeF
pattern TyDouble x = Fix (TyDoubleF x)

pattern TyBool :: TypeF
pattern TyBool = Fix TyBoolF

pattern TyFun :: TypeF -> TypeF -> TypeF
pattern TyFun t1 t2 = Fix (TyFunF t1 t2)

pattern TyVarDecl :: Identifier -> TypeF -> TypeF
pattern TyVarDecl x t = Fix (TyVarDeclF x t)

pattern TyFunDecl :: Identifier -> TypeF -> TypeF
pattern TyFunDecl x t = Fix (TyFunDeclF x t)

pattern TyMulMeasureExpr :: TypeF -> TypeF -> TypeF
pattern TyMulMeasureExpr t1 t2 = Fix (TyMulMeasureExprF t1 t2)

pattern TyDivMeasureExpr :: TypeF -> TypeF -> TypeF
pattern TyDivMeasureExpr t1 t2 = Fix (TyDivMeasureExprF t1 t2)

pattern TyExpMeasureExpr :: TypeF -> TypeF -> TypeF
pattern TyExpMeasureExpr t1 t2 = Fix (TyExpMeasureExprF t1 t2)

pattern UTyVar :: Identifier -> UType
pattern UTyVar v = UTerm (TyVarF v)

pattern UTyMeasure :: Identifier -> UType
pattern UTyMeasure m = UTerm (TyMeasureF m)

pattern UTyInt :: UType -> UType
pattern UTyInt x = UTerm (TyIntF x)

pattern UMPure :: UType
pattern UMPure = UTerm MPureF

pattern UTyDouble :: UType -> UType
pattern UTyDouble x = UTerm (TyDoubleF x)

pattern UTyBool :: UType
pattern UTyBool = UTerm TyBoolF

pattern UTyFun :: UType -> UType -> UType
pattern UTyFun t1 t2 = UTerm (TyFunF t1 t2)

pattern UTyVarDecl :: Identifier -> UType -> UType
pattern UTyVarDecl x t = UTerm (TyVarDeclF x t)

pattern UTyFunDecl :: Identifier -> UType -> UType
pattern UTyFunDecl x t = UTerm (TyFunDeclF x t)

pattern UTyMulMeasureExpr :: UType -> UType -> UType
pattern UTyMulMeasureExpr m1 m2 = UTerm (TyMulMeasureExprF m1 m2)

pattern UTyDivMeasureExpr :: UType -> UType -> UType
pattern UTyDivMeasureExpr m1 m2 = UTerm (TyDivMeasureExprF m1 m2)

pattern UTyExpMeasureExpr :: UType -> UType -> UType
pattern UTyExpMeasureExpr m1 m2 = UTerm (TyExpMeasureExprF m1 m2)

toTypeF :: Type -> TypeF
toTypeF x = case x of
  TBool -> Fix TyBoolF
  (TInt measure) -> case measure of 
    Just m -> Fix (TyIntF $ fromMToF m)
    Nothing -> Fix (TyIntF MPure)
  (TDouble measure) -> case measure of 
    Just m -> Fix (TyDoubleF $ fromMToF m)
    Nothing -> Fix (TyDoubleF MPure)
  (TFun t1 t2) -> Fix $ TyFunF (toTypeF t1) (toTypeF t2)

fromMToF :: MeasureTypeExpr -> TypeF
fromMToF x = case x of
   (MIdentifier ident) -> TyMeasure ident
   (MTypesMul e1 e2) -> TyMulMeasureExpr (fromMToF e1) (fromMToF e2)
   (MTypesDiv e1 e2) -> TyDivMeasureExpr (fromMToF e1) (fromMToF e2)
   (MTypesExp e1 _) -> TyExpMeasureExpr (fromMToF e1) (Fix $ TyIntF MPure)

fromMToUM :: MeasureTypeExpr -> UType
fromMToUM x = case x of
   (MIdentifier ident) -> UTyMeasure ident
   (MTypesMul e1 e2) -> UTyMulMeasureExpr (fromMToUM e1) (fromMToUM e2)
   (MTypesDiv e1 e2) -> UTyDivMeasureExpr (fromMToUM e1) (fromMToUM e2)
   (MTypesExp e1 _) -> UTyExpMeasureExpr (fromMToUM e1) (UTerm $ TyIntF UMPure)


fromTypeToUType :: Type -> UType
fromTypeToUType x = case x of
  TBool -> UTerm TyBoolF
  (TInt measure) -> case measure of
    Just m -> UTerm (TyIntF $ fromMToUM m)
    Nothing -> UTerm (TyIntF UMPure)
  (TDouble measure) -> case measure of
    Just m -> UTerm (TyDoubleF $ fromMToUM m)
    Nothing -> UTerm (TyDoubleF UMPure)
  (TFun t1 t2) -> UTerm $ TyFunF (fromTypeToUType t1) (fromTypeToUType t2)

type Infer = ReaderT Ctx (ExceptT TypeError (IntBindingT HType Identity))

type Ctx = Map Identifier UPolytype

lookup :: LookUpType -> Infer UType
lookup x = do
  ctx <- ask
  case x of
    (Var v) -> maybe (throwError $ UnboundVar v) instantiate (M.lookup v ctx)
    (Measure m) -> maybe (throwError $ UnboundMeasure m) instantiate (M.lookup m ctx)
  
checkForDuplicate :: LookUpType -> Infer UType
checkForDuplicate x = do
  ctx <- ask
  case x of
    (Var v) -> 
      case M.lookup v ctx of
        (Just _) -> throwError $ DuplicateDifinition v
        Nothing -> return $ UTyVar v
    (Measure m) -> 
      case M.lookup m ctx of
        (Just _) -> throwError $ DuplicateMeasureDifinition m
        Nothing -> return $ UTyVar m

withBinding :: MonadReader Ctx m => Identifier -> UPolytype -> m a -> m a
withBinding x ty = local (M.insert x ty)

-- bindHelper :: MonadReader Ctx m => (m a -> m a) -> [Identifier] -> UPolytype -> m a -> m a
-- bindHelper state [] _ = state
-- bindHelper _ (x : xs) ty = bindHelper (local (M.insert x ty)) xs ty

ucata :: Functor t => (v -> a) -> (t a -> a) -> UTerm t v -> a
ucata f _ (UVar v) = f v
ucata f g (UTerm t) = g (fmap (ucata f g) t)

deriving instance Ord IntVar

class FreeVars a where
  freeVars :: a -> Infer (Set (Either Identifier IntVar))

instance FreeVars UType where
  freeVars ut = do
    fuvs <- fmap (S.fromList . map Right) . lift . lift $ getFreeVars ut
    let ftvs =
          ucata
            (const S.empty)
            (\case TyVarF x -> S.singleton (Left x); f -> fold f)
            ut
    return $ fuvs `S.union` ftvs

instance FreeVars UPolytype where
  freeVars (Forall xs ut) = (\\ S.fromList (map Left xs)) <$> freeVars ut

instance FreeVars Ctx where
  freeVars = fmap S.unions . mapM freeVars . M.elems

data LookUpType = Var Identifier
                | Measure Identifier

data TypeError where
  EmptyList :: TypeError
  Unreachable :: TypeError
  DuplicateDifinition :: Identifier -> TypeError
  DuplicateMeasureDifinition :: Identifier -> TypeError
  UnboundVar :: Identifier -> TypeError
  UnboundMeasure :: Identifier -> TypeError
  Infinite :: IntVar -> UType -> TypeError
  Mismatch :: HType UType -> HType UType -> TypeError

instance Fallible HType IntVar TypeError where
  occursFailure = Infinite
  mismatchFailure = Mismatch

fresh :: Infer UType
fresh = UVar <$> lift (lift freeVar)

(=:=) :: UType -> UType -> Infer UType
(UTyFunDecl _ t) =:= (UTyFunDecl _ t') = t =:= t'
(UTyFunDecl _ t) =:= t' = lift $ t U.=:= t'
t' =:= (UTyFunDecl _ t) = lift $ t U.=:= t'
s =:= t = lift $ s U.=:= t

applyBindings :: UType -> Infer UType
applyBindings = lift . U.applyBindings

instantiate :: UPolytype -> Infer UType
instantiate (Forall xs uty) = do
  xs' <- mapM (const fresh) xs
  return $ substU (M.fromList (zip (map Left xs) xs')) uty

substU :: Map (Either Identifier IntVar) UType -> UType -> UType
substU m =
  ucata
    (\v -> fromMaybe (UVar v) (M.lookup (Right v) m))
    ( \case
        TyVarF v -> fromMaybe (UTyVar v) (M.lookup (Left v) m)
        f -> UTerm f
    )

skolemize :: UPolytype -> Infer UType
skolemize (Forall xs uty) = do
  xs' <- mapM (const fresh) xs
  return $ substU (M.fromList (zip (map Left xs) (map toSkolem xs'))) uty
  where
    toSkolem (UVar v) = UTyVar (mkVarName "s" v)

mkVarName :: String -> IntVar -> Identifier
mkVarName nm (IntVar v) = pack (nm ++ show (v + (maxBound :: Int) + 1))

generalize :: UType -> Infer UPolytype
generalize uty = do
  uty' <- applyBindings uty
  ctx <- ask
  tmfvs <- freeVars uty'
  ctxfvs <- freeVars ctx
  let fvs = S.toList $ tmfvs \\ ctxfvs
      xs = map (either id (mkVarName "a")) fvs
  return $ Forall xs (substU (M.fromList (zip fvs (map UTyVar xs))) uty')

toUPolytype :: Polytype -> UPolytype
toUPolytype = fmap unfreeze

fromUPolytype :: UPolytype -> Polytype
fromUPolytype = fmap (fromJust . freeze)
