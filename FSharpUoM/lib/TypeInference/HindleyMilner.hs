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
  | TyIntF (Maybe MeasureTypeExpr)
  | TyDoubleF (Maybe MeasureTypeExpr)
  | TyFunF a a
  deriving (Show, Eq, Functor, Foldable, Traversable, Generic1, Unifiable)

type TypeF = Fix HType

type UType = UTerm HType IntVar

data Poly t = Forall [Identifier] t
  deriving (Eq, Show, Functor)

type Polytype = Poly TypeF

type UPolytype = Poly UType

pattern TyVar :: Identifier -> TypeF
pattern TyVar v = Fix (TyVarF v)

pattern TyInt :: Maybe MeasureTypeExpr -> TypeF
pattern TyInt x = Fix (TyIntF x)

pattern TyDouble :: Maybe MeasureTypeExpr -> TypeF
pattern TyDouble x = Fix (TyDoubleF x)

pattern TyBool :: TypeF
pattern TyBool = Fix TyBoolF

pattern TyFun :: TypeF -> TypeF -> TypeF
pattern TyFun t1 t2 = Fix (TyFunF t1 t2)

pattern UTyVar :: Identifier -> UType
pattern UTyVar v = UTerm (TyVarF v)

pattern UTyInt :: Maybe MeasureTypeExpr -> UType
pattern UTyInt x = UTerm (TyIntF x)

pattern UTyDouble :: Maybe MeasureTypeExpr -> UType
pattern UTyDouble x = UTerm (TyDoubleF x)

pattern UTyBool :: UType
pattern UTyBool = UTerm TyBoolF

pattern UTyFun :: UType -> UType -> UType
pattern UTyFun t1 t2 = UTerm (TyFunF t1 t2)

toTypeF :: Type -> TypeF
toTypeF x = case x of
  TBool -> Fix TyBoolF
  (TInt m) -> Fix (TyIntF m)
  (TDouble m) -> Fix (TyDoubleF m)
  (TFun t1 t2) -> Fix $ TyFunF (toTypeF t1) (toTypeF t2)

fromTypeToUType :: Type -> UType
fromTypeToUType x = case x of
  TBool -> UTerm TyBoolF
  (TInt m) -> UTerm (TyIntF m)
  (TDouble m) -> UTerm (TyDoubleF m)
  (TFun t1 t2) -> UTerm $ TyFunF (fromTypeToUType t1) (fromTypeToUType t2)

type Infer = ReaderT Ctx (ExceptT TypeError (IntBindingT HType Identity))

type Ctx = Map Identifier UPolytype

lookup :: Identifier -> Infer UType
lookup x = do
  ctx <- ask
  maybe (throwError $ UnboundVar x) instantiate (M.lookup x ctx)

withBinding :: MonadReader Ctx m => Identifier -> UPolytype -> m a -> m a
withBinding x ty = local (M.insert x ty)

bindHelper :: MonadReader Ctx m => (m a -> m a) -> [Identifier] -> UPolytype -> m a -> m a
bindHelper state [] _ = state
bindHelper _ (x : xs) ty = bindHelper (local (M.insert x ty)) xs ty

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
  freeVars (Forall xs ut) = (\\ (S.fromList (map Left xs))) <$> freeVars ut

instance FreeVars Ctx where
  freeVars = fmap S.unions . mapM freeVars . M.elems

data TypeError where
  EmptyList :: TypeError
  UnboundVar :: Identifier -> TypeError
  Infinite :: IntVar -> UType -> TypeError
  Mismatch :: HType UType -> HType UType -> TypeError

instance Fallible HType IntVar TypeError where
  occursFailure = Infinite
  mismatchFailure = Mismatch

fresh :: Infer UType
fresh = UVar <$> lift (lift freeVar)

(=:=) :: UType -> UType -> Infer UType
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
