{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module TypeInference.PrettyPrint where

import Control.Unification hiding (applyBindings, (=:=))
import Control.Unification.IntVar
import Data.Functor.Fixedpoint
import Data.Text (unpack)
import Text.Printf
import TypeInference.HindleyMilner
import Prelude hiding (lookup)

type Prec = Int

class Pretty p where
  pretty :: p -> String
  pretty = prettyPrec 0

  prettyPrec :: Prec -> p -> String
  prettyPrec _ = pretty

instance Pretty (t (Fix t)) => Pretty (Fix t) where
  prettyPrec p = prettyPrec p . unFix

instance Pretty t => Pretty (HType t) where
  prettyPrec _ MPureF = "non measure type" -- "[<Measure>]\ntype "
  prettyPrec _ (TyMeasureF x) = unpack x
  prettyPrec _ (TyMulMeasureExprF m1 m2) = prettyPrec 0 m1 <> " " <> prettyPrec 1 m2
  prettyPrec _ (TyDivMeasureExprF m1 m2) = prettyPrec 0 m1 <> " / " <> prettyPrec 1 m2
  prettyPrec _ (TyExpMeasureExprF m1 m2) = prettyPrec 0 m1 <> " ^ " <> prettyPrec 1 m2
  prettyPrec _ (TyVarF x) = unpack x
  prettyPrec _ TyBoolF = "bool"
  prettyPrec _ (TyIntF m) =
    let measure = prettyPrec 0 m
     in if measure /= "non measure type" then "int<" <> measure <> ">" else "int"
  prettyPrec _ (TyDoubleF m) =
    let measure = prettyPrec 0 m
     in if measure /= "non measure type" then "double<" <> measure <> ">" else "double"
  prettyPrec p (TyFunF ty1 ty2) =
    mparens (p > 0) $ prettyPrec 1 ty1 ++ " -> " ++ prettyPrec 0 ty2
  prettyPrec _ (TyVarDeclF x t) = "val " ++ unpack x ++ ": " ++ prettyPrec 0 t
  prettyPrec _ (TyFunDeclF x t) = "val " ++ unpack x ++ ": " ++ prettyPrec 0 t

instance (Pretty (t (UTerm t v)), Pretty v) => Pretty (UTerm t v) where
  pretty (UTerm t) = pretty t
  pretty (UVar v) = pretty v

instance Pretty Polytype where
  pretty (Forall [] t) = pretty t
  pretty (Forall xs t) = unwords ("forall" : (unpack <$> xs)) ++ ". " ++ pretty t

mparens :: Bool -> String -> String
mparens True = ("(" ++) . (++ ")")
mparens False = id

-- instance Pretty Expr where
--   prettyPrec _ (EVar x) = x
--   prettyPrec _ (EInt i) = show i
--   prettyPrec _ (EDouble i) = show i
--   prettyPrec p (EPlus e1 e2) =
--     mparens (p>1) $
--       prettyPrec 1 e1 ++ " + " ++ prettyPrec 2 e2
--   prettyPrec p (ELam x body) =
--     mparens (p>0) $
--       "\\" ++ x ++ ". " ++ prettyPrec 0 body
--   prettyPrec p (ELet x mty xdef body) =
--     mparens (p>0) $
--       "let " ++ x ++ maybe "" (\ty -> " : " ++ pretty ty) mty
--             ++ " = " ++ prettyPrec 0 xdef
--             ++ " in " ++ prettyPrec 0 body
--   prettyPrec p (EApp e1 e2) =
--     mparens (p>2) $
--       prettyPrec 2 e1 ++ " " ++ prettyPrec 3 e2

instance Pretty IntVar where
  pretty = unpack . mkVarName "u"

instance Pretty TypeError where
  pretty EmptyList = printf "List of statemnet is empty"
  pretty Unreachable = printf "Unreachable state"
  pretty (DuplicateDifinition x) = printf "Duplicate definition of value '%s'" (unpack x)
  pretty (DuplicateMeasureDifinition x) = printf "Duplicate definition of measure type '%s'" (unpack x)
  pretty (UnboundVar x) = printf "Unbound variable '%s'" (unpack x)
  pretty (UnboundMeasure x) = printf "Measure '%s' do not define" (unpack x)
  pretty (Infinite x ty) = printf "Infinite type %s = %s" (pretty x) (pretty ty)
  pretty (Mismatch ty1 ty2) = printf "The type '%s' does not match the type '%s'" (pretty ty1) (pretty ty2)

-- instance Pretty Value where
--   pretty (VInt n) = show n
--   pretty (VDouble n) = show n
--   pretty (VClo x body env)
--     = printf "<%s: %s %s>"
--       x (pretty body) (pretty env)

-- instance Pretty Env where
--   pretty env = "[" ++ intercalate ", " bindings ++ "]"
--     where
--       bindings = map prettyBinding (M.assocs env)
--       prettyBinding (x, v) = x ++ " -> " ++ pretty v