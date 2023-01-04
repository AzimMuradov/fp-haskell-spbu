{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module TypeInference.PrettyPrint where

import           Data.Maybe
import           Prelude                    hiding (lookup)
import           Text.Printf

import Data.Text ( unpack ) 

import           Control.Unification        hiding ((=:=), applyBindings)
import           Control.Unification.IntVar
import           Data.Functor.Fixedpoint

import TypeInference.HindleyMilner


type Prec = Int

class Pretty p where
  pretty :: p -> String
  pretty = prettyPrec 0

  prettyPrec :: Prec -> p -> String
  prettyPrec _ = pretty

instance Pretty (t (Fix t)) => Pretty (Fix t) where
  prettyPrec p = prettyPrec p . unFix

instance Pretty t => Pretty (HType t) where
  prettyPrec _ (TyVarF x) = unpack x
  prettyPrec _ TyBoolF = "bool"
  prettyPrec _ (TyIntF m)= "int" ++ maybe "" show m
  prettyPrec _ (TyDoubleF m) = "double" ++ show m
  prettyPrec p (TyFunF ty1 ty2) =
    mparens (p > 0) $ prettyPrec 1 ty1 ++ " -> " ++ prettyPrec 0 ty2

instance (Pretty (t (UTerm t v)), Pretty v) => Pretty (UTerm t v) where
  pretty (UTerm t) = pretty t
  pretty (UVar v)  = pretty v

instance Pretty Polytype where
  pretty (Forall [] t) = pretty t
  pretty (Forall xs t) = unwords ("forall" : (unpack <$> xs)) ++ ". " ++ pretty t

mparens :: Bool -> String -> String
mparens True  = ("("++) . (++")")
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
  pretty EmptyList = printf ""
  pretty (UnboundVar x)     = printf "Unbound variable %s" (unpack x)
  pretty (Infinite x ty)    = printf "Infinite type %s = %s" (pretty x) (pretty ty)
  pretty (Mismatch ty1 ty2) = printf "Can't unify %s and %s" (pretty ty1) (pretty ty2)

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