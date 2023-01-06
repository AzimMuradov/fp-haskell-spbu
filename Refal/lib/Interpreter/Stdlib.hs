module Stdlib where

import           AST

descriptor :: FName -> (Pattern -> RSide)
descriptor name =
  case name of
    "Mul"     -> bin (*)
    "Sub"     -> bin (-)
    "Add"     -> bin (+)
    "Div"     -> bin div
    "Mod"     -> bin mod
    "Compare" -> compare
  where
    bin :: (Integer -> Integer -> Integer) -> Pattern -> RSide
    bin op [Sym (MDig a), Sym (MDig b)] = [Term $ Sym $ MDig (op a b)]
    compare :: Pattern -> RSide
    compare [Sym (MDig a), Sym (MDig b)]
      | a == b = [Term $ Sym $ Ch '0']
      | a > b  = [Term $ Sym $ Ch '+']
      | a < b  = [Term $ Sym $ Ch '-']
