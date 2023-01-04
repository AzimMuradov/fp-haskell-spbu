module PrimitiveValue where

import Data.Text (Text)
import qualified Parser.Ast as Ast

-- | Primitive value.
data PrimitiveValue num
  = -- | Number primitive.
    PrimNum num
  | -- | Boolean primitive.
    PrimBool Bool
  | -- | String primitive.
    PrimString Text
  deriving (Show, Eq)

-- | Represents an unsuccessful operation application on primitive values.
data Err
  = -- | Mismatched types error.
    MismatchedTypes
  | -- | Division by 0 error.
    DivisionByZero

primitiveUnOpApplication :: Integral num => Ast.UnaryOp -> PrimitiveValue num -> Either Err (PrimitiveValue num)
primitiveUnOpApplication unOp constant = case (unOp, constant) of
  (Ast.UnaryPlusOp, PrimNum c) -> return $ PrimNum c
  (Ast.UnaryMinusOp, PrimNum c) -> return $ PrimNum $ -c
  (Ast.NotOp, PrimBool c) -> return $ PrimBool $ not c
  _ -> Left MismatchedTypes

primitiveBinOpApplication :: Integral num => Ast.BinaryOp -> PrimitiveValue num -> PrimitiveValue num -> Either Err (PrimitiveValue num)
primitiveBinOpApplication binOp lhs rhs = case (binOp, lhs, rhs) of
  -- Number
  (Ast.EqOp, PrimNum lhs', PrimNum rhs') -> return $ PrimBool $ lhs' == rhs'
  (Ast.NeOp, PrimNum lhs', PrimNum rhs') -> return $ PrimBool $ lhs' /= rhs'
  (Ast.LeOp, PrimNum lhs', PrimNum rhs') -> return $ PrimBool $ lhs' <= rhs'
  (Ast.LtOp, PrimNum lhs', PrimNum rhs') -> return $ PrimBool $ lhs' < rhs'
  (Ast.MeOp, PrimNum lhs', PrimNum rhs') -> return $ PrimBool $ lhs' >= rhs'
  (Ast.MtOp, PrimNum lhs', PrimNum rhs') -> return $ PrimBool $ lhs' > rhs'
  (Ast.PlusOp, PrimNum lhs', PrimNum rhs') -> return $ PrimNum $ lhs' + rhs'
  (Ast.MinusOp, PrimNum lhs', PrimNum rhs') -> return $ PrimNum $ lhs' - rhs'
  (Ast.MultOp, PrimNum lhs', PrimNum rhs') -> return $ PrimNum $ lhs' * rhs'
  (Ast.DivOp, PrimNum lhs', PrimNum rhs') -> if rhs' == 0 then Left DivisionByZero else return $ PrimNum $ lhs' `div` rhs'
  (Ast.ModOp, PrimNum lhs', PrimNum rhs') -> if rhs' == 0 then Left DivisionByZero else return $ PrimNum $ lhs' `mod` rhs'
  -- Boolean
  (Ast.OrOp, PrimBool lhs', PrimBool rhs') -> return $ PrimBool $ lhs' || rhs'
  (Ast.AndOp, PrimBool lhs', PrimBool rhs') -> return $ PrimBool $ lhs' && rhs'
  (Ast.EqOp, PrimBool lhs', PrimBool rhs') -> return $ PrimBool $ lhs' == rhs'
  (Ast.NeOp, PrimBool lhs', PrimBool rhs') -> return $ PrimBool $ lhs' /= rhs'
  -- String
  (Ast.EqOp, PrimString lhs', PrimString rhs') -> return $ PrimBool $ lhs' == rhs'
  (Ast.NeOp, PrimString lhs', PrimString rhs') -> return $ PrimBool $ lhs' /= rhs'
  (Ast.LeOp, PrimString lhs', PrimString rhs') -> return $ PrimBool $ lhs' <= rhs'
  (Ast.LtOp, PrimString lhs', PrimString rhs') -> return $ PrimBool $ lhs' < rhs'
  (Ast.MeOp, PrimString lhs', PrimString rhs') -> return $ PrimBool $ lhs' >= rhs'
  (Ast.MtOp, PrimString lhs', PrimString rhs') -> return $ PrimBool $ lhs' > rhs'
  (Ast.PlusOp, PrimString lhs', PrimString rhs') -> return $ PrimString $ lhs' <> rhs'
  _ -> Left MismatchedTypes
