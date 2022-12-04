-- {-# LANGUAGE OverloadedStrings #-}

-- | Useful constant expression converters.
module Analyzer.ConstExpressionConverters where

import Analyzer.AnalysisResult (Err (..), ResultValue)
import Analyzer.AnalyzedAst (Expression (ExprValue), Value (..))
import Analyzer.AnalyzedType (Type (..))
import Data.Functor ((<&>))
import Data.Text (Text, append)
import qualified Parser.Ast as Ast

-- | Simplifies expression to value expression if possible.
simplifyConstExpr :: Ast.Expression -> ResultValue (Maybe Type, Expression)
simplifyConstExpr expression = do
  constant <- simplifyConstExpr' expression
  case constant of
    CInt c -> convertIntegerToInt c <&> (\c' -> (Just TInt, ExprValue $ ValInt c'))
    CBool c -> return (Just TBool, ExprValue $ ValBool c)
    CString c -> return (Just TString, ExprValue $ ValString c)

-- | Simplifies expression to int if possible.
simplifyConstIntExpr :: Ast.Expression -> ResultValue Int
simplifyConstIntExpr expression = do
  constant <- simplifyConstExpr' expression
  case constant of
    CInt c -> convertIntegerToInt c
    _ -> Left MismatchedTypes

-- | Converts integer to int if possible.
convertIntegerToInt :: Integer -> ResultValue Int
convertIntegerToInt integer =
  -- Checks for overflow
  if toInteger (fromIntegral integer :: Int) == integer
    then return $ fromIntegral integer
    else Left NotInIntBounds

-- | Constant value.
data ConstValue
  = -- | Integer constant.
    CInt Integer
  | -- | Boolean constant.
    CBool Bool
  | -- | String constant.
    CString Text

-- | Simplifies expression to constant value if possible.
simplifyConstExpr' :: Ast.Expression -> ResultValue ConstValue
simplifyConstExpr' expression = case expression of
  Ast.ExprValue val -> case val of
    Ast.ValInt c -> return $ CInt c
    Ast.ValBool c -> return $ CBool c
    Ast.ValString c -> return $ CString c
    _ -> Left MismatchedTypes
  Ast.ExprUnaryOp unOp expr -> simplifyConstExpr' expr >>= constUnOpApplication unOp
  Ast.ExprBinaryOp binOp lhs rhs -> do
    lhs' <- simplifyConstExpr' lhs
    rhs' <- simplifyConstExpr' rhs
    constBinOpApplication binOp lhs' rhs'
  _ -> Left MismatchedTypes
  where
    constUnOpApplication :: Ast.UnaryOp -> ConstValue -> ResultValue ConstValue
    constUnOpApplication unOp constant = case (unOp, constant) of
      (Ast.UnaryPlusOp, CInt c) -> return $ CInt c
      (Ast.UnaryMinusOp, CInt c) -> return $ CInt $ -c
      (Ast.NotOp, CBool c) -> return $ CBool $ not c
      _ -> Left MismatchedTypes

    constBinOpApplication :: Ast.BinaryOp -> ConstValue -> ConstValue -> ResultValue ConstValue
    constBinOpApplication binOp lhs rhs = case (binOp, lhs, rhs) of
      -- Integer
      (Ast.EqOp, CInt lhs', CInt rhs') -> return $ CBool $ lhs' == rhs'
      (Ast.NeOp, CInt lhs', CInt rhs') -> return $ CBool $ lhs' /= rhs'
      (Ast.LeOp, CInt lhs', CInt rhs') -> return $ CBool $ lhs' <= rhs'
      (Ast.LtOp, CInt lhs', CInt rhs') -> return $ CBool $ lhs' < rhs'
      (Ast.MeOp, CInt lhs', CInt rhs') -> return $ CBool $ lhs' >= rhs'
      (Ast.MtOp, CInt lhs', CInt rhs') -> return $ CBool $ lhs' > rhs'
      (Ast.PlusOp, CInt lhs', CInt rhs') -> return $ CInt $ lhs' + rhs'
      (Ast.MinusOp, CInt lhs', CInt rhs') -> return $ CInt $ lhs' - rhs'
      (Ast.MultOp, CInt lhs', CInt rhs') -> return $ CInt $ lhs' * rhs'
      (Ast.DivOp, CInt lhs', CInt rhs') -> if rhs' == 0 then Left DivByZero else return $ CInt $ lhs' `div` rhs'
      (Ast.ModOp, CInt lhs', CInt rhs') -> if rhs' == 0 then Left DivByZero else return $ CInt $ lhs' `mod` rhs'
      -- Boolean
      (Ast.OrOp, CBool lhs', CBool rhs') -> return $ CBool $ lhs' || rhs'
      (Ast.AndOp, CBool lhs', CBool rhs') -> return $ CBool $ lhs' && rhs'
      (Ast.EqOp, CBool lhs', CBool rhs') -> return $ CBool $ lhs' == rhs'
      (Ast.NeOp, CBool lhs', CBool rhs') -> return $ CBool $ lhs' /= rhs'
      -- String
      (Ast.EqOp, CString lhs', CString rhs') -> return $ CBool $ lhs' == rhs'
      (Ast.NeOp, CString lhs', CString rhs') -> return $ CBool $ lhs' /= rhs'
      (Ast.LeOp, CString lhs', CString rhs') -> return $ CBool $ lhs' <= rhs'
      (Ast.LtOp, CString lhs', CString rhs') -> return $ CBool $ lhs' < rhs'
      (Ast.MeOp, CString lhs', CString rhs') -> return $ CBool $ lhs' >= rhs'
      (Ast.MtOp, CString lhs', CString rhs') -> return $ CBool $ lhs' > rhs'
      (Ast.PlusOp, CString lhs', CString rhs') -> return $ CString $ append lhs' rhs'
      _ -> Left MismatchedTypes
