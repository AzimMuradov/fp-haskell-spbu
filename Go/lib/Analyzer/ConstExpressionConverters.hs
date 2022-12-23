-- {-# LANGUAGE OverloadedStrings #-}

-- | Useful constant expression converters.
module Analyzer.ConstExpressionConverters where

import Analyzer.AnalysisResult (Err (..), ResultValue)
import Analyzer.AnalyzedAst (Expression (ExprValue), Value (..))
import Analyzer.AnalyzedType (Type (..))
import Data.Either.Extra (mapLeft)
import Data.Functor ((<&>))
import qualified Parser.Ast as Ast
import qualified PrimitiveValue as PV

-- | Simplifies expression to value expression if possible.
simplifyConstExpr :: Ast.Expression -> ResultValue (Maybe Type, Expression)
simplifyConstExpr expression = do
  constant <- simplifyConstExpr' expression
  case constant of
    PV.PrimNum c -> convertIntegerToInt c <&> (\c' -> (Just TInt, ExprValue $ ValInt c'))
    PV.PrimBool c -> return (Just TBool, ExprValue $ ValBool c)
    PV.PrimString c -> return (Just TString, ExprValue $ ValString c)

-- | Simplifies expression to int if possible.
simplifyConstIntExpr :: Ast.Expression -> ResultValue Int
simplifyConstIntExpr expression = do
  constant <- simplifyConstExpr' expression
  case constant of
    PV.PrimNum c -> convertIntegerToInt c
    _ -> Left MismatchedTypes

-- | Converts integer to int if possible.
convertIntegerToInt :: Integer -> ResultValue Int
convertIntegerToInt integer =
  -- Checks for overflow
  if toInteger (fromIntegral integer :: Int) == integer
    then return $ fromIntegral integer
    else Left NotInIntBounds

-- | Simplifies expression to constant value if possible.
simplifyConstExpr' :: Ast.Expression -> ResultValue (PV.PrimitiveValue Integer)
simplifyConstExpr' expression = case expression of
  Ast.ExprValue val -> case val of
    Ast.ValInt c -> return $ PV.PrimNum c
    Ast.ValBool c -> return $ PV.PrimBool c
    Ast.ValString c -> return $ PV.PrimString c
    _ -> Left MismatchedTypes
  Ast.ExprUnaryOp unOp expr -> do
    expr' <- simplifyConstExpr' expr
    mapLeft mapErr (PV.primitiveUnOpApplication unOp expr')
  Ast.ExprBinaryOp binOp lhs rhs -> do
    lhs' <- simplifyConstExpr' lhs
    rhs' <- simplifyConstExpr' rhs
    mapLeft mapErr (PV.primitiveBinOpApplication binOp lhs' rhs')
  _ -> Left MismatchedTypes

mapErr :: PV.Err -> Err
mapErr err = case err of
  PV.MismatchedTypes -> MismatchedTypes
  PV.DivByZero -> DivByZero
