{-# LANGUAGE OverloadedStrings #-}

module AstOptimizer where

import qualified Ast

-- TODO : Add const expressions simplification (w/o `const` keyword)

-- should help for creation simpler AST
-- should help finding 'too big int literals' 'compile' (parse) time

simplifyConstExpr :: Ast.Expression -> Maybe Ast.Value
simplifyConstExpr (Ast.ExprLiteral lit) = Just lit
simplifyConstExpr (Ast.ExprIdentifier _) = Nothing
simplifyConstExpr (Ast.ExprUnaryOp _ _) = undefined -- TODO
simplifyConstExpr (Ast.ExprBinaryOp {}) = undefined -- TODO
simplifyConstExpr (Ast.ExprFuncCall _ _) = undefined -- TODO
simplifyConstExpr (Ast.ExprArrayAccessByIndex _ _) = undefined -- TODO

-- simplifyConstExpr Ast.ExprArrayIndexAccess {arr, index} = do
--   Just Ast.LitArray simp_arr <- simplifyConstExpr arr
--   Just Ast.LitInt simp_index <- simplifyConstExpr index
--   return Ast.Value {  }

-- simplifyConstIntExpr :: Ast.Expression -> Maybe Int
-- simplifyConstIntExpr (Ast.ExprLiteral lit) = Just lit
-- simplifyConstIntExpr (Ast.ExprIdentifier _) = Nothing
-- simplifyConstIntExpr (Ast.ExprUnaryOp _ _) = undefined
-- simplifyConstIntExpr (Ast.ExprBinaryOp _ _ _) = undefined
-- simplifyConstIntExpr (Ast.ExprFuncCall _ _) = undefined
-- simplifyConstIntExpr (Ast.ExprArrayAccessByIndex _ _) = undefined
