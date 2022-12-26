{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Interpreter.Interpreter where

import qualified Analyzer.AnalyzedAst as Ast
import Control.Lens
import Control.Monad (void, (>=>))
import Control.Monad.Except (MonadError (throwError), liftEither, runExceptT)
import Control.Monad.State (modify, runState)
import Data.Either.Combinators (leftToMaybe, mapBoth)
import Data.Functor (($>))
import Data.List.Extra ((!?))
import qualified Data.Map as Map
import Data.Text (Text, pack)
import qualified Data.Text as T
import Interpreter.InterpretationResult
import Interpreter.InterpreterRuntime
import Interpreter.RuntimeValue (RuntimeValue (..))
import qualified PrimitiveValue as PV
import StdLib (stdLibFunctionsMap)

------------------------------------------------------Interpreter-------------------------------------------------------

-- | Interpreter entry point. Assumes that program is checked.
interpret :: Ast.Program -> (ResultValue (), Env)
interpret ast = runState (runExceptT (interpretProgram ast)) emptyEnv & _2 . accumulatedOutput %~ reverse

-- TODO : Docs
getInterpretationOut :: (ResultValue (), Env) -> (Text, Maybe Text)
getInterpretationOut (result, env) = (T.concat $ env ^. accumulatedOutput, pack . show <$> leftToMaybe result)

-------------------------------------------------Program and functions--------------------------------------------------

-- TODO : Docs
interpretProgram :: Ast.Program -> Result ()
interpretProgram (Ast.Program _ functions) = do
  main <- getMain
  modify $ funcs .~ fs
  void $ interpretFunc main []
  where
    fs = Map.fromList $ (Ast.funcName <$> functions) `zip` (Ast.func <$> functions)
    getMain = maybe (throwError UnexpectedError) return (fs Map.!? "main")

-- TODO : Docs
interpretFunc :: Ast.Function -> [RuntimeValue] -> Result (Maybe RuntimeValue)
interpretFunc (Ast.Function params body voidMark) args = do
  res <- interpretBlock (scope $ params `zip` args) pushFuncScope popFuncScope body
  case (res, voidMark) of
    (Ret val, _) -> return val
    (Unit, Ast.VoidFunc) -> return Nothing
    (Unit, Ast.NonVoidFunc) -> throwError NoReturn
interpretFunc (Ast.StdLibFunction name) args = do
  func <- unwrapJust $ stdLibFunctionsMap Map.!? name
  (res, out) <- liftEither $ func args
  modify $ accumulatedOutput %~ (out :)
  return res

-- TODO : Docs
interpretBlock :: Scope -> (Scope -> Env -> Env) -> (Env -> Env) -> Ast.Block -> Result StmtResult
interpretBlock initScope pushScope popScope block = do
  modify $ pushScope initScope
  res <- foldl f (return Unit) block
  modify popScope
  return res
  where
    f res stmt = res >>= \r -> if r == Unit then interpretStmt stmt else return r

-------------------------------------------------------Statements-------------------------------------------------------

-- TODO : Docs
interpretStmt :: Ast.Statement -> Result StmtResult
interpretStmt statement = case statement of
  Ast.StmtReturn expr -> interpretStmtReturn expr
  Ast.StmtForGoTo goto -> interpretStmtForGoTo goto
  Ast.StmtFor for -> interpretStmtFor for
  Ast.StmtVarDecl varDecl -> interpretStmtVarDecl varDecl
  Ast.StmtIfElse ifElse -> interpretStmtIfElse ifElse
  Ast.StmtBlock block -> interpretStmtBlock block
  Ast.StmtSimple simpleStmt -> interpretStmtSimple simpleStmt

-- TODO : Docs
interpretStmtReturn :: Maybe Ast.Expression -> Result StmtResult
interpretStmtReturn = maybe (return $ Ret Nothing) (fmap Ret . interpretExpr)

-- TODO : Docs
interpretStmtForGoTo :: Ast.ForGoTo -> Result StmtResult
interpretStmtForGoTo = undefined -- TODO

-- TODO : Docs
interpretStmtFor :: Ast.For -> Result StmtResult
interpretStmtFor = undefined -- TODO

-- TODO : Docs
interpretStmtVarDecl :: Ast.VarDecl -> Result StmtResult
interpretStmtVarDecl (Ast.VarDecl name expr) = (interpretExpr' expr >>= addNewVar name) $> Unit

-- TODO : Docs
interpretStmtIfElse :: Ast.IfElse -> Result StmtResult
interpretStmtIfElse (Ast.IfElse condition block elseStmt) = do
  cond <- interpretBoolExpr condition
  if cond
    then interpretBlock emptyScope pushBlockScope popBlockScope block
    else case elseStmt of
      Ast.NoElse -> return Unit
      Ast.Else block' -> interpretBlock emptyScope pushBlockScope popBlockScope block'
      Ast.Elif ifElse -> interpretStmtIfElse ifElse

-- TODO : Docs
interpretStmtBlock :: Ast.Block -> Result StmtResult
interpretStmtBlock = interpretBlock emptyScope pushBlockScope popBlockScope

-- TODO : Docs
interpretStmtSimple :: Ast.SimpleStmt -> Result StmtResult
interpretStmtSimple simpleStmt = case simpleStmt of
  Ast.StmtAssignment updEl expr -> do
    e <- interpretExpr' expr
    (n, _, f) <- getUpdElF updEl
    updateVar n (f e)
    return Unit
  Ast.StmtIncDec updEl incDec -> do
    let upd = case incDec of
          Ast.Inc -> (+ 1)
          Ast.Dec -> \x -> x - 1
    (n, v, f) <- getUpdElF updEl
    v' <- castToInt v
    updateVar n (f (ValInt (upd v')))
    return Unit
  Ast.StmtShortVarDecl name expr -> (interpretExpr' expr >>= addOrUpdateVar name) $> Unit
  Ast.StmtExpression expr -> interpretExpr expr $> Unit

-- TODO : Docs
getUpdElF :: Ast.UpdatableElement -> Result (Ast.Identifier, RuntimeValue, RuntimeValue -> RuntimeValue)
getUpdElF = getUpdElF'
  where
    getUpdElF' (Ast.UpdVar name) = (name,,id) <$> getVarValue name
    getUpdElF' (Ast.UpdArrEl _ _) = undefined -- TODO

------------------------------------------------------Expressions-------------------------------------------------------

-- TODO : Docs
interpretExpr :: Ast.Expression -> Result (Maybe RuntimeValue)
interpretExpr expression = case expression of
  Ast.ExprValue value -> interpretExprValue value
  Ast.ExprIdentifier name -> interpretExprIdentifier name
  Ast.ExprUnaryOp unOp expr -> interpretExprUnaryOp unOp expr
  Ast.ExprBinaryOp binOp lhs rhs -> interpretExprBinaryOp binOp lhs rhs
  Ast.ExprArrayAccessByIndex arr i -> interpretExprArrayAccessByIndex arr i
  Ast.ExprFuncCall func args -> interpretExprFuncCall func args

-- TODO : Docs
interpretExprValue :: Ast.Value -> Result (Maybe RuntimeValue)
interpretExprValue value = case value of
  Ast.ValInt v -> return' $ ValInt v
  Ast.ValBool v -> return' $ ValBool v
  Ast.ValString v -> return' $ ValString v
  Ast.ValArray es -> mapM interpretExpr' es >>= return' . ValArray
  Ast.ValFunction v -> return' $ ValFunction v
  where
    return' v = return $ Just v

-- TODO : Docs
interpretExprIdentifier :: Ast.Identifier -> Result (Maybe RuntimeValue)
interpretExprIdentifier name = Just <$> getVarValue name

-- TODO : Docs
interpretExprUnaryOp :: Ast.UnaryOp -> Ast.Expression -> Result (Maybe RuntimeValue)
interpretExprUnaryOp unOp expr =
  interpretExpr' expr >>= valueToPrimitive >>= liftPV . PV.primitiveUnOpApplication unOp

-- TODO : Docs
interpretExprBinaryOp :: Ast.BinaryOp -> Ast.Expression -> Ast.Expression -> Result (Maybe RuntimeValue)
interpretExprBinaryOp Ast.OrOp lhs rhs =
  Just . ValBool <$> ((||) <$> interpretBoolExpr lhs <*> interpretBoolExpr rhs)
interpretExprBinaryOp Ast.AndOp lhs rhs = do
  Just . ValBool <$> ((&&) <$> interpretBoolExpr lhs <*> interpretBoolExpr rhs)
interpretExprBinaryOp binOp lhs rhs = do
  lhsVal <- interpretExpr' lhs
  rhsVal <- interpretExpr' rhs
  case binOp of
    Ast.EqOp -> return $ Just $ ValBool $ lhsVal == rhsVal
    Ast.NeOp -> return $ Just $ ValBool $ lhsVal /= rhsVal
    _ -> do
      lhsPV <- valueToPrimitive lhsVal
      rhsPV <- valueToPrimitive rhsVal
      liftPV $ PV.primitiveBinOpApplication binOp lhsPV rhsPV

-- TODO : Docs
interpretExprArrayAccessByIndex :: Ast.Expression -> Ast.Expression -> Result (Maybe RuntimeValue)
interpretExprArrayAccessByIndex arr i = do
  arr' <- interpretArrExpr arr
  i' <- interpretIntExpr i
  maybe (throwError IndexOutOfRange) (return . Just) (arr' !? i')

-- TODO : Docs
interpretExprFuncCall :: Ast.Expression -> [Ast.Expression] -> Result (Maybe RuntimeValue)
interpretExprFuncCall func args = do
  func' <- interpretFuncExpr func
  args' <- mapM interpretExpr' args
  interpretFunc func' args'

---------------------------------------------------------Utils----------------------------------------------------------

-- * Utils

-- ** Interpret non-void Expression

-- TODO : Docs
interpretIntExpr :: Ast.Expression -> Result Int
interpretIntExpr = interpretExpr' >=> castToInt

-- TODO : Docs
interpretBoolExpr :: Ast.Expression -> Result Bool
interpretBoolExpr = interpretExpr' >=> castToBool

-- TODO : Docs
interpretArrExpr :: Ast.Expression -> Result [RuntimeValue]
interpretArrExpr = interpretExpr' >=> castToArr

-- TODO : Docs
interpretFuncExpr :: Ast.Expression -> Result Ast.Function
interpretFuncExpr = interpretExpr' >=> castToFunc

-- TODO : Docs
interpretExpr' :: Ast.Expression -> Result RuntimeValue
interpretExpr' = interpretExpr >=> unwrapJust

-- ** Primitive values

-- TODO : Docs
valueToPrimitive :: RuntimeValue -> Result (PV.PrimitiveValue Int)
valueToPrimitive value = case value of
  ValInt v -> return $ PV.PrimNum v
  ValBool v -> return $ PV.PrimBool v
  ValString v -> return $ PV.PrimString v
  _ -> throwError UnexpectedError

-- TODO : Docs
liftPV :: Either PV.Err (PV.PrimitiveValue Int) -> Result (Maybe RuntimeValue)
liftPV pvRes = liftEither $ mapBoth mapErr primitiveToValue pvRes
  where
    mapErr err = case err of
      PV.MismatchedTypes -> UnexpectedError
      PV.DivisionByZero -> DivisionByZero

    primitiveToValue value = case value of
      PV.PrimNum v -> Just $ ValInt v
      PV.PrimBool v -> Just $ ValBool v
      PV.PrimString v -> Just $ ValString v

-- ** Cast runtime values

-- TODO : Docs
castToInt :: RuntimeValue -> Result Int
castToInt val = case val of
  ValInt int -> return int
  _ -> throwError UnexpectedError

-- TODO : Docs
castToBool :: RuntimeValue -> Result Bool
castToBool val = case val of
  ValBool bool -> return bool
  _ -> throwError UnexpectedError

-- TODO : Docs
castToArr :: RuntimeValue -> Result [RuntimeValue]
castToArr val = case val of
  ValArray arr -> return arr
  _ -> throwError UnexpectedError

-- TODO : Docs
castToFunc :: RuntimeValue -> Result Ast.Function
castToFunc val = case val of
  ValFunction (Ast.AnonymousFunction f) -> return f
  ValFunction Ast.Nil -> throwError Npe
  _ -> throwError UnexpectedError
