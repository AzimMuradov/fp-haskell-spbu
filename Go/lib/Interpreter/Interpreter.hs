{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Interpreter.Interpreter where

import qualified Analyzer.AnalyzedAst as Ast
import Control.Lens
import Control.Monad (liftM3, void, (>=>))
import Control.Monad.Except (MonadError (throwError), liftEither, runExceptT)
import Control.Monad.ST (ST, runST)
import Control.Monad.State (MonadState (get), StateT (runStateT), modify)
import Data.Either.Combinators (leftToMaybe, mapBoth)
import Data.Functor (($>))
import Data.List.Extra ((!?))
import qualified Data.Map as Map
import Data.STRef (newSTRef, readSTRef)
import Data.Text (Text, pack)
import qualified Data.Text as T
import Interpreter.InterpretationResult
import Interpreter.InterpreterRuntime
import qualified PrimitiveValue as PV
import StdLib (stdLibFunctionsMap)

------------------------------------------------------Interpreter-------------------------------------------------------

-- | Interpreter entry point. Assumes that program is checked.
interpret :: Ast.Program -> (ResultValue (), Env')
interpret ast = runST $ do
  (res, env) <- runStateT (runExceptT (interpretProgram ast)) emptyEnv
  env' <- mapEnv env
  return (res, env' & accumulatedOutput' %~ reverse)

mapEnv :: Env s -> ST s Env'
mapEnv (Env fs fScs accOut) = liftM3 Env' (mapM (readSTRef . fst) fs) (mapM mapFuncScope fScs) (return accOut)

mapFuncScope :: FuncScope s -> ST s FuncScope'
mapFuncScope (FuncScope scs) = FuncScope' <$> mapM mapScope scs

mapScope :: Scope s -> ST s Scope'
mapScope sc = Scope' <$> mapM (fmap mapRuntimeValue . readSTRef) (sc ^. vars)

mapRuntimeValue :: RuntimeValue s -> RuntimeValue'
mapRuntimeValue rv = case rv of
  ValInt v -> ValInt' v
  ValBool v -> ValBool' v
  ValString v -> ValString' v
  ValArray v -> ValArray' $ mapRuntimeValue <$> v
  ValFunction v _ -> ValFunction' v

mapRuntimeValue' :: RuntimeValue' -> RuntimeValue s
mapRuntimeValue' rv = case rv of
  ValInt' v -> ValInt v
  ValBool' v -> ValBool v
  ValString' v -> ValString v
  ValArray' v -> ValArray $ mapRuntimeValue' <$> v
  ValFunction' v -> ValFunction v (FuncScope [])

-- TODO : Docs
getInterpretationOut :: (ResultValue (), Env') -> (Text, Maybe Text)
getInterpretationOut (result, env) = (T.concat $ env ^. accumulatedOutput', pack . show <$> leftToMaybe result)

-------------------------------------------------Program and functions--------------------------------------------------

-- TODO : Docs
interpretProgram :: Ast.Program -> Result s ()
interpretProgram (Ast.Program globals functions) = do
  -- a <- lift2 $ mapFuncScope mainFs
  -- error $ show a
  fs' <- fs
  modify $ funcs .~ fs'
  (main, mainFs) <- maybe (throwError UnexpectedError) return (fs' Map.!? "main")
  main' <- lift2 $ readSTRef main
  void $ interpretFunc main' mainFs []
  where
    gs = do
      exprs <- mapM interpretExpr' (Ast.value <$> globals)
      fsRefs <- lift2 $ mapM newSTRef exprs
      return $ Map.fromList $ (Ast.identifier <$> globals) `zip` fsRefs

    fs = do
      gsFuncScope <- (\a -> FuncScope [Scope a]) <$> gs
      fsRefs <- lift2 $ mapM newSTRef (Ast.func <$> functions)
      return $ Map.fromList $ (Ast.funcName <$> functions) `zip` (fsRefs `zip` replicate (length fsRefs) gsFuncScope)

-- TODO : Docs
interpretFunc :: Ast.Function -> FuncScope s -> [RuntimeValue s] -> Result s (Maybe (RuntimeValue s))
interpretFunc (Ast.Function params body voidMark) (FuncScope scs) args = do
  initScope <- lift2 $ Scope <$> mapM newSTRef (Map.fromList $ params `zip` args)
  res <- interpretBlock (flattenFuncScope $ FuncScope (initScope : scs)) pushFuncScope popFuncScope body
  case (res, voidMark) of
    (Ret val, _) -> return val
    (Unit, Ast.VoidFunc) -> return Nothing
    (Unit, Ast.NonVoidFunc) -> throwError NoReturn
interpretFunc (Ast.StdLibFunction name) _ args = do
  func <- unwrapJust $ stdLibFunctionsMap Map.!? name
  (res, out) <- liftEither $ func (mapRuntimeValue <$> args)
  modify $ accumulatedOutput %~ (out :)
  return (mapRuntimeValue' <$> res)

-- TODO : Docs
interpretBlock :: Scope s -> (Scope s -> Env s -> Env s) -> (Env s -> Env s) -> Ast.Block -> Result s (StmtResult s)
interpretBlock initScope pushScope popScope block = do
  modify $ pushScope initScope
  res <- foldl f (return Unit) block
  modify popScope
  return res
  where
    f res stmt = res >>= \r -> if r == Unit then interpretStmt stmt else return r

-------------------------------------------------------Statements-------------------------------------------------------

-- TODO : Docs
interpretStmt :: Ast.Statement -> Result s (StmtResult s)
interpretStmt statement = case statement of
  Ast.StmtReturn expr -> interpretStmtReturn expr
  Ast.StmtForGoTo goto -> interpretStmtForGoTo goto
  Ast.StmtFor for -> interpretStmtFor for
  Ast.StmtVarDecl varDecl -> interpretStmtVarDecl varDecl
  Ast.StmtIfElse ifElse -> interpretStmtIfElse ifElse
  Ast.StmtBlock block -> interpretStmtBlock block
  Ast.StmtSimple simpleStmt -> interpretStmtSimple simpleStmt

-- TODO : Docs
interpretStmtReturn :: Maybe Ast.Expression -> Result s (StmtResult s)
interpretStmtReturn = maybe (return $ Ret Nothing) (fmap Ret . interpretExpr)

-- TODO : Docs
interpretStmtForGoTo :: Ast.ForGoTo -> Result s (StmtResult s)
interpretStmtForGoTo = undefined -- TODO

-- TODO : Docs
interpretStmtFor :: Ast.For -> Result s (StmtResult s)
interpretStmtFor = undefined -- TODO

-- TODO : Docs
interpretStmtVarDecl :: Ast.VarDecl -> Result s (StmtResult s)
interpretStmtVarDecl (Ast.VarDecl name expr) = (interpretExpr' expr >>= addNewVar name) $> Unit

-- TODO : Docs
interpretStmtIfElse :: Ast.IfElse -> Result s (StmtResult s)
interpretStmtIfElse (Ast.IfElse condition block elseStmt) = do
  cond <- interpretBoolExpr condition
  if cond
    then interpretBlock emptyScope pushBlockScope popBlockScope block
    else case elseStmt of
      Ast.NoElse -> return Unit
      Ast.Else block' -> interpretBlock emptyScope pushBlockScope popBlockScope block'
      Ast.Elif ifElse -> interpretStmtIfElse ifElse

-- TODO : Docs
interpretStmtBlock :: Ast.Block -> Result s (StmtResult s)
interpretStmtBlock = interpretBlock emptyScope pushBlockScope popBlockScope

-- TODO : Docs
interpretStmtSimple :: Ast.SimpleStmt -> Result s (StmtResult s)
interpretStmtSimple simpleStmt = case simpleStmt of
  Ast.StmtAssignment lval expr -> do
    e <- interpretExpr' expr
    (n, _, f) <- getLvalueUpdater lval
    updateVar n (f e)
    return Unit
  Ast.StmtIncDec lval incDec -> do
    let upd = case incDec of
          Ast.Inc -> (+ 1)
          Ast.Dec -> \x -> x - 1
    (n, v, f) <- getLvalueUpdater lval
    v' <- castToInt v
    updateVar n (f (ValInt (upd v')))
    return Unit
  Ast.StmtShortVarDecl name expr -> (interpretExpr' expr >>= addOrUpdateVar name) $> Unit
  Ast.StmtExpression expr -> interpretExpr expr $> Unit

-- TODO : Docs
getLvalueUpdater :: Ast.Lvalue -> Result s (Ast.Identifier, RuntimeValue s, RuntimeValue s -> RuntimeValue s)
getLvalueUpdater = getLvalueUpdater'
  where
    getLvalueUpdater' (Ast.LvalVar name) = (name,,id) <$> getVarValue name
    getLvalueUpdater' (Ast.LvalArrEl _ _) = undefined -- TODO

-- | Statement interpretation result, its either unit (`void`) or some result of the return statement.
data StmtResult s = Unit | Ret (Maybe (RuntimeValue s))
  deriving (Eq)

------------------------------------------------------Expressions-------------------------------------------------------

-- TODO : Docs
interpretExpr :: Ast.Expression -> Result s (Maybe (RuntimeValue s))
interpretExpr expression = case expression of
  Ast.ExprValue value -> interpretExprValue value
  Ast.ExprIdentifier name -> interpretExprIdentifier name
  Ast.ExprUnaryOp unOp expr -> interpretExprUnaryOp unOp expr
  Ast.ExprBinaryOp binOp lhs rhs -> interpretExprBinaryOp binOp lhs rhs
  Ast.ExprArrayAccessByIndex arr i -> interpretExprArrayAccessByIndex arr i
  Ast.ExprFuncCall func args -> interpretExprFuncCall func args

-- TODO : Docs
interpretExprValue :: Ast.Value -> Result s (Maybe (RuntimeValue s))
interpretExprValue value = case value of
  Ast.ValInt v -> return' $ ValInt v
  Ast.ValBool v -> return' $ ValBool v
  Ast.ValString v -> return' $ ValString v
  Ast.ValArray es -> mapM interpretExpr' es >>= return' . ValArray
  Ast.ValFunction v ->
    Just <$> do
      env <- get
      return $ ValFunction v (env ^?! (funcScopes . ix 0))
  where
    return' v = return $ Just v

-- TODO : Docs
interpretExprIdentifier :: Ast.Identifier -> Result s (Maybe (RuntimeValue s))
interpretExprIdentifier name = Just <$> getVarValue name

-- TODO : Docs
interpretExprUnaryOp :: Ast.UnaryOp -> Ast.Expression -> Result s (Maybe (RuntimeValue s))
interpretExprUnaryOp unOp expr =
  interpretExpr' expr >>= valueToPrimitive >>= liftPV . PV.primitiveUnOpApplication unOp

-- TODO : Docs
interpretExprBinaryOp :: Ast.BinaryOp -> Ast.Expression -> Ast.Expression -> Result s (Maybe (RuntimeValue s))
interpretExprBinaryOp Ast.OrOp lhs rhs = do
  lhs' <- interpretBoolExpr lhs
  if lhs'
    then return $ Just $ ValBool True
    else Just . ValBool <$> interpretBoolExpr rhs
interpretExprBinaryOp Ast.AndOp lhs rhs = do
  lhs' <- interpretBoolExpr lhs
  if not lhs'
    then return $ Just $ ValBool False
    else Just . ValBool <$> interpretBoolExpr rhs
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
interpretExprArrayAccessByIndex :: Ast.Expression -> Ast.Expression -> Result s (Maybe (RuntimeValue s))
interpretExprArrayAccessByIndex arr i = do
  arr' <- interpretArrExpr arr
  i' <- interpretIntExpr i
  maybe (throwError $ IndexOutOfRange i' (length arr')) (return . Just) (arr' !? i')

-- TODO : Docs
interpretExprFuncCall :: Ast.Expression -> [Ast.Expression] -> Result s (Maybe (RuntimeValue s))
interpretExprFuncCall func args = do
  (func', fSc) <- interpretFuncExpr func
  args' <- mapM interpretExpr' args
  interpretFunc func' fSc args'

---------------------------------------------------------Utils----------------------------------------------------------

-- * Utils

-- ** Interpret non-void Expression

-- TODO : Docs
interpretIntExpr :: Ast.Expression -> Result s Int
interpretIntExpr = interpretExpr' >=> castToInt

-- TODO : Docs
interpretBoolExpr :: Ast.Expression -> Result s Bool
interpretBoolExpr = interpretExpr' >=> castToBool

-- TODO : Docs
interpretArrExpr :: Ast.Expression -> Result s [RuntimeValue s]
interpretArrExpr = interpretExpr' >=> castToArr

-- TODO : Docs
interpretFuncExpr :: Ast.Expression -> Result s (Ast.Function, FuncScope s)
interpretFuncExpr = interpretExpr' >=> castToFunc

-- TODO : Docs
interpretExpr' :: Ast.Expression -> Result s (RuntimeValue s)
interpretExpr' = interpretExpr >=> unwrapJust

-- ** Primitive values

-- TODO : Docs
valueToPrimitive :: RuntimeValue s -> Result s (PV.PrimitiveValue Int)
valueToPrimitive value = case value of
  ValInt v -> return $ PV.PrimNum v
  ValBool v -> return $ PV.PrimBool v
  ValString v -> return $ PV.PrimString v
  _ -> throwError UnexpectedError

-- TODO : Docs
liftPV :: Either PV.Err (PV.PrimitiveValue Int) -> Result s (Maybe (RuntimeValue s))
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
castToInt :: RuntimeValue s -> Result s Int
castToInt val = case val of
  ValInt int -> return int
  _ -> throwError UnexpectedError

-- TODO : Docs
castToBool :: RuntimeValue s -> Result s Bool
castToBool val = case val of
  ValBool bool -> return bool
  _ -> throwError UnexpectedError

-- TODO : Docs
castToArr :: RuntimeValue s -> Result s [RuntimeValue s]
castToArr val = case val of
  ValArray arr -> return arr
  _ -> throwError UnexpectedError

-- TODO : Docs
castToFunc :: RuntimeValue s -> Result s (Ast.Function, FuncScope s)
castToFunc val = case val of
  ValFunction (Ast.AnonymousFunction f) fSc -> return (f, fSc)
  ValFunction Ast.Nil _ -> throwError Npe
  _ -> throwError UnexpectedError
