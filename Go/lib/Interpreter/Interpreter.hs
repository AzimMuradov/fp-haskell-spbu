{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Interpreter.Interpreter where

import qualified Analyzer.AnalyzedAst as Ast
import Control.Lens (Ixed (ix), (%~), (&), (.~), (^.), (^?!))
import Control.Monad (foldM, void, (>=>))
import Control.Monad.Except (MonadError (throwError), liftEither, runExceptT)
import Control.Monad.ST (runST)
import Control.Monad.State (MonadState (get), StateT (runStateT), modify)
import Data.Either.Combinators (leftToMaybe, mapBoth)
import Data.Functor (($>))
import Data.List.Extra ((!?))
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.STRef (newSTRef, readSTRef)
import Data.Text (Text, pack)
import qualified Data.Text as T
import Interpreter.Result
import Interpreter.Runtime
import qualified PrimitiveValue as PV
import StdLib (stdLibFunctionsMap)

------------------------------------------------------Interpreter-------------------------------------------------------

-- * Interpreter

-- | Interpreter entry point. Assumes that program is analyzed successfully.
interpret :: Ast.Program -> (ResultValue (), Env')
interpret ast = runST $ do
  (res, env) <- runStateT (runExceptT (interpretProgram ast)) emptyEnv
  env' <- evalEnv (env & accumulatedOutput %~ reverse)
  return (res, env')

getInterpretationOut :: (ResultValue (), Env') -> (Text, Maybe Text)
getInterpretationOut (result, env) = (T.concat $ env ^. accumulatedOutput', pack . show <$> leftToMaybe result)

-------------------------------------------------Program and functions--------------------------------------------------

interpretProgram :: Ast.Program -> Result s ()
interpretProgram (Ast.Program tlVarDecls tlFuncDefs) = do
  fs' <- fs
  modify $ funcs .~ fs'
  (main, mainFs) <- unwrapJust $ fs' Map.!? "main"
  main' <- lift2 $ readSTRef main
  void $ interpretFunc main' mainFs []
  where
    globalScope = do
      vsRefs <- mapM (interpretExpr' >=> lift2 . newSTRef) (Ast.value <$> tlVarDecls)
      return $ Scope $ Map.fromList $ (Ast.identifier <$> tlVarDecls) `zip` vsRefs

    fs = do
      globalScope' <- globalScope
      fsRefs <- lift2 $ mapM newSTRef (Ast.func <$> tlFuncDefs)
      return $ Map.fromList $ (Ast.funcName <$> tlFuncDefs) `zip` ((,globalScope') <$> fsRefs)

interpretFunc :: Ast.Function -> Scope s -> [RuntimeValue s] -> Result s (Maybe (RuntimeValue s))
interpretFunc (Ast.OrdinaryFunction params body voidMark) (Scope ns) args = do
  args' <- lift2 $ mapM newSTRef (Map.fromList $ params `zip` args)
  res <- interpretFuncBlock (FuncScope [Scope args', Scope ns]) body
  case (res, voidMark) of
    (Ret val, _) -> return val
    (Unit, Ast.VoidFunc) -> return Nothing
    (Unit, Ast.NonVoidFunc) -> throwError NoReturn
    _ -> throwError UnexpectedError
interpretFunc (Ast.StdLibFunction name) _ args = do
  func <- unwrapJust $ stdLibFunctionsMap Map.!? name
  (res, out) <- liftEither $ func $ evalRuntimeValue <$> args
  modify $ accumulatedOutput %~ (out :)
  return $ unevalRuntimeValue <$> res

interpretFuncBlock :: FuncScope s -> Ast.Block -> Result s (StmtResult s)
interpretFuncBlock initScope = interpretBlock' initScope pushFuncScope popFuncScope

interpretBlock :: Scope s -> Ast.Block -> Result s (StmtResult s)
interpretBlock initScope = interpretBlock' initScope pushBlockScope popBlockScope

interpretBlock' :: scope -> (scope -> Env s -> Env s) -> (Env s -> Env s) -> Ast.Block -> Result s (StmtResult s)
interpretBlock' initScope pushScope popScope block = do
  modify $ pushScope initScope
  res <- foldl f (return Unit) block
  modify popScope
  return res
  where
    f res stmt = res >>= \r -> if r == Unit then interpretStmt stmt else return r

-------------------------------------------------------Statements-------------------------------------------------------

interpretStmt :: Ast.Statement -> Result s (StmtResult s)
interpretStmt = \case
  Ast.StmtReturn expr -> interpretStmtReturn expr
  Ast.StmtForGoTo goto -> interpretStmtForGoTo goto
  Ast.StmtFor for -> interpretStmtFor for
  Ast.StmtVarDecl varDecl -> interpretStmtVarDecl varDecl
  Ast.StmtIfElse ifElse -> interpretStmtIfElse ifElse
  Ast.StmtBlock block -> interpretBlock emptyScope block
  Ast.StmtSimple simpleStmt -> interpretStmtSimple simpleStmt

interpretStmtReturn :: Maybe Ast.Expression -> Result s (StmtResult s)
interpretStmtReturn = maybe (return $ Ret Nothing) (fmap Ret . interpretExpr)

interpretStmtForGoTo :: Ast.ForGoTo -> Result s (StmtResult s)
interpretStmtForGoTo = \case
  Ast.Break -> return Break
  Ast.Continue -> return Continue

interpretStmtFor :: Ast.For -> Result s (StmtResult s)
interpretStmtFor (Ast.For kind block) = case kind of
  Ast.ForKindFor pre cond post -> for pre cond post block
  Ast.ForKindWhile cond -> for Nothing (Just cond) Nothing block
  Ast.ForKindLoop -> for Nothing Nothing Nothing block
  where
    for :: Maybe Ast.SimpleStmt -> Maybe Ast.Expression -> Maybe Ast.SimpleStmt -> Ast.Block -> Result s (StmtResult s)
    for pre cond post b = do
      modify $ pushBlockScope emptyScope
      mapM_ interpretStmtSimple pre
      res <- for' cond post b
      modify popBlockScope
      return res
    for' :: Maybe Ast.Expression -> Maybe Ast.SimpleStmt -> Ast.Block -> Result s (StmtResult s)
    for' cond post b = do
      cond' <- fromMaybe True <$> mapM interpretBoolExpr cond
      if cond'
        then do
          res <- interpretBlock emptyScope b
          case res of
            Unit -> mapM_ interpretStmtSimple post >> for' cond post b
            Break -> mapM_ interpretStmtSimple post $> Unit
            Continue -> mapM_ interpretStmtSimple post >> for' cond post b
            Ret _ -> mapM_ interpretStmtSimple post $> res
        else return Unit

interpretStmtVarDecl :: Ast.VarDecl -> Result s (StmtResult s)
interpretStmtVarDecl (Ast.VarDecl name expr) = (interpretExpr' expr >>= addNewVar name) $> Unit

interpretStmtIfElse :: Ast.IfElse -> Result s (StmtResult s)
interpretStmtIfElse (Ast.IfElse condition block elseStmt) = do
  cond <- interpretBoolExpr condition
  if cond
    then interpretBlock emptyScope block
    else case elseStmt of
      Ast.NoElse -> return Unit
      Ast.Else block' -> interpretBlock emptyScope block'
      Ast.Elif ifElse -> interpretStmtIfElse ifElse

interpretStmtSimple :: Ast.SimpleStmt -> Result s (StmtResult s)
interpretStmtSimple = \case
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

getLvalueUpdater :: Ast.Lvalue -> Result s (Ast.Identifier, RuntimeValue s, RuntimeValue s -> RuntimeValue s)
getLvalueUpdater (Ast.LvalVar name) = (name,,id) <$> getVarValue name
getLvalueUpdater (Ast.LvalArrEl name indices) = do
  arr <- getVarValue name
  indices' <- mapM interpretIntExpr indices
  (value, accessor) <- foldM helper (arr, \_ v -> v) indices'
  return (name, value, accessor arr)
  where
    helper ::
      (RuntimeValue s, RuntimeValue s -> RuntimeValue s -> RuntimeValue s) ->
      Int ->
      Result s (RuntimeValue s, RuntimeValue s -> RuntimeValue s -> RuntimeValue s)
    helper (arr, replacer) i = do
      arr' <- castToArr arr
      let arrEl = arr' ^?! ix i
      return (arrEl, \oldArr v -> replacer oldArr (ValArray (arr' & ix i .~ v)))

-- | Statement interpretation result.
data StmtResult s
  = -- | Result of ordinary statement.
    Unit
  | -- | Result of the @break@ statement.
    Break
  | -- | Result of the @continue@ statement.
    Continue
  | -- | Result of the @return@ statement.
    Ret (Maybe (RuntimeValue s))
  deriving (Eq)

------------------------------------------------------Expressions-------------------------------------------------------

interpretExpr :: Ast.Expression -> Result s (Maybe (RuntimeValue s))
interpretExpr = \case
  Ast.ExprValue value -> interpretExprValue value
  Ast.ExprIdentifier name -> interpretExprIdentifier name
  Ast.ExprUnaryOp unOp expr -> interpretExprUnaryOp unOp expr
  Ast.ExprBinaryOp binOp lhs rhs -> interpretExprBinaryOp binOp lhs rhs
  Ast.ExprArrayAccessByIndex arr i -> interpretExprArrayAccessByIndex arr i
  Ast.ExprFuncCall func args -> interpretExprFuncCall func args

interpretExprValue :: Ast.Value -> Result s (Maybe (RuntimeValue s))
interpretExprValue = \case
  Ast.ValInt v -> return' $ ValInt v
  Ast.ValBool v -> return' $ ValBool v
  Ast.ValString v -> return' $ ValString v
  Ast.ValArray es -> mapM interpretExpr' es >>= return' . ValArray
  Ast.ValFunction v ->
    Just <$> do
      env <- get
      return $ ValFunction v $ flattenFuncScope (env ^?! (funcScopes . ix 0))
  where
    return' v = return $ Just v

interpretExprIdentifier :: Ast.Identifier -> Result s (Maybe (RuntimeValue s))
interpretExprIdentifier name = Just <$> getVarValue name

interpretExprUnaryOp :: Ast.UnaryOp -> Ast.Expression -> Result s (Maybe (RuntimeValue s))
interpretExprUnaryOp unOp expr =
  interpretExpr' expr >>= valueToPrimitive >>= liftPV . PV.primitiveUnOpApplication unOp

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

interpretExprArrayAccessByIndex :: Ast.Expression -> Ast.Expression -> Result s (Maybe (RuntimeValue s))
interpretExprArrayAccessByIndex arr i = do
  arr' <- interpretArrExpr arr
  i' <- interpretIntExpr i
  maybe (throwError $ IndexOutOfRange i' (length arr')) (return . Just) (arr' !? i')

interpretExprFuncCall :: Ast.Expression -> [Ast.Expression] -> Result s (Maybe (RuntimeValue s))
interpretExprFuncCall func args = do
  (func', sc) <- interpretFuncExpr func
  args' <- mapM interpretExpr' args
  interpretFunc func' sc args'

---------------------------------------------------------Utils----------------------------------------------------------

-- * Utils

-- ** Interpret non-void Expression

interpretIntExpr :: Ast.Expression -> Result s Int
interpretIntExpr = interpretExpr' >=> castToInt

interpretBoolExpr :: Ast.Expression -> Result s Bool
interpretBoolExpr = interpretExpr' >=> castToBool

interpretArrExpr :: Ast.Expression -> Result s [RuntimeValue s]
interpretArrExpr = interpretExpr' >=> castToArr

interpretFuncExpr :: Ast.Expression -> Result s (Ast.Function, Scope s)
interpretFuncExpr = interpretExpr' >=> castToFunc

interpretExpr' :: Ast.Expression -> Result s (RuntimeValue s)
interpretExpr' = interpretExpr >=> unwrapJust

unwrapJust :: Maybe a -> Result s a
unwrapJust = maybe (throwError UnexpectedError) return

-- ** Primitive values

valueToPrimitive :: RuntimeValue s -> Result s (PV.PrimitiveValue Int)
valueToPrimitive = \case
  ValInt v -> return $ PV.PrimNum v
  ValBool v -> return $ PV.PrimBool v
  ValString v -> return $ PV.PrimString v
  _ -> throwError UnexpectedError

liftPV :: Either PV.Err (PV.PrimitiveValue Int) -> Result s (Maybe (RuntimeValue s))
liftPV pvRes = liftEither $ mapBoth mapErr primitiveToValue pvRes
  where
    mapErr = \case
      PV.MismatchedTypes -> UnexpectedError
      PV.DivisionByZero -> DivisionByZero

    primitiveToValue = \case
      PV.PrimNum v -> Just $ ValInt v
      PV.PrimBool v -> Just $ ValBool v
      PV.PrimString v -> Just $ ValString v

-- ** Cast runtime values

castToInt :: RuntimeValue s -> Result s Int
castToInt = \case
  ValInt int -> return int
  _ -> throwError UnexpectedError

castToBool :: RuntimeValue s -> Result s Bool
castToBool = \case
  ValBool bool -> return bool
  _ -> throwError UnexpectedError

castToArr :: RuntimeValue s -> Result s [RuntimeValue s]
castToArr = \case
  ValArray arr -> return arr
  _ -> throwError UnexpectedError

castToFunc :: RuntimeValue s -> Result s (Ast.Function, Scope s)
castToFunc = \case
  ValFunction (Ast.Function f) sc -> return (f, sc)
  ValFunction Ast.Nil _ -> throwError Npe
  _ -> throwError UnexpectedError
