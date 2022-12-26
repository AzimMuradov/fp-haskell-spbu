{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Interpreter.Interpreter where

import qualified Analyzer.AnalyzedAst as Ast
import Control.Lens
import Control.Monad (void, (>=>))
import Control.Monad.Except (MonadError (throwError), liftEither, runExceptT)
import Control.Monad.State (modify, runState)
import Data.Either.Combinators (leftToMaybe)
import Data.Functor (($>))
import Data.List.Extra ((!?))
import qualified Data.Map as Map
import Data.Text (Text, append, intercalate, pack)
import Errors (todo')
import Interpreter.InterpretationResult
import Interpreter.InterpreterRuntime
import Interpreter.RuntimeValue (RuntimeValue (..))
import StdLib (stdLibFunctionsMap)

------------------------------------------------------Interpreter-------------------------------------------------------

-- | Interpreter entry point. Assumes that program is checked.
interpret :: Ast.Program -> (ResultValue (), Env)
interpret ast = runState (runExceptT (interpretProgram ast)) emptyEnv & _2 . accumulatedOutput %~ reverse

-- TODO : Docs
getInterpretationOut :: (ResultValue (), Env) -> (Text, Maybe Text)
getInterpretationOut (result, env) =
  (intercalate "" $ env ^. accumulatedOutput, pack . show <$> leftToMaybe result)

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
interpretStmtForGoTo = todo' -- TODO

-- TODO : Docs
interpretStmtFor :: Ast.For -> Result StmtResult
interpretStmtFor = todo' -- TODO

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
  Ast.StmtInc updEl -> do
    (n, v, f) <- getUpdElF updEl
    v' <- castToInt v
    updateVar n (f (ValInt (v' + 1)))
    return Unit
  Ast.StmtDec updEl -> do
    (n, v, f) <- getUpdElF updEl
    v' <- castToInt v
    updateVar n (f (ValInt (v' - 1)))
    return Unit
  Ast.StmtShortVarDecl name expr -> (interpretExpr' expr >>= addOrUpdateVar name) $> Unit
  Ast.StmtExpression expr -> interpretExpr expr $> Unit

-- TODO : Docs
getUpdElF :: Ast.UpdatableElement -> Result (Ast.Identifier, RuntimeValue, RuntimeValue -> RuntimeValue)
getUpdElF = getUpdElF'
  where
    getUpdElF' (Ast.UpdVar name) = (name,,id) <$> getVarValue name
    getUpdElF' (Ast.UpdArrEl _ _) = todo' -- TODO

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
interpretExprUnaryOp unOp expr = do
  val <- interpretExpr' expr
  case (unOp, val) of
    (Ast.UnaryPlusOp, ValInt v) -> returnInt v
    (Ast.UnaryMinusOp, ValInt v) -> returnInt $ -v
    (Ast.NotOp, ValBool v) -> returnBool $ not v
    _ -> throwError UnexpectedError
  where
    return' val = return $ Just val
    returnInt val = return' $ ValInt val
    returnBool val = return' $ ValBool val

-- TODO : Docs
interpretExprBinaryOp :: Ast.BinaryOp -> Ast.Expression -> Ast.Expression -> Result (Maybe RuntimeValue)
interpretExprBinaryOp Ast.OrOp lhs rhs =
  Just . ValBool <$> ((||) <$> interpretBoolExpr lhs <*> interpretBoolExpr rhs)
interpretExprBinaryOp Ast.AndOp lhs rhs = do
  Just . ValBool <$> ((&&) <$> interpretBoolExpr lhs <*> interpretBoolExpr rhs)
interpretExprBinaryOp binOp lhs rhs = do
  lhsVal <- interpretExpr' lhs
  rhsVal <- interpretExpr' rhs
  case (lhsVal, rhsVal, binOp) of
    (lhsVal', rhsVal', Ast.EqOp) -> returnBool $ lhsVal' == rhsVal'
    (lhsVal', rhsVal', Ast.NeOp) -> returnBool $ lhsVal' /= rhsVal'
    (ValInt lhs', ValInt rhs', _) -> case binOp of
      Ast.LeOp -> returnBool $ lhs' <= rhs'
      Ast.LtOp -> returnBool $ lhs' < rhs'
      Ast.MeOp -> returnBool $ lhs' >= rhs'
      Ast.MtOp -> returnBool $ lhs' > rhs'
      Ast.PlusOp -> returnInt $ lhs' + rhs'
      Ast.MinusOp -> returnInt $ lhs' - rhs'
      Ast.MultOp -> returnInt $ lhs' * rhs'
      Ast.DivOp -> if rhs' /= 0 then returnInt $ lhs' `div` rhs' else throwError DivisionByZero
      Ast.ModOp -> if rhs' /= 0 then returnInt $ lhs' `mod` rhs' else throwError DivisionByZero
    (ValString lhs', ValString rhs', _) -> case binOp of
      Ast.LeOp -> returnBool $ lhs' <= rhs'
      Ast.LtOp -> returnBool $ lhs' < rhs'
      Ast.MeOp -> returnBool $ lhs' >= rhs'
      Ast.MtOp -> returnBool $ lhs' > rhs'
      Ast.PlusOp -> returnString $ append lhs' rhs'
      _ -> throwError UnexpectedError
    _ -> throwError UnexpectedError
  where
    return' val = return $ Just val
    returnInt val = return' $ ValInt val
    returnBool val = return' $ ValBool val
    returnString val = return' $ ValString val

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
