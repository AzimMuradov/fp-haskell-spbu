{-# LANGUAGE TupleSections #-}

module Interpreter.Interpreter where

import Parser.Ast
import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT)
import Control.Monad.State (MonadState (get), State, evalState, gets, modify, runState, void)
import Data.List.Extra (uncons, unsnoc)
import Data.Maybe (fromJust)
import Interpreter.Runtime
import Numeric.IEEE (nan)

type Interp a = ExceptT InterpError (State Env) a

data InterpError = DivisionByZero | Unreachable deriving (Show)

interp :: Program -> String
interp (Program stmts) = show $ flip runState [] $ runExceptT $ do
  modify pushScope
  let (xs, x) = fromJust $ unsnoc stmts
  mapM_ statementI xs
  res <- case x of
    SExpr e -> exprI e
    _ -> undefined
  modify popScope
  return res

statementI :: Statement -> Interp ()
statementI (SExpr expr) = void $ exprI expr
statementI (SMeasureDecl _) = return ()
statementI (SVarDecl (VarDecl (ident, _) xdef)) = do
  val <- blockI xdef
  modify $ addVar ident val
statementI (SFunDecl (FunDecl ident xdef)) =
  modify $ addVar ident (convertFunToIValue xdef)
statementI (SRecFunDecl (RecFunDecl ident xdef)) =
  modify $ addVar ident (convertFunToIValue xdef)

exprI :: Expr -> Interp IValue
exprI expr = case expr of
  EIdentifier ident -> gets $ getVar ident
  EValue value -> return $ convertValueToIValue value
  EOperations op -> opI op
  EApplication func arg -> applicationI func arg
  EIf cond thenB elseB -> ifI cond thenB elseB
  ELetInV (var, _) xdef body -> letInVI var xdef body
  ELetInF func xdef body -> letInFI func xdef body

convertValueToIValue :: Value -> IValue
convertValueToIValue val = case val of
  VBool b -> IVBool b
  VInt v _ -> IVInt v
  VDouble v _ -> IVDouble v
  VFun f -> convertFunToIValue f

convertFunToIValue :: Fun -> IValue
convertFunToIValue (Fun args block) = IVFun (fst <$> args) block

opI :: Operations -> Interp IValue
opI oper = case oper of
  BooleanOp (AndOp left right) -> do
    l <- boolExprI left
    r <- boolExprI right
    return $ IVBool (l && r)
  BooleanOp (OrOp left right) -> do
    l <- boolExprI left
    r <- boolExprI right
    return $ IVBool (l || r)
  NotOp expr -> do
    e <- boolExprI expr
    return $ IVBool (not e)
  ArithmeticOp (PlusOp left right) -> do
    l <- exprI left
    r <- exprI right
    return $ l + r
  ArithmeticOp (MinusOp left right) -> do
    l <- exprI left
    r <- exprI right
    return $ l - r
  ArithmeticOp (MulOp left right) -> do
    l <- exprI left
    r <- exprI right
    return $ l * r
  ArithmeticOp (DivOp left right) -> do
    l <- exprI left
    r <- exprI right
    case (l, r) of
      (IVInt lv, IVInt rv) ->
        if rv /= 0
          then return $ IVInt $ lv `div` rv
          else throwError DivisionByZero
      (IVDouble lv, IVDouble rv) -> return $ IVDouble $ lv / rv
      _ -> throwError Unreachable
  ArithmeticOp (ModOp left right) -> do
    l <- exprI left
    r <- exprI right
    case (l, r) of
      (IVInt lv, IVInt rv) ->
        if rv /= 0
          then return $ IVInt $ lv `mod` rv
          else throwError DivisionByZero
      (IVDouble lv, IVDouble rv) ->
        if rv /= 0.0
          then return $ IVDouble $ lv - (lv / rv)
          else return $ IVDouble nan
      _ -> throwError Unreachable
  ArithmeticOp (ExpOp left right) -> do
    l <- exprI left
    r <- exprI right
    case (l, r) of
      (IVInt lv, IVInt rv) -> return $ IVInt $ lv ^ rv
      (IVDouble lv, IVDouble rv) -> return $ IVDouble $ lv ** rv
      _ -> throwError Unreachable
  ComparisonOp (EqOp left right) -> compExprI left right (==)
  ComparisonOp (NeOp left right) -> compExprI left right (/=)
  ComparisonOp (LtOp left right) -> compExprI left right (<)
  ComparisonOp (LeOp left right) -> compExprI left right (<=)
  ComparisonOp (MtOp left right) -> compExprI left right (>)
  ComparisonOp (MeOp left right) -> compExprI left right (>=)

compExprI :: Expr -> Expr -> (IValue -> IValue -> Bool) -> Interp IValue
compExprI l r op = do
  l' <- exprI l
  r' <- exprI r
  return $ IVBool $ op l' r'

applicationI :: Expr -> Expr -> Interp IValue
applicationI func arg = do
  (params, body) <- funcI func
  let (p, ps) = fromJust $ uncons params
  arg' <- exprI arg
  let val = EValue $ case arg' of
        IVBool v -> VBool v
        IVInt v -> VInt v Nothing
        IVDouble v -> VDouble v Nothing
        IVFun args block -> VFun $ Fun ((,Nothing) <$> args) block
  let body' = evalState (replaceBody p val body) [False]
  if null ps
    then blockI body'
    else return $ IVFun ps body'
  where
    funcI expr = do
      v <- exprI expr
      case v of
        IVFun a b -> return (a, b)
        _ -> throwError Unreachable

    replaceBody :: Identifier -> Expr -> [Expr] -> State [Bool] [Expr]
    replaceBody p val exprs = do
      modify (False :)
      res <- mapM (replaceExpr p val) exprs
      modify tail
      return res

    replaceExpr :: Identifier -> Expr -> Expr -> State [Bool] Expr
    replaceExpr p val expr = do
      ctx <- get
      if all not ctx
        then case expr of
          (EIdentifier x) | x == p -> return val
          (EIdentifier _) -> return expr
          (EValue (VFun (Fun args fbody))) -> do
            body <- replaceBody p val fbody
            return $ EValue (VFun (Fun args body))
          (EValue _) -> return expr
          (EOperations (BooleanOp op)) -> do
            bL' <- replaceExpr p val (bL op)
            bR' <- replaceExpr p val (bR op)
            return $ EOperations $ BooleanOp $ op {bL = bL', bR = bR'}
          (EOperations (NotOp e)) -> do
            e' <- replaceExpr p val e
            return $ EOperations $ NotOp e'
          (EOperations (ArithmeticOp op)) -> do
            aL' <- replaceExpr p val (aL op)
            aR' <- replaceExpr p val (aR op)
            return $ EOperations $ ArithmeticOp $ op {aL = aL', aR = aR'}
          (EOperations (ComparisonOp op)) -> do
            cL' <- replaceExpr p val (cL op)
            cR' <- replaceExpr p val (cR op)
            return $ EOperations $ ComparisonOp $ op {cL = cL', cR = cR'}
          (EApplication func arg) -> EApplication <$> replaceExpr p val func <*> replaceExpr p val arg
          (EIf cond thenB elseB) -> EIf <$> replaceExpr p val cond <*> replaceBody p val thenB <*> replaceBody p val elseB
          (ELetInV (var, t) xdef body) -> do
            if var == p then modify (True :) else modify (False :)
            xdef' <- replaceBody p val xdef
            body' <- replaceBody p val body
            modify tail
            return $ ELetInV (var, t) xdef' body'
          (ELetInF func (Fun params fbody) body) -> do
            if func == p then modify (True :) else modify (False :)
            if p `elem` (fst <$> params) then modify (True :) else modify (False :)
            fbody' <- replaceBody p val fbody
            modify tail
            body' <- replaceBody p val body
            modify tail
            return $ ELetInF func (Fun params fbody') body'
        else return expr

ifI :: Expr -> [Expr] -> [Expr] -> Interp IValue
ifI cond thenB elseB = do
  c <- boolExprI cond
  blockI $ if c then thenB else elseB

letInVI :: Identifier -> [Expr] -> [Expr] -> Interp IValue
letInVI var xdef body = do
  modify pushScope
  val <- blockI xdef
  modify $ addVar var val
  res <- blockI body
  modify popScope
  return res

letInFI :: Identifier -> Fun -> [Expr] -> Interp IValue
letInFI func xdef body = do
  modify pushScope
  modify $ addVar func (convertFunToIValue xdef)
  res <- blockI body
  modify popScope
  return res

blockI :: [Expr] -> Interp IValue
blockI exprs = do
  modify pushScope
  let (xs, x) = fromJust $ unsnoc exprs
  mapM_ exprI xs
  res <- exprI x
  modify popScope
  return res

boolExprI :: Expr -> Interp Bool
boolExprI expr = do
  v <- exprI expr
  case v of
    IVBool b -> return b
    _ -> throwError Unreachable