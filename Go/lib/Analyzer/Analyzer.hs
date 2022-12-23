{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Analyzer.Analyzer where

import Analyzer.AnalysisResult
import qualified Analyzer.AnalyzedAst as AAst
import qualified Analyzer.AnalyzedType as AType
import Analyzer.AnalyzerRuntime (addNewVar, addOrUpdateVar, getScopeType, getTypeDefault, getVarType, popScope, pushScope)
import Analyzer.ConstExpressionConverters (simplifyConstExpr, simplifyConstIntExpr)
import Control.Monad (when, (>=>))
import Control.Monad.State (MonadState (..), MonadTrans (..), StateT (runStateT), modify)
import Data.Functor (($>))
import Data.List.Extra (anySame, find)
import qualified Data.Map as Map
import Data.Maybe (isNothing)
import qualified Parser.Ast as Ast
import qualified StdLib

-- TODO : main call err

--------------------------------------------------------Analyzer--------------------------------------------------------

-- * Analyzer

-- | Analyzer entry point
analyze :: Ast.Program -> Either Err (AAst.Program, Env)
analyze ast = runStateT (analyzeProgram ast) emptyEnv

-------------------------------------------------Program and functions--------------------------------------------------

analyzeProgram :: Ast.Program -> Result AAst.Program
analyzeProgram (Ast.Program _ functions) = do
  checkForUniqueness $ Ast.funcName <$> functions
  checkForMain functions
  funcs <- analyzeFuncs functions
  case find predicate' funcs of
    Just main -> return $ AAst.Program main [] funcs
    Nothing -> throw UnexpectedError
  where
    checkForUniqueness funcsNames = when (anySame funcsNames) (throw IdentifierRedeclaration)

    checkForMain funcs = when (isNothing $ find predicate funcs) (throw NoMain)

    predicate (Ast.FunctionDef name (Ast.Function (Ast.FunctionSignature params ret) _)) =
      name == "main" && null params && null ret

    predicate' (AAst.FunctionDef name (AAst.Function [] args _)) = name == "main" && null args
    predicate' _ = False

analyzeFuncs :: [Ast.FunctionDef] -> Result [AAst.FunctionDef]
analyzeFuncs functionDefinitions = do
  env <- initEnv functionDefinitions
  put env
  mapM checkFunc' functionDefinitions
  where
    checkFunc' (Ast.FunctionDef name func) = AAst.FunctionDef name <$> (snd <$> analyzeFunc func)

    initEnv funcs = do
      funcs' <- mapM convertToPair funcs
      return $ Env [Scope OrdinaryScope (Map.fromList funcs')]

    convertToPair (Ast.FunctionDef name (Ast.Function (Ast.FunctionSignature params ret) _)) = do
      params' <- mapM analyzeType (snd <$> params)
      ret' <- mapM analyzeType ret
      return (name, AType.TFunction $ AType.FunctionType params' ret')

analyzeFunc :: Ast.Function -> Result (AType.FunctionType, AAst.Function)
analyzeFunc (Ast.Function (Ast.FunctionSignature params ret) stmts) = do
  params' <- mapM (\(name, t) -> (name,) <$> analyzeType t) params
  ret' <- mapM analyzeType ret
  stmts' <- analyzeBlock (Scope OrdinaryScope (Map.fromList params')) stmts ret'
  return (AType.FunctionType (snd <$> params') ret', AAst.Function [] (fst <$> params') stmts')

-------------------------------------------------------Statements-------------------------------------------------------

-- | Analyze statement.
analyzeStmt :: Ast.Statement -> Maybe AType.Type -> Result AAst.Statement
analyzeStmt statement funcReturn = case statement of
  Ast.StmtReturn (Just expr) -> do
    (t, expr') <- analyzeExpr expr
    checkTypesEq t funcReturn
    return $ AAst.StmtReturn (Just expr')
  Ast.StmtReturn Nothing -> do
    checkTypesEq Nothing funcReturn
    return $ AAst.StmtReturn Nothing
  Ast.StmtBreak -> do
    env <- get
    if getScopeType env == Right ForScope
      then return AAst.StmtBreak
      else throw BreakOrContinueInOrdinaryScope
  Ast.StmtContinue -> do
    env <- get
    if getScopeType env == Right ForScope
      then return AAst.StmtContinue
      else throw BreakOrContinueInOrdinaryScope
  Ast.StmtFor (Ast.For kind stmts) ->
    AAst.StmtFor <$> case kind of
      Ast.ForKindFor preStmt cond postStmt -> do
        modify $ pushScope (Scope OrdinaryScope Map.empty)
        preStmt' <- maybe (return Nothing) (fmap Just . analyzeSimpleStmt) preStmt
        cond' <- maybe (return Nothing) (fmap (Just . snd) . analyzeExpr) cond
        postStmt' <- maybe (return Nothing) (fmap Just . analyzeSimpleStmt) postStmt
        stmts' <- analyzeBlock (Scope ForScope Map.empty) stmts funcReturn
        modify popScope
        return $ AAst.For (AAst.ForKindFor preStmt' cond' postStmt') stmts'
      Ast.ForKindWhile cond ->
        AAst.For
          <$> (AAst.ForKindWhile . snd <$> analyzeExpr cond)
          <*> analyzeBlock (Scope ForScope Map.empty) stmts funcReturn
      Ast.ForKindLoop ->
        AAst.For AAst.ForKindLoop
          <$> analyzeBlock (Scope ForScope Map.empty) stmts funcReturn
  Ast.StmtVarDecl varDecl -> case varDecl of
    Ast.VarDecl name (Just t) expr -> do
      (t', expr') <- analyzeExpr expr
      t'' <- analyzeType t
      checkTypesEq (Just t'') t'
      addNewVar name t''
      return $ AAst.StmtVarDecl $ AAst.VarDecl name expr'
    Ast.VarDecl name Nothing expr -> do
      (t, expr') <- analyzeExpr expr
      case t of
        Just t' -> do
          addNewVar name t'
          return $ AAst.StmtVarDecl $ AAst.VarDecl name expr'
        _ -> throw MismatchedTypes
    Ast.DefaultedVarDecl name t -> do
      t' <- analyzeType t
      addNewVar name t'
      return $ AAst.StmtVarDecl $ AAst.VarDecl name (getTypeDefault t')
  Ast.StmtIfElse ifElse -> AAst.StmtIfElse <$> analyzeIfElse ifElse funcReturn
  Ast.StmtBlock stmts -> AAst.StmtBlock <$> analyzeBlock (Scope OrdinaryScope Map.empty) stmts funcReturn
  Ast.StmtSimple simpleStmt -> AAst.StmtSimple <$> analyzeSimpleStmt simpleStmt

analyzeIfElse :: Ast.IfElse -> Maybe AType.Type -> Result AAst.IfElse
analyzeIfElse (Ast.IfElse condition stmts elseStmt) funcReturn = do
  (condT, condExpr) <- analyzeExpr condition
  checkTypesEq condT (Just AType.TBool)
  stmts' <- analyzeBlock (Scope OrdinaryScope Map.empty) stmts funcReturn
  elseStmt' <- case elseStmt of
    Just (Left ifElse) -> Just . Left <$> analyzeIfElse ifElse funcReturn
    Just (Right elseStmt') -> Just . Right <$> analyzeBlock (Scope OrdinaryScope Map.empty) elseStmt' funcReturn
    Nothing -> return Nothing
  return $ AAst.IfElse condExpr stmts' elseStmt'

analyzeBlock :: Scope -> [Ast.Statement] -> Maybe AType.Type -> Result [AAst.Statement]
analyzeBlock scope stmts ret = do
  modify $ pushScope scope
  stmts'' <- mapM (`analyzeStmt` ret) stmts
  modify popScope
  return stmts''

analyzeSimpleStmt :: Ast.SimpleStmt -> Result AAst.SimpleStmt
analyzeSimpleStmt simpleStmt = case simpleStmt of
  Ast.StmtAssignment updEl expr -> do
    (updElT, updEl') <- analyzeUpdEl updEl
    (t, expr') <- analyzeExpr expr
    checkTypesEq (Just updElT) t
    return $ AAst.StmtAssignment updEl' expr'
  Ast.StmtInc updEl -> do
    (updElT, updEl') <- analyzeUpdEl updEl
    checkTypesEq (Just updElT) (Just AType.TInt)
    return $ AAst.StmtInc updEl'
  Ast.StmtDec updEl -> do
    (updElT, updEl') <- analyzeUpdEl updEl
    checkTypesEq (Just updElT) (Just AType.TInt)
    return $ AAst.StmtDec updEl'
  Ast.StmtShortVarDecl name expr -> do
    (t, expr') <- analyzeExpr expr
    case t of
      Just t' -> addOrUpdateVar name t' $> AAst.StmtShortVarDecl name expr'
      _ -> throw MismatchedTypes
  Ast.StmtExpression expr -> do
    (_, expr') <- analyzeExpr expr
    return $ AAst.StmtExpression expr'

analyzeUpdEl :: Ast.UpdatableElement -> Result (AType.Type, AAst.UpdatableElement)
analyzeUpdEl updEl = case updEl of
  Ast.UpdVar name -> (,AAst.UpdVar name) <$> getVarType name
  Ast.UpdArrEl name indexExprs -> do
    varT <- getVarType name
    indexExprs' <- mapM (analyzeExpr >=> \(t, expr) -> checkTypesEq (Just AType.TInt) t $> expr) indexExprs
    let calculatedType =
          let getArrayElementType t dimCnt = case t of
                AType.TArray t' _ | dimCnt > 0 -> getArrayElementType t' (dimCnt - 1)
                _ | dimCnt == 0 -> Just t
                _ -> Nothing
           in getArrayElementType varT (length indexExprs)
    maybe (throw MismatchedTypes) (return . (,AAst.UpdArrEl name indexExprs')) calculatedType

------------------------------------------------------Expressions-------------------------------------------------------

analyzeExpr :: Ast.Expression -> Result (Maybe AType.Type, AAst.Expression)
analyzeExpr expression = case simplifyConstExpr expression of
  Right res -> return res
  Left NotInIntBounds -> throw NotInIntBounds
  Left _ -> nonConstExprResult
  where
    nonConstExprResult = case expression of
      Ast.ExprValue val ->
        let return' t expr = return (Just t, AAst.ExprValue expr)
         in case val of
              Ast.ValInt v -> return' AType.TInt $ AAst.ValInt $ fromIntegral v
              Ast.ValBool v -> return' AType.TBool $ AAst.ValBool v
              Ast.ValString v -> return' AType.TString $ AAst.ValString v
              Ast.ValArray (Ast.ArrayValue t els) -> do
                t'@(elT, len) <- analyzeArrayType t
                els' <- mapM analyzeExpr els
                if length els' <= len then return () else throw MismatchedTypes
                mapM_ (checkTypesEq (Just elT)) (fst <$> els')
                return' (uncurry AType.TArray t') $ AAst.ValArray $ replicate (len - length els') (getTypeDefault elT) ++ (snd <$> els')
              Ast.ValFunction (Ast.AnonymousFunction function) -> do
                (t, f) <- analyzeFunc function
                return' (AType.TFunction t) $ AAst.ValFunction (AAst.AnonymousFunction f)
              Ast.ValFunction Ast.Nil -> return' AType.TNil $ AAst.ValFunction AAst.Nil
      Ast.ExprIdentifier name -> do
        t <- getVarType name
        return (Just t, AAst.ExprIdentifier name)
      Ast.ExprUnaryOp unOp expr -> do
        (t, expr') <- analyzeExpr expr
        let return' = return (t, AAst.ExprUnaryOp unOp expr')
        let throw' = throw MismatchedTypes
        case t of
          Just t' -> case (unOp, t') of
            (Ast.UnaryPlusOp, AType.TInt) -> return'
            (Ast.UnaryMinusOp, AType.TInt) -> return'
            (Ast.NotOp, AType.TBool) -> return'
            _ -> throw'
          _ -> throw'
      Ast.ExprBinaryOp binOp lhs rhs -> do
        (lhsT, lhs') <- analyzeExpr lhs
        (rhsT, rhs') <- analyzeExpr rhs
        let return' t = return (Just t, AAst.ExprBinaryOp binOp lhs' rhs')
        let returnInt = return' AType.TInt
        let returnBool = return' AType.TBool
        let returnString = return' AType.TString
        let throw' = throw MismatchedTypes
        case (lhsT, rhsT) of
          (Just lhsT', Just rhsT') | lhsT' == rhsT' -> case (binOp, lhsT') of
            (Ast.OrOp, AType.TBool) -> returnBool
            (Ast.AndOp, AType.TBool) -> returnBool
            (Ast.EqOp, _) -> returnBool
            (Ast.NeOp, _) -> returnBool
            (Ast.LeOp, AType.TInt) -> returnBool
            (Ast.LtOp, AType.TInt) -> returnBool
            (Ast.MeOp, AType.TInt) -> returnBool
            (Ast.MtOp, AType.TInt) -> returnBool
            (Ast.LeOp, AType.TString) -> returnBool
            (Ast.LtOp, AType.TString) -> returnBool
            (Ast.MeOp, AType.TString) -> returnBool
            (Ast.MtOp, AType.TString) -> returnBool
            (Ast.PlusOp, AType.TInt) -> returnInt
            (Ast.PlusOp, AType.TString) -> returnString
            (Ast.MinusOp, AType.TInt) -> returnInt
            (Ast.MultOp, AType.TInt) -> returnInt
            (Ast.DivOp, AType.TInt) -> returnInt
            (Ast.ModOp, AType.TInt) -> returnInt
            _ -> throw'
          _ -> throw'
      Ast.ExprFuncCall func args -> do
        (funcT, func') <- analyzeExpr func
        case funcT of
          Just (AType.TFunction (AType.FunctionType paramsTs retT)) -> do
            args' <- mapM analyzeExpr args
            args'' <- mapM (\(eT, (aT, expr)) -> checkTypesEq (Just eT) aT $> expr) (paramsTs `zip` args')
            return (retT, AAst.ExprFuncCall func' args'')
          _ -> throw MismatchedTypes
      Ast.ExprArrayAccessByIndex arr index -> do
        (aT, a) <- analyzeExpr arr
        (iT, i) <- analyzeExpr index
        case (aT, iT) of
          (Just (AType.TArray elT _), Just AType.TInt) -> return (Just elT, AAst.ExprArrayAccessByIndex a i)
          _ -> throw MismatchedTypes
      Ast.ExprLenFuncCall arg -> do
        (argT, argE) <- analyzeExpr arg
        let return' = return (Just AType.TInt, AAst.ExprFuncCall (stdLibFuncExpr $ StdLib.name StdLib.lenFunction) [argE])
        case argT of
          Just AType.TString -> return'
          Just (AType.TArray _ _) -> return'
          _ -> throw MismatchedTypes
      Ast.ExprPrintlnFuncCall maybeArg -> case maybeArg of
        Just arg -> do
          (argT, argE) <- analyzeExpr arg
          let return' = return (Nothing, AAst.ExprFuncCall (stdLibFuncExpr $ StdLib.name StdLib.printlnFunction) [argE])
          case argT of
            Just AType.TInt -> return'
            Just AType.TBool -> return'
            Just AType.TString -> return'
            Just (AType.TArray _ _) -> return'
            _ -> throw MismatchedTypes
        Nothing -> return (Nothing, AAst.ExprFuncCall (stdLibFuncExpr $ StdLib.name StdLib.printlnFunction) [])
      Ast.ExprPanicFuncCall arg -> do
        (argT, argE) <- analyzeExpr arg
        checkTypesEq (Just AType.TString) argT
        return (Nothing, AAst.ExprFuncCall (stdLibFuncExpr $ StdLib.name StdLib.panicFunction) [argE])

stdLibFuncExpr :: AAst.Identifier -> AAst.Expression
stdLibFuncExpr name = AAst.ExprValue $ AAst.ValFunction $ AAst.AnonymousFunction $ AAst.StdLibFunction name

checkTypesEq :: Maybe AType.Type -> Maybe AType.Type -> Result ()
checkTypesEq lhs rhs = if lhs == rhs then return () else throw MismatchedTypes

---------------------------------------------------------Types----------------------------------------------------------

analyzeType :: Ast.Type -> Result AType.Type
analyzeType t = case t of
  Ast.TInt -> return AType.TInt
  Ast.TBool -> return AType.TBool
  Ast.TString -> return AType.TString
  Ast.TArray arrType -> uncurry AType.TArray <$> analyzeArrayType arrType
  Ast.TFunction funcType -> AType.TFunction <$> analyzeFunctionType funcType

analyzeArrayType :: Ast.ArrayType -> Result (AType.Type, Int)
analyzeArrayType (Ast.ArrayType elementT len) = (,) <$> analyzeType elementT <*> lift (simplifyConstIntExpr len)

analyzeFunctionType :: Ast.FunctionType -> Result AType.FunctionType
analyzeFunctionType (Ast.FunctionType paramsTs retT) =
  AType.FunctionType <$> mapM analyzeType paramsTs <*> mapM analyzeType retT
