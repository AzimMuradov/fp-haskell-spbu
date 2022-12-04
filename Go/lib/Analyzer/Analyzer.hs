{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Analyzer.Analyzer where

import Analyzer.AnalysisResult
import qualified Analyzer.AnalyzedAst as AAst
import qualified Analyzer.AnalyzedType as AType
import Analyzer.AnalyzerRuntime (addNewVar, addOrUpdateVar, getTypeDefault, getVarType)
import Analyzer.ConstExpressionConverters (simplifyConstExpr, simplifyConstIntExpr)
import Control.Monad (when, (>=>))
import Control.Monad.State (MonadState (..), MonadTrans (..), StateT (runStateT), modify)
import Data.Functor (($>))
import Data.List.Extra (anySame, find)
import qualified Data.Map as Map
import Data.Maybe (isNothing)
import Data.Text (unpack)
import Errors (todo)
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

    predicate' (AAst.FunctionDef name (AAst.Function args _)) = name == "main" && null args
    predicate' _ = False

analyzeFuncs :: [Ast.FunctionDef] -> Result [AAst.FunctionDef]
analyzeFuncs functions = do
  env <- initEnv functions
  put env
  mapM checkFunc' functions
  where
    checkFunc' (Ast.FunctionDef name func) = AAst.FunctionDef name <$> analyzeFunc func

    initEnv funcs = do
      funcs' <- mapM convertToPair funcs
      return $ Env (Scope $ Map.fromList funcs') []

    convertToPair (Ast.FunctionDef name (Ast.Function (Ast.FunctionSignature params ret) _)) = do
      params' <- mapM analyzeType (snd <$> params)
      ret' <- mapM analyzeType ret
      return (name, AType.TFunction $ AType.FunctionType params' ret')

analyzeFunc :: Ast.Function -> Result AAst.Function
analyzeFunc (Ast.Function (Ast.FunctionSignature params ret) stmts) = do
  params' <- mapM (\(name, t) -> (name,) <$> analyzeType t) params
  ret' <- mapM analyzeType ret
  modify $ pushScope (Scope (Map.fromList params'))
  stmts' <- analyzeBlock stmts ret'
  modify popScope
  return $ AAst.Function (fst <$> params') stmts'
  where
    pushScope initScope env@(Env _ fScs) = env {funcsScopes = FuncScope [initScope] : fScs}

    popScope env@(Env _ (_ : fScs)) = env {funcsScopes = fScs}
    popScope env = env

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
  Ast.StmtBreak -> todo $ unpack "`break` statement checker" -- TODO
  Ast.StmtContinue -> todo $ unpack "`continue` statement checker" -- TODO
  Ast.StmtFor _ -> todo $ unpack "`for` statement checker" -- TODO
  Ast.StmtVarDecl varDecl -> case varDecl of
    Ast.VarDecl name (Just t) expr -> do
      (t', expr') <- analyzeExpr expr
      t'' <- analyzeType t
      checkTypesEq (Just t'') t'
      addNewVar $ return (name, t'')
      return $ AAst.StmtVarDecl $ AAst.VarDecl name expr'
    Ast.VarDecl name Nothing expr -> do
      (t, expr') <- analyzeExpr expr
      case t of
        Just t' -> do
          addNewVar $ return (name, t')
          return $ AAst.StmtVarDecl $ AAst.VarDecl name expr'
        _ -> throw MismatchedTypes
    Ast.DefaultedVarDecl name t -> do
      t' <- analyzeType t
      addNewVar $ return (name, t')
      return $ AAst.StmtVarDecl $ AAst.VarDecl name (getTypeDefault t')
  Ast.StmtIfElse (Ast.IfElse condition stmts _) -> do
    -- TODO : Else
    (condT, condExpr) <- analyzeExpr condition
    checkTypesEq condT (Just AType.TBool)
    stmts' <- analyzeBlock stmts funcReturn
    return $ AAst.StmtIfElse (AAst.IfElse condExpr stmts' Nothing)
  Ast.StmtBlock stmts -> AAst.StmtBlock <$> analyzeBlock stmts funcReturn
  Ast.StmtSimple simpleStmt ->
    AAst.StmtSimple <$> case simpleStmt of
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
          Just t' -> addOrUpdateVar (return (name, t')) $> AAst.StmtShortVarDecl name expr'
          _ -> throw MismatchedTypes
      Ast.StmtExpression expr -> do
        (_, expr') <- analyzeExpr expr
        return $ AAst.StmtExpression expr'

analyzeUpdEl :: Ast.UpdatableElement -> Result (AType.Type, AAst.UpdatableElement)
analyzeUpdEl updEl = case updEl of
  Ast.UpdVar name -> (,AAst.UpdVar name) <$> getVarType (return name)
  Ast.UpdArrEl name indexExprs -> do
    varT <- getVarType $ return name
    indexExprs' <- mapM (analyzeExpr >=> \(t, expr) -> checkTypesEq (Just AType.TInt) t $> expr) indexExprs
    let calculatedType =
          let getArrayElementType t dimCnt = case t of
                AType.TArray t' _ | dimCnt > 0 -> getArrayElementType t' (dimCnt - 1)
                _ | dimCnt == 0 -> Just t
                _ -> Nothing
           in getArrayElementType varT (length indexExprs)
    maybe (throw MismatchedTypes) (return . (,AAst.UpdArrEl name indexExprs')) calculatedType

analyzeBlock :: [Ast.Statement] -> Maybe AType.Type -> Result [AAst.Statement]
analyzeBlock stmts ret = do
  modify pushScope
  stmts'' <- foldl analyzeStmt' (return []) stmts
  modify popScope
  return stmts''
  where
    analyzeStmt' res stmt = do
      stmts' <- res
      stmt' <- analyzeStmt stmt ret
      return (stmt' : stmts')

    pushScope env@(Env _ (fSc@(FuncScope scs) : fScs)) = env {funcsScopes = fSc {scopes = Scope Map.empty : scs} : fScs}
    pushScope env = env {funcsScopes = [FuncScope [Scope Map.empty]]}

    popScope env@(Env _ (fSc@(FuncScope (_ : scs)) : fScs)) = env {funcsScopes = fSc {scopes = scs} : fScs}
    popScope env = env

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
              Ast.ValArray _ -> todo $ unpack "array value analyzer" -- TODO
              Ast.ValFunction (Ast.AnonymousFunction _) -> todo $ unpack "anonymous function analyzer" -- TODO
              Ast.ValFunction Ast.Nil -> return' AType.TNil $ AAst.ValFunction AAst.Nil
      Ast.ExprIdentifier name -> do
        t <- getVarType $ return name
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
      Ast.ExprPrintlnFuncCall arg -> do
        (argT, argE) <- analyzeExpr arg
        let return' = return (Nothing, AAst.ExprFuncCall (stdLibFuncExpr $ StdLib.name StdLib.printlnFunction) [argE])
        case argT of
          Just AType.TInt -> return'
          Just AType.TBool -> return'
          Just AType.TString -> return'
          Just (AType.TArray _ _) -> return'
          _ -> throw MismatchedTypes
      Ast.ExprPanicFuncCall arg -> do
        (argT, argE) <- analyzeExpr arg
        checkTypesEq (Just AType.TString) argT
        return (Nothing, AAst.ExprFuncCall (stdLibFuncExpr $ StdLib.name StdLib.panicFunction) [argE])

checkTypesEq :: Maybe AType.Type -> Maybe AType.Type -> Result ()
checkTypesEq lhs rhs = if lhs == rhs then return () else throw MismatchedTypes

---------------------------------------------------------Types----------------------------------------------------------

analyzeType :: Ast.Type -> Result AType.Type
analyzeType t = case t of
  Ast.TInt -> return AType.TInt
  Ast.TBool -> return AType.TBool
  Ast.TString -> return AType.TString
  Ast.TArray (Ast.ArrayType elementT len) ->
    AType.TArray <$> analyzeType elementT <*> lift (simplifyConstIntExpr len)
  Ast.TFunction (Ast.FunctionType paramsTs retT) ->
    AType.TFunction <$> (AType.FunctionType <$> mapM analyzeType paramsTs <*> mapM analyzeType retT)

stdLibFuncExpr :: AAst.Identifier -> AAst.Expression
stdLibFuncExpr name = AAst.ExprValue $ AAst.ValFunction $ AAst.AnonymousFunction $ AAst.StdLibFunction name
