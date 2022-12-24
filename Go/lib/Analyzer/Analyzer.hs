{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

-- TODO : Docs
module Analyzer.Analyzer where

import Analyzer.AnalysisResult
import qualified Analyzer.AnalyzedAst as AAst
import qualified Analyzer.AnalyzedType as AType
import Analyzer.AnalyzerRuntime (addNewVar, addOrUpdateVar, getCurrScopeType, getTypeDefault, getVarType, popScope, pushScope)
import Analyzer.ConstExpressionConverters (simplifyConstExpr, simplifyConstIntExpr)
import Control.Monad (mapAndUnzipM, when, (>=>))
import Control.Monad.State (MonadState (..), MonadTrans (..), StateT (runStateT), modify)
import Data.Functor (($>))
import Data.List.Extra (anySame, find)
import Data.Maybe (isNothing)
import qualified Parser.Ast as Ast
import qualified StdLib

--------------------------------------------------------Analyzer--------------------------------------------------------

-- * Analyzer

-- | Analyzer entry point
analyze :: Ast.Program -> Either Err (AAst.Program, Env)
analyze ast = runStateT (analyzeProgram ast) emptyEnv

-------------------------------------------------Program and functions--------------------------------------------------

-- TODO : Docs
analyzeProgram :: Ast.Program -> Result AAst.Program
analyzeProgram (Ast.Program vars functions) = do
  checkForUniqueness $ (Ast.funcName <$> functions) ++ (Ast.varName <$> vars)
  checkForMain functions
  funcs <- analyzeFuncs vars functions
  return $ AAst.Program [] funcs
  where
    checkForUniqueness ns = when (anySame ns) (throw IdentifierRedeclaration)

    checkForMain funcs = when (isNothing $ find predicate funcs) (throw NoMain)
    predicate (Ast.FunctionDef name (Ast.Function (Ast.FunctionSignature params ret) _)) =
      name == "main" && null params && null ret

-- TODO : Docs
analyzeFuncs :: [Ast.VarDecl] -> [Ast.FunctionDef] -> Result [AAst.FunctionDef]
analyzeFuncs varDeclarations functionDefinitions = do
  initEnv >>= put
  mapM checkFunc' functionDefinitions
  where
    checkFunc' (Ast.FunctionDef name func) = AAst.FunctionDef name <$> (snd <$> analyzeFunc func)

    initEnv = do
      mapM_ analyzeStmtVarDecl varDeclarations
      funcs <- mapM convertToPair functionDefinitions
      Env scs <- get
      return $ Env {scopes = scope OrdinaryScope funcs : scs}

    convertToPair (Ast.FunctionDef name (Ast.Function (Ast.FunctionSignature params ret) _)) = do
      params' <- mapM analyzeType (snd <$> params)
      ret' <- mapM analyzeType ret
      return (name, AType.TFunction $ AType.FunctionType params' ret')

-- TODO : Docs
analyzeFunc :: Ast.Function -> Result (AType.FunctionType, AAst.Function)
analyzeFunc (Ast.Function (Ast.FunctionSignature params ret) stmts) = do
  let paramsNs = fst <$> params
  paramsTs <- mapM (analyzeType . snd) params
  let params' = paramsNs `zip` paramsTs
  ret' <- mapM analyzeType ret
  stmts' <- analyzeBlock (scope OrdinaryScope params') stmts ret'
  return (AType.FunctionType paramsTs ret', AAst.Function paramsNs stmts')

-------------------------------------------------------Statements-------------------------------------------------------

-- | Analyze statement.
analyzeStmt :: Ast.Statement -> Maybe AType.Type -> Result AAst.Statement
analyzeStmt statement funcReturn = case statement of
  Ast.StmtReturn expr -> analyzeStmtReturn expr funcReturn
  Ast.StmtForGoTo goto -> analyzeStmtForGoTo goto
  Ast.StmtFor for -> analyzeStmtFor for funcReturn
  Ast.StmtVarDecl varDecl -> AAst.StmtVarDecl <$> analyzeStmtVarDecl varDecl
  Ast.StmtIfElse ifElse -> AAst.StmtIfElse <$> analyzeIfElse ifElse funcReturn
  Ast.StmtBlock stmts -> AAst.StmtBlock <$> analyzeBlock' stmts funcReturn
  Ast.StmtSimple simpleStmt -> AAst.StmtSimple <$> analyzeSimpleStmt simpleStmt

-- TODO : Docs
analyzeStmtReturn :: Maybe Ast.Expression -> Maybe AType.Type -> Result AAst.Statement
analyzeStmtReturn expression funcReturn = case expression of
  (Just expr) -> do
    (t, expr') <- analyzeExpr expr
    checkEq t funcReturn
    return $ AAst.StmtReturn (Just expr')
  Nothing -> AAst.StmtReturn Nothing <$ checkEq funcReturn Nothing

-- TODO : Docs
analyzeStmtForGoTo :: Ast.ForGoTo -> Result AAst.Statement
analyzeStmtForGoTo goto = do
  env <- get
  if getCurrScopeType env == Right ForScope
    then return $ AAst.StmtForGoTo goto
    else throw BreakOrContinueOutsideOfForScope

-- TODO : Docs
analyzeStmtFor :: Ast.For -> Maybe AType.Type -> Result AAst.Statement
analyzeStmtFor (Ast.For kind stmts) funcReturn =
  AAst.StmtFor <$> case kind of
    Ast.ForKindFor preStmt cond postStmt -> do
      modify $ pushScope (emptyScope OrdinaryScope)
      preStmt' <- mapM analyzeSimpleStmt preStmt
      cond' <- mapM analyzeCondition cond
      postStmt' <- mapM analyzeSimpleStmt postStmt
      stmts' <- analyzeStmts
      modify popScope
      return $ AAst.For (AAst.ForKindFor preStmt' cond' postStmt') stmts'
    Ast.ForKindWhile cond ->
      AAst.For <$> (AAst.ForKindWhile <$> analyzeCondition cond) <*> analyzeStmts
    Ast.ForKindLoop -> AAst.For AAst.ForKindLoop <$> analyzeStmts
  where
    analyzeCondition cond = do
      (t, e) <- analyzeExpr' cond
      checkEq t AType.TInt
      return e
    analyzeStmts = analyzeBlock (emptyScope ForScope) stmts funcReturn

-- TODO : Docs
analyzeStmtVarDecl :: Ast.VarDecl -> Result AAst.VarDecl
analyzeStmtVarDecl (Ast.VarDecl name val) = do
  (t, e) <- case val of
    Ast.VarValue (Just t) expr -> do
      (t', expr') <- analyzeExpr' expr
      t'' <- analyzeType t
      checkEq t' t''
      return (t', expr')
    Ast.VarValue Nothing expr -> do
      (t, expr') <- analyzeExpr' expr
      return (t, expr')
    Ast.DefaultedVarValue t -> do
      t' <- analyzeType t
      return (t', getTypeDefault t')
  addNewVar name t
  return $ AAst.VarDecl name e

-- TODO : Docs
analyzeIfElse :: Ast.IfElse -> Maybe AType.Type -> Result AAst.IfElse
analyzeIfElse (Ast.IfElse condition stmts elseStmt) funcReturn = do
  (condT, condExpr) <- analyzeExpr' condition
  checkEq condT AType.TBool
  stmts' <- analyzeBlock' stmts funcReturn
  elseStmt' <- case elseStmt of
    Just (Left ifElse) -> Just . Left <$> analyzeIfElse ifElse funcReturn
    Just (Right elseStmt') ->
      Just . Right <$> analyzeBlock' elseStmt' funcReturn
    Nothing -> return Nothing
  return $ AAst.IfElse condExpr stmts' elseStmt'

-- TODO : Docs
analyzeBlock :: Scope -> [Ast.Statement] -> Maybe AType.Type -> Result [AAst.Statement]
analyzeBlock initScope stmts ret = do
  modify $ pushScope initScope
  stmts'' <- mapM (`analyzeStmt` ret) stmts
  modify popScope
  return stmts''

-- TODO : Docs
analyzeBlock' :: [Ast.Statement] -> Maybe AType.Type -> Result [AAst.Statement]
analyzeBlock' = analyzeBlock (emptyScope OrdinaryScope)

-- TODO : Docs
analyzeSimpleStmt :: Ast.SimpleStmt -> Result AAst.SimpleStmt
analyzeSimpleStmt simpleStmt = case simpleStmt of
  Ast.StmtAssignment updEl expr -> do
    (updElT, updEl') <- analyzeUpdEl updEl
    (t, expr') <- analyzeExpr' expr
    checkEq updElT t
    return $ AAst.StmtAssignment updEl' expr'
  Ast.StmtInc updEl -> do
    (updElT, updEl') <- analyzeUpdEl updEl
    checkEq updElT AType.TInt
    return $ AAst.StmtInc updEl'
  Ast.StmtDec updEl -> do
    (updElT, updEl') <- analyzeUpdEl updEl
    checkEq updElT AType.TInt
    return $ AAst.StmtDec updEl'
  Ast.StmtShortVarDecl name expr -> do
    (t, expr') <- analyzeExpr' expr
    addOrUpdateVar name t
    return $ AAst.StmtShortVarDecl name expr'
  Ast.StmtExpression expr -> AAst.StmtExpression . snd <$> analyzeExpr expr

-- TODO : Docs
analyzeUpdEl :: Ast.UpdatableElement -> Result (AType.Type, AAst.UpdatableElement)
analyzeUpdEl updEl = case updEl of
  Ast.UpdVar name -> (,AAst.UpdVar name) <$> getVarType name
  Ast.UpdArrEl name indexExprs -> do
    varT <- getVarType name
    indexExprs' <- mapM (analyzeExpr' >=> \(t, expr) -> checkEq AType.TInt t $> expr) indexExprs
    let calculatedType =
          let getArrayElementType t dimCnt = case t of
                AType.TArray t' _ | dimCnt > 0 -> getArrayElementType t' (dimCnt - 1)
                _ | dimCnt == 0 -> Just t
                _ -> Nothing
           in getArrayElementType varT (length indexExprs)
    maybe (throw MismatchedTypes) (return . (,AAst.UpdArrEl name indexExprs')) calculatedType

------------------------------------------------------Expressions-------------------------------------------------------

-- TODO : Docs
analyzeExpr :: Ast.Expression -> Result (Maybe AType.Type, AAst.Expression)
analyzeExpr expression = case simplifyConstExpr expression of
  Right res -> return res
  Left NotInIntBounds -> throw NotInIntBounds
  Left _ -> case expression of
    Ast.ExprValue val -> analyzeExprValue val
    Ast.ExprIdentifier name -> analyzeExprIdentifier name
    Ast.ExprUnaryOp unOp expr -> analyzeExprUnaryOp unOp expr
    Ast.ExprBinaryOp binOp lhs rhs -> analyzeExprBinaryOp binOp lhs rhs
    Ast.ExprFuncCall func args -> analyzeExprFuncCall func args
    Ast.ExprArrayAccessByIndex arr index -> analyzeExprArrayAccessByIndex arr index
    Ast.ExprLenFuncCall arg -> analyzeExprLenFuncCall arg
    Ast.ExprPrintlnFuncCall maybeArg -> analyzeExprPrintlnFuncCall maybeArg
    Ast.ExprPanicFuncCall arg -> analyzeExprPanicFuncCall arg

-- TODO : Docs
analyzeExprValue :: Ast.Value -> Result (Maybe AType.Type, AAst.Expression)
analyzeExprValue val = case val of
  Ast.ValInt v -> returnInt v
  Ast.ValBool v -> returnBool v
  Ast.ValString v -> returnString v
  Ast.ValArray (Ast.ArrayValue t els) -> do
    (elT, len) <- analyzeArrayType t
    (elsTs, elsVs) <- mapAndUnzipM analyzeExpr' els
    mapM_ (checkEq elT) elsTs
    checkCondition $ length elsVs <= len
    returnArray elT len elsVs
  Ast.ValFunction (Ast.AnonymousFunction function) -> analyzeFunc function >>= uncurry returnFunction
  Ast.ValFunction Ast.Nil -> returnNil
  where
    returnInt v = return' AType.TInt $ AAst.ValInt $ fromIntegral v
    returnBool v = return' AType.TBool $ AAst.ValBool v
    returnString v = return' AType.TString $ AAst.ValString v
    returnArray elT len elsVs =
      return' (AType.TArray elT len) $ AAst.ValArray $ replicate (len - length elsVs) (getTypeDefault elT) ++ elsVs
    returnFunction t f = return' (AType.TFunction t) $ AAst.ValFunction (AAst.AnonymousFunction f)
    returnNil = return' AType.TNil $ AAst.ValFunction AAst.Nil

    return' t expr = return (Just t, AAst.ExprValue expr)

-- TODO : Docs
analyzeExprIdentifier :: AAst.Identifier -> Result (Maybe AType.Type, AAst.Expression)
analyzeExprIdentifier name = (,AAst.ExprIdentifier name) . Just <$> getVarType name

-- TODO : Docs
analyzeExprUnaryOp :: Ast.UnaryOp -> Ast.Expression -> Result (Maybe AType.Type, AAst.Expression)
analyzeExprUnaryOp unOp expr = do
  (t, expr') <- analyzeExpr' expr
  let return' = return (Just t, AAst.ExprUnaryOp unOp expr')
  case (unOp, t) of
    (Ast.UnaryPlusOp, AType.TInt) -> return'
    (Ast.UnaryMinusOp, AType.TInt) -> return'
    (Ast.NotOp, AType.TBool) -> return'
    _ -> throw MismatchedTypes

-- TODO : Docs
analyzeExprBinaryOp :: Ast.BinaryOp -> Ast.Expression -> Ast.Expression -> Result (Maybe AType.Type, AAst.Expression)
analyzeExprBinaryOp binOp lhs rhs = do
  (lhsT, lhs') <- analyzeExpr' lhs
  (rhsT, rhs') <- analyzeExpr' rhs
  checkEq lhsT rhsT
  let return' t = return (Just t, AAst.ExprBinaryOp binOp lhs' rhs')
  let returnInt = return' AType.TInt
  let returnBool = return' AType.TBool
  let returnString = return' AType.TString
  case (binOp, lhsT) of
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
    _ -> throw MismatchedTypes

-- TODO : Docs
analyzeExprFuncCall :: Ast.Expression -> [Ast.Expression] -> Result (Maybe AType.Type, AAst.Expression)
analyzeExprFuncCall func args = do
  (funcT, func') <- analyzeExpr' func
  case funcT of
    (AType.TFunction (AType.FunctionType paramsTs retT)) -> do
      args' <- mapM analyzeExpr args
      args'' <- mapM (\(eT, (aT, expr)) -> checkEq (Just eT) aT $> expr) (paramsTs `zip` args')
      return (retT, AAst.ExprFuncCall func' args'')
    _ -> throw MismatchedTypes

-- TODO : Docs
analyzeExprArrayAccessByIndex :: Ast.Expression -> Ast.Expression -> Result (Maybe AType.Type, AAst.Expression)
analyzeExprArrayAccessByIndex arr index = do
  (aT, a) <- analyzeExpr' arr
  (iT, i) <- analyzeExpr' index
  case (aT, iT) of
    (AType.TArray elT _, AType.TInt) -> return (Just elT, AAst.ExprArrayAccessByIndex a i)
    _ -> throw MismatchedTypes

-- TODO : Docs
analyzeExprLenFuncCall :: Ast.Expression -> Result (Maybe AType.Type, AAst.Expression)
analyzeExprLenFuncCall arg = do
  (argT, argE) <- analyzeExpr' arg
  let return' = return (Just AType.TInt, AAst.ExprFuncCall (stdLibFuncExpr $ StdLib.name StdLib.lenFunction) [argE])
  case argT of
    AType.TString -> return'
    AType.TArray _ _ -> return'
    _ -> throw MismatchedTypes

-- TODO : Docs
analyzeExprPrintlnFuncCall :: Maybe Ast.Expression -> Result (Maybe AType.Type, AAst.Expression)
analyzeExprPrintlnFuncCall maybeArg = case maybeArg of
  Just arg -> do
    (argT, argE) <- analyzeExpr' arg
    let return' = return (Nothing, AAst.ExprFuncCall (stdLibFuncExpr $ StdLib.name StdLib.printlnFunction) [argE])
    case argT of
      AType.TInt -> return'
      AType.TBool -> return'
      AType.TString -> return'
      _ -> throw MismatchedTypes
  Nothing -> return (Nothing, AAst.ExprFuncCall (stdLibFuncExpr $ StdLib.name StdLib.printlnFunction) [])

-- TODO : Docs
analyzeExprPanicFuncCall :: Ast.Expression -> Result (Maybe AType.Type, AAst.Expression)
analyzeExprPanicFuncCall arg = do
  (argT, argE) <- analyzeExpr arg
  checkEq (Just AType.TString) argT
  return (Nothing, AAst.ExprFuncCall (stdLibFuncExpr $ StdLib.name StdLib.panicFunction) [argE])

---------------------------------------------------------Types----------------------------------------------------------

-- TODO : Docs
analyzeType :: Ast.Type -> Result AType.Type
analyzeType t = case t of
  Ast.TInt -> return AType.TInt
  Ast.TBool -> return AType.TBool
  Ast.TString -> return AType.TString
  Ast.TArray arrType -> uncurry AType.TArray <$> analyzeArrayType arrType
  Ast.TFunction funcType -> AType.TFunction <$> analyzeFunctionType funcType

-- TODO : Docs
analyzeArrayType :: Ast.ArrayType -> Result (AType.Type, Int)
analyzeArrayType (Ast.ArrayType elementT len) =
  (,) <$> analyzeType elementT <*> lift (simplifyConstIntExpr len)

-- TODO : Docs
analyzeFunctionType :: Ast.FunctionType -> Result AType.FunctionType
analyzeFunctionType (Ast.FunctionType paramsTs retT) =
  AType.FunctionType <$> mapM analyzeType paramsTs <*> mapM analyzeType retT

---------------------------------------------------------Utils----------------------------------------------------------

-- TODO : Docs
analyzeExpr' :: Ast.Expression -> Result (AType.Type, AAst.Expression)
analyzeExpr' = analyzeExpr >=> unwrapExprRes

-- TODO : Docs
stdLibFuncExpr :: AAst.Identifier -> AAst.Expression
stdLibFuncExpr name = AAst.ExprValue $ AAst.ValFunction $ AAst.AnonymousFunction $ AAst.StdLibFunction name

-- TODO : Docs
checkEq :: Eq a => a -> a -> Result ()
checkEq lhs rhs = checkCondition $ lhs == rhs

-- TODO : Docs
checkCondition :: Bool -> Result ()
checkCondition cond = if cond then return () else throw MismatchedTypes

-- TODO : Docs
unwrapJust :: Maybe a -> Result a
unwrapJust = maybe (throw MismatchedTypes) return

-- TODO : Docs
unwrapExprRes :: (Maybe AType.Type, AAst.Expression) -> Result (AType.Type, AAst.Expression)
unwrapExprRes (t, expr) = (,expr) <$> unwrapJust t
