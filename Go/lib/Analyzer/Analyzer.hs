{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Analyzer.Analyzer where

import qualified Analyzer.AnalyzedAst as AAst
import qualified Analyzer.AnalyzedType as AType
import Analyzer.ConstExpressionConverters (simplifyConstExpr, simplifyConstIntExpr)
import qualified Analyzer.ConstExpressionConverters as CEC
import Analyzer.Result
import Analyzer.Runtime (addNewVar, addOrUpdateVar, getCurrScopeType, getTypeDefault, getVarType, popScope, pushScope)
import Control.Lens ((%~))
import Control.Monad (mapAndUnzipM, void, when, (>=>))
import Control.Monad.Except (MonadError (throwError), liftEither, runExceptT)
import Control.Monad.State (gets, modify, runState)
import Data.Either.Extra (mapLeft)
import Data.Functor (($>))
import Data.List.Extra (find, nubOrd, (\\))
import Data.Maybe (isNothing, listToMaybe)
import Data.Text (Text)
import qualified Parser.Ast as Ast
import qualified StdLib

--------------------------------------------------------Analyzer--------------------------------------------------------

-- * Analyzer

-- | Analyzer entry point
analyze :: Ast.Program -> (ResultValue AAst.Program, Env)
analyze ast = runState (runExceptT (analyzeProgram ast)) emptyEnv

-------------------------------------------------Program and functions--------------------------------------------------

analyzeProgram :: Ast.Program -> Result AAst.Program
analyzeProgram (Ast.Program tlVarDecls tlFuncDefs) = do
  checkForUniqueness $ (Ast.funcName <$> tlFuncDefs) ++ (Ast.varName <$> tlVarDecls)
  checkForMain tlFuncDefs

  (vs, vsKVPs) <- analyzeTLVarDecls tlVarDecls
  funcs <- analyzeTLFuncDefs vsKVPs tlFuncDefs

  return $ AAst.Program vs funcs
  where
    checkForUniqueness :: [Text] -> Result ()
    checkForUniqueness ns = maybe (return ()) (throwError . IdentifierRedeclaration) (findDuplicate ns)

    findDuplicate :: Ord a => [a] -> Maybe a
    findDuplicate list = listToMaybe $ list \\ nubOrd list

    checkForMain :: [Ast.FunctionDef] -> Result ()
    checkForMain funcs = when (isNothing $ find predicate funcs) (void $ throwError NoMain)
    predicate (Ast.FunctionDef name (Ast.Function (Ast.FunctionSignature params ret) _)) =
      name == "main" && null params && null ret

analyzeTLVarDecls :: [Ast.VarDecl] -> Result ([AAst.VarDecl], [(AAst.Identifier, AType.Type)])
analyzeTLVarDecls = mapAndUnzipM convertVarDecl
  where
    convertVarDecl (Ast.VarDecl name val) = do
      (t, expr) <- analyzeVarValue val
      return (AAst.VarDecl name expr, (name, t))

analyzeTLFuncDefs :: [(AAst.Identifier, AType.Type)] -> [Ast.FunctionDef] -> Result [AAst.FunctionDef]
analyzeTLFuncDefs varsKeyValuePairs functionDefinitions = do
  funcsKeyValuePairs <- mapM convertFuncDef functionDefinitions
  let initScope = scope OrdinaryScope (varsKeyValuePairs <> funcsKeyValuePairs)
  modify $ scopes %~ (initScope :)
  mapM analyzeFuncDef functionDefinitions
  where
    analyzeFuncDef (Ast.FunctionDef name func) = AAst.FunctionDef name <$> (snd <$> analyzeFunc func)

    convertFuncDef (Ast.FunctionDef name (Ast.Function (Ast.FunctionSignature params ret) _)) = do
      params' <- mapM analyzeType (snd <$> params)
      ret' <- mapM analyzeType ret
      return (name, AType.TFunction $ AType.FunctionType params' ret')

analyzeFunc :: Ast.Function -> Result (AType.FunctionType, AAst.Function)
analyzeFunc (Ast.Function (Ast.FunctionSignature params ret) stmts) = do
  let paramsNs = fst <$> params
  paramsTs <- mapM (analyzeType . snd) params
  ret' <- mapM analyzeType ret
  stmts' <- analyzeBlock (scope OrdinaryScope (paramsNs `zip` paramsTs)) stmts ret'
  return
    ( AType.FunctionType paramsTs ret',
      AAst.OrdinaryFunction paramsNs stmts' (maybe AAst.VoidFunc (const AAst.NonVoidFunc) ret')
    )

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

analyzeStmtReturn :: Maybe Ast.Expression -> Maybe AType.Type -> Result AAst.Statement
analyzeStmtReturn expression funcReturn = case expression of
  (Just expr) -> do
    (t, expr') <- analyzeExpr expr
    checkEq t funcReturn
    return $ AAst.StmtReturn (Just expr')
  Nothing -> AAst.StmtReturn Nothing <$ checkEq funcReturn Nothing

analyzeStmtForGoTo :: Ast.ForGoTo -> Result AAst.Statement
analyzeStmtForGoTo goto = do
  scopeT <- gets getCurrScopeType
  case scopeT of
    ForScope -> return $ AAst.StmtForGoTo goto
    OrdinaryScope -> throwError BreakOrContinueOutsideOfForScope

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
    Ast.ForKindWhile cond -> AAst.For . AAst.ForKindWhile <$> analyzeCondition cond <*> analyzeStmts
    Ast.ForKindLoop -> AAst.For AAst.ForKindLoop <$> analyzeStmts
  where
    analyzeCondition cond = do
      (t, e) <- analyzeExpr' cond
      checkEq t AType.TBool
      return e
    analyzeStmts = analyzeBlock (emptyScope ForScope) stmts funcReturn

analyzeStmtVarDecl :: Ast.VarDecl -> Result AAst.VarDecl
analyzeStmtVarDecl (Ast.VarDecl name val) = do
  (t, e) <- analyzeVarValue val
  addNewVar name t
  return $ AAst.VarDecl name e

analyzeVarValue :: Ast.VarValue -> Result (AType.Type, AAst.Expression)
analyzeVarValue = \case
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

analyzeIfElse :: Ast.IfElse -> Maybe AType.Type -> Result AAst.IfElse
analyzeIfElse (Ast.IfElse condition stmts elseStmt) funcReturn = do
  (condT, condExpr) <- analyzeExpr' condition
  checkEq condT AType.TBool
  stmts' <- analyzeBlock' stmts funcReturn
  elseStmt' <- case elseStmt of
    Ast.NoElse -> return AAst.NoElse
    Ast.Else elseStmt' -> AAst.Else <$> analyzeBlock' elseStmt' funcReturn
    Ast.Elif ifElse -> AAst.Elif <$> analyzeIfElse ifElse funcReturn
  return $ AAst.IfElse condExpr stmts' elseStmt'

analyzeBlock :: Scope -> Ast.Block -> Maybe AType.Type -> Result AAst.Block
analyzeBlock initScope stmts ret = do
  modify $ pushScope initScope
  stmts' <- mapM (`analyzeStmt` ret) stmts
  modify popScope
  return stmts'

analyzeBlock' :: Ast.Block -> Maybe AType.Type -> Result AAst.Block
analyzeBlock' = analyzeBlock (emptyScope OrdinaryScope)

analyzeSimpleStmt :: Ast.SimpleStmt -> Result AAst.SimpleStmt
analyzeSimpleStmt = \case
  Ast.StmtAssignment lval expr -> do
    (lvalT, lval') <- analyzeLvalue lval
    (t, expr') <- analyzeExpr' expr
    checkEq lvalT t
    return $ AAst.StmtAssignment lval' expr'
  Ast.StmtIncDec lval incDec -> do
    (lvalT, lval') <- analyzeLvalue lval
    checkEq lvalT AType.TInt
    return $ AAst.StmtIncDec lval' incDec
  Ast.StmtShortVarDecl name expr -> do
    (t, expr') <- analyzeExpr' expr
    addOrUpdateVar name t
    return $ AAst.StmtShortVarDecl name expr'
  Ast.StmtExpression expr -> AAst.StmtExpression . snd <$> analyzeExpr expr

analyzeLvalue :: Ast.Lvalue -> Result (AType.Type, AAst.Lvalue)
analyzeLvalue = \case
  Ast.LvalVar name -> (,AAst.LvalVar name) <$> getVarType name
  Ast.LvalArrEl name indexExprs -> do
    varT <- getVarType name
    indexExprs' <- mapM analyzeIntExpr indexExprs
    let calculatedType =
          let getArrayElementType t dimCnt = case t of
                AType.TArray t' _ | dimCnt > 0 -> getArrayElementType t' (dimCnt - 1)
                _ | dimCnt == 0 -> Just t
                _ -> Nothing
           in getArrayElementType varT (length indexExprs)
    maybe (throwError MismatchedTypes) (return . (,AAst.LvalArrEl name indexExprs')) calculatedType

------------------------------------------------------Expressions-------------------------------------------------------

analyzeExpr :: Ast.Expression -> Result (Maybe AType.Type, AAst.Expression)
analyzeExpr expression = case simplifyConstExpr expression of
  Right res -> return res
  Left CEC.MismatchedTypes -> throwError MismatchedTypes
  Left CEC.DivisionByZero -> throwError DivisionByZero
  Left CEC.NotInIntBounds -> throwError NotInIntBounds
  Left CEC.NotConstExpr -> case expression of
    Ast.ExprValue val -> analyzeExprValue val
    Ast.ExprIdentifier name -> analyzeExprIdentifier name
    Ast.ExprUnaryOp unOp expr -> analyzeExprUnaryOp unOp expr
    Ast.ExprBinaryOp binOp lhs rhs -> analyzeExprBinaryOp binOp lhs rhs
    Ast.ExprFuncCall func args -> analyzeExprFuncCall func args
    Ast.ExprArrayAccessByIndex arr index -> analyzeExprArrayAccessByIndex arr index
    Ast.ExprLenFuncCall arg -> analyzeExprLenFuncCall arg
    Ast.ExprPrintFuncCall args -> analyzeExprPrintFuncCall args
    Ast.ExprPrintlnFuncCall args -> analyzeExprPrintlnFuncCall args
    Ast.ExprPanicFuncCall arg -> analyzeExprPanicFuncCall arg

analyzeExprValue :: Ast.Value -> Result (Maybe AType.Type, AAst.Expression)
analyzeExprValue = \case
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
      return' (AType.TArray elT len) $ AAst.ValArray $ elsVs <> replicate (len - length elsVs) (getTypeDefault elT)
    returnFunction t f = return' (AType.TFunction t) $ AAst.ValFunction (AAst.Function f)
    returnNil = return' AType.TNil $ AAst.ValFunction AAst.Nil

    return' t expr = return (Just t, AAst.ExprValue expr)

analyzeExprIdentifier :: AAst.Identifier -> Result (Maybe AType.Type, AAst.Expression)
analyzeExprIdentifier name = (,AAst.ExprIdentifier name) . Just <$> getVarType name

analyzeExprUnaryOp :: Ast.UnaryOp -> Ast.Expression -> Result (Maybe AType.Type, AAst.Expression)
analyzeExprUnaryOp unOp expr = do
  (t, expr') <- analyzeExpr' expr
  let return' = return (Just t, AAst.ExprUnaryOp unOp expr')
  case (unOp, t) of
    (Ast.UnaryPlusOp, AType.TInt) -> return'
    (Ast.UnaryMinusOp, AType.TInt) -> return'
    (Ast.NotOp, AType.TBool) -> return'
    _ -> throwError MismatchedTypes

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
    _ -> throwError MismatchedTypes

analyzeExprFuncCall :: Ast.Expression -> [Ast.Expression] -> Result (Maybe AType.Type, AAst.Expression)
analyzeExprFuncCall func args = do
  (funcT, func') <- analyzeExpr' func
  case funcT of
    (AType.TFunction (AType.FunctionType paramsTs retT)) -> do
      (argsTs, args') <- mapAndUnzipM analyzeExpr' args
      mapM_ (uncurry checkEq) (paramsTs `zip` argsTs)
      return (retT, AAst.ExprFuncCall func' args')
    _ -> throwError MismatchedTypes

analyzeExprArrayAccessByIndex :: Ast.Expression -> Ast.Expression -> Result (Maybe AType.Type, AAst.Expression)
analyzeExprArrayAccessByIndex arr index = do
  (aT, a) <- analyzeExpr' arr
  i <- analyzeIntExpr index
  case aT of
    AType.TArray elT _ -> return (Just elT, AAst.ExprArrayAccessByIndex a i)
    _ -> throwError MismatchedTypes

analyzeExprLenFuncCall :: Ast.Expression -> Result (Maybe AType.Type, AAst.Expression)
analyzeExprLenFuncCall arg = do
  (argT, argE) <- analyzeExpr' arg
  let return' = return (Just AType.TInt, AAst.ExprFuncCall (stdLibFuncExpr $ StdLib.name StdLib.lenFunction) [argE])
  case argT of
    AType.TString -> return'
    AType.TArray _ _ -> return'
    _ -> throwError MismatchedTypes

analyzeExprPrintFuncCall :: [Ast.Expression] -> Result (Maybe AType.Type, AAst.Expression)
analyzeExprPrintFuncCall args =
  (\args' -> (Nothing, AAst.ExprFuncCall (stdLibFuncExpr $ StdLib.name StdLib.printFunction) args'))
    <$> mapM (fmap snd . analyzeExpr) args

analyzeExprPrintlnFuncCall :: [Ast.Expression] -> Result (Maybe AType.Type, AAst.Expression)
analyzeExprPrintlnFuncCall args =
  (\args' -> (Nothing, AAst.ExprFuncCall (stdLibFuncExpr $ StdLib.name StdLib.printlnFunction) args'))
    <$> mapM (fmap snd . analyzeExpr) args

analyzeExprPanicFuncCall :: Ast.Expression -> Result (Maybe AType.Type, AAst.Expression)
analyzeExprPanicFuncCall arg = do
  (argT, argE) <- analyzeExpr arg
  checkEq (Just AType.TString) argT
  return (Nothing, AAst.ExprFuncCall (stdLibFuncExpr $ StdLib.name StdLib.panicFunction) [argE])

---------------------------------------------------------Types----------------------------------------------------------

analyzeType :: Ast.Type -> Result AType.Type
analyzeType = \case
  Ast.TInt -> return AType.TInt
  Ast.TBool -> return AType.TBool
  Ast.TString -> return AType.TString
  Ast.TArray arrType -> uncurry AType.TArray <$> analyzeArrayType arrType
  Ast.TFunction funcType -> AType.TFunction <$> analyzeFunctionType funcType

analyzeArrayType :: Ast.ArrayType -> Result (AType.Type, Int)
analyzeArrayType (Ast.ArrayType elementT len) =
  (,) <$> analyzeType elementT <*> liftCEC (simplifyConstIntExpr len)

analyzeFunctionType :: Ast.FunctionType -> Result AType.FunctionType
analyzeFunctionType (Ast.FunctionType paramsTs retT) =
  AType.FunctionType <$> mapM analyzeType paramsTs <*> mapM analyzeType retT

---------------------------------------------------------Utils----------------------------------------------------------

analyzeExpr' :: Ast.Expression -> Result (AType.Type, AAst.Expression)
analyzeExpr' = analyzeExpr >=> unwrapExprRes

analyzeIntExpr :: Ast.Expression -> Result AAst.Expression
analyzeIntExpr = analyzeExpr' >=> (\(t, expr) -> checkEq AType.TInt t $> expr)

stdLibFuncExpr :: AAst.Identifier -> AAst.Expression
stdLibFuncExpr = AAst.ExprValue . AAst.ValFunction . AAst.Function . AAst.StdLibFunction

checkEq :: Eq a => a -> a -> Result ()
checkEq lhs rhs = checkCondition $ lhs == rhs

checkCondition :: Bool -> Result ()
checkCondition cond = if cond then return () else throwError MismatchedTypes

unwrapJust :: Maybe a -> Result a
unwrapJust = maybe (throwError MismatchedTypes) return

unwrapExprRes :: (Maybe AType.Type, AAst.Expression) -> Result (AType.Type, AAst.Expression)
unwrapExprRes (t, expr) = (,expr) <$> unwrapJust t

liftCEC :: Either CEC.Err a -> Result a
liftCEC cecRes = liftEither (mapLeft mapErr cecRes)
  where
    mapErr = \case
      CEC.MismatchedTypes -> MismatchedTypes
      CEC.DivisionByZero -> DivisionByZero
      CEC.NotInIntBounds -> NotInIntBounds
      CEC.NotConstExpr -> MismatchedTypes
