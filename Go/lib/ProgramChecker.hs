{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module ProgramChecker (check) where

import qualified Ast
import Control.Monad (void)
import Data.Either.Combinators (mapRight)
import Data.List (find)
import Data.Map (Map, (!?))
import qualified Data.Map as Map
import Data.Maybe (isJust, isNothing)
import qualified Data.Set as Set
import Data.Text (unpack)
import Errors (todo, unreachable')
import StdLib (stdLibFuncs)
import Prelude hiding (id)

-- | Checker entry point
check :: Ast.Program -> Result ()
check = checkProgram

-- TODO : Check for identifier collisions with predeclared

checkProgram :: Ast.Program -> Result ()
checkProgram (Ast.Program _ funcs) = do
  (Success _ fs) <- uniqueFuncs
  if isNothing $ find (\f -> funcSelector f == "main") fs
    then Left NoMain
    else checkFuncs fs
  where
    funcSelector f = let Ast.FunctionDef id _ = f in id
    uniqueFuncs = checkIfHasDuplicates (stdLibFuncs ++ funcs) funcSelector

checkFuncs :: [Ast.FunctionDef] -> Result ()
checkFuncs funcs = foldl f (Right $ Success emptyEnv ()) funcs
  where
    f res (Ast.FunctionDef _ (Ast.FunctionLiteral (Ast.FunctionSignature params ret) stmts)) = do
      void res
      checkStmts emptyEnv (Scope (Map.fromList params)) ret stmts
    f res _ = void res *> Right (Success emptyEnv ())

    emptyEnv = Env [Scope $ Map.fromList $ convertToIdTypePair <$> funcs]

    convertToIdTypePair (Ast.FunctionDef id (Ast.FunctionLiteral (Ast.FunctionSignature params ret) _)) =
      (id, Ast.TFunction $ Ast.FunctionType (snd <$> params) ret)
    convertToIdTypePair (Ast.FunctionDef id (Ast.StdLibFunction t _)) = (id, Ast.TFunction t)
    convertToIdTypePair _ = unreachable'

checkStmt :: Env -> Maybe Ast.Type -> Ast.Statement -> Result ()
checkStmt env ret stmt = case stmt of
  Ast.StmtReturn expr -> case expr of
    Just expr' -> do
      Success _ t <- checkExpr env expr'
      if t == ret then Right $ Success env () else Left MismatchedTypes
    Nothing -> if isNothing ret then Right $ Success env () else Left MismatchedTypes
  Ast.StmtBreak -> todo $ unpack "`break` statement checker" -- TODO
  Ast.StmtContinue -> todo $ unpack "`continue` statement checker" -- TODO
  Ast.StmtFor _ -> todo $ unpack "`for` statement checker" -- TODO
  Ast.StmtVarDecl (Ast.VarDecl varSpecs) -> case varSpecs of
    [Ast.VarSpec id (Just t) expr] -> do
      Success _ t' <- checkExpr env expr
      if isJust t' && Just t == t'
        then addNewIdentifierOrFail env id t
        else Left MismatchedTypes
    [Ast.VarSpec id Nothing expr] -> do
      Success _ t <- checkExpr env expr
      case t of
        Just t' -> addNewIdentifierOrFail env id t'
        Nothing -> Left MismatchedTypes
    _ -> undefined -- TODO
  Ast.StmtIfElse (Ast.IfElse _ condition block _) -> do
    -- TODO : Else, SimpleStmt
    Success _ condT <- checkExpr env condition
    if condT == Just Ast.TBool
      then checkStmts env (Scope Map.empty) ret block
      else Left MismatchedTypes
  Ast.StmtBlock _stmts -> undefined -- TODO
  Ast.StmtSimple simpleStmt -> case simpleStmt of
    Ast.StmtAssignment _ _ -> undefined -- TODO
    Ast.StmtInc _ -> todo $ unpack "`increment` statement checker" -- TODO
    Ast.StmtDec _ -> todo $ unpack "`decrement` statement checker" -- TODO
    Ast.StmtShortVarDecl _ _ -> todo $ unpack "`short var declaration` statement checker" -- TODO
    Ast.StmtExpression expr -> do
      void $ checkExpr env expr
      return $ Success env ()

checkExpr :: Env -> Ast.Expression -> Result (Maybe Ast.Type)
checkExpr env@(Env scopes) expr = case expr of
  Ast.ExprLiteral lit -> do
    let ok t = Right $ Success env (Just t)
    case lit of
      Ast.LitInt _ -> ok Ast.TInt
      Ast.LitBool _ -> ok Ast.TBool
      Ast.LitString _ -> ok Ast.TString
      Ast.LitArray (Ast.ArrayLiteral t _) -> ok $ Ast.TArray t
      Ast.LitFunction _ -> undefined -- TODO
  Ast.ExprIdentifier id -> case getIdentifierType scopes id of
    Just t -> Right $ Success env $ Just t
    Nothing -> Left IdentifierNotFound
  Ast.ExprUnaryOp unOp expr -> do
    exprS@(Success _ t) <- checkExpr env expr
    let ok = Right exprS
    let err = Left MismatchedTypes
    case (unOp, t) of
      (Ast.NotOp, Just Ast.TBool) -> ok
      (_, Just Ast.TInt) -> ok
      _ -> err
  Ast.ExprBinaryOp binOp lhs rhs -> do
    Success _ t' <- checkExpr env lhs
    Success _ t'' <- checkExpr env rhs
    let ok t = Right $ Success env (Just t)
    let ok' = Right $ Success env t''
    let err = Left MismatchedTypes
    case (binOp, t', t'') of
      (Ast.OrOp, Just Ast.TBool, Just Ast.TBool) -> ok'
      (Ast.AndOp, Just Ast.TBool, Just Ast.TBool) -> ok'
      (Ast.RelOp Ast.EqOp, Just _, Just _) | t' == t'' -> ok Ast.TBool
      (Ast.RelOp Ast.NeOp, Just _, Just _) | t' == t'' -> ok Ast.TBool
      (Ast.RelOp _, Just Ast.TInt, Just Ast.TInt) -> ok Ast.TBool
      (Ast.RelOp _, Just Ast.TString, Just Ast.TString) -> ok Ast.TBool
      (Ast.AddOp Ast.PlusOp, Just Ast.TString, Just Ast.TString) -> ok'
      (Ast.AddOp _, Just Ast.TInt, Just Ast.TInt) -> ok'
      (Ast.MulOp _, Just Ast.TInt, Just Ast.TInt) -> ok'
      _ -> err
  Ast.ExprFuncCall func args -> do
    Success _ t <- checkExpr env func
    case t of
      Just (Ast.TFunction (Ast.FunctionType paramsTs ret)) -> checkExprsTypes args paramsTs (Success env ret)
      _ -> Left MismatchedTypes
  Ast.ExprArrayAccessByIndex _ _ -> undefined -- TODO

checkExprsTypes :: [Ast.Expression] -> [Ast.Type] -> Success a -> Result a
checkExprsTypes exprs expectedTs success@(Success env _) =
  foldl checkExprType (Right $ Success env ()) (exprs `zip` expectedTs) *> Right success
  where
    checkExprType res (expr, expectedT) = do
      void res
      Success _ actualT <- checkExpr env expr
      if actualT == Just expectedT
        then Right $ Success env ()
        else Left MismatchedTypes

checkStmts :: Env -> Scope -> Maybe Ast.Type -> [Ast.Statement] -> Result ()
checkStmts env initScope ret stmts =
  mapRight popScope $ foldl f (Right $ Success (pushScope env initScope) ()) stmts
  where
    f res stmt = do
      Success env' _ <- res
      checkStmt env' ret stmt

    pushScope (Env scopes) initScope = Env $ initScope : scopes

    popScope s@(Success (Env (_ : scopes)) _) = s {env = Env scopes}
    popScope _ = unreachable'

checkIfHasDuplicates :: Ord b => [a] -> (a -> b) -> Result [a]
checkIfHasDuplicates list selector =
  if hasDuplicates $ selector <$> list
    then Left IdentifierRedeclaration
    else Right $ Success (Env []) list

hasDuplicates :: (Ord a) => [a] -> Bool
hasDuplicates list = length list /= length (Set.fromList list)

getIdentifierType :: [Scope] -> Ast.Identifier -> Maybe Ast.Type
getIdentifierType scopes id = foldl acc Nothing scopes
  where
    acc t (Scope ids) = case t of
      Nothing -> ids !? id
      _ -> t

addNewIdentifierOrFail :: Env -> Ast.Identifier -> Ast.Type -> Result ()
addNewIdentifierOrFail (Env (Scope ids : scopes)) id t = case ids !? id of
  Just _ -> Left IdentifierRedeclaration
  Nothing -> Right $ Success (Env $ Scope (Map.insert id t ids) : scopes) ()
addNewIdentifierOrFail _ _ _ = unreachable'

-- Program checker result

-- | Represents the result of checking
type Result a = Either Err (Success a)

-- | Represents unsuccessful checking
data Err
  = -- | Identifier redeclaration error
    IdentifierRedeclaration
  | -- | Identifier not found error
    IdentifierNotFound
  | -- | Mismatched types error
    MismatchedTypes
  | -- | No entry point for the interpreter error
    NoMain
  deriving (Show)

-- | Represents successful checking,
-- it contains the current environment and the result of checking
data Success a = Success {env :: Env, res :: a}
  deriving (Show)

-- | Environment contains scopes stack
newtype Env = Env [Scope]
  deriving (Show)

-- | Scope contains identifiers mapped to their types
newtype Scope = Scope {ids :: Map Ast.Identifier Ast.Type}
  deriving (Show)
