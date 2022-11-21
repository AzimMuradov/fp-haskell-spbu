{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Interpreter (interpret, getInterpretationOut) where

import qualified Ast
import Data.Bits (Bits (..))
import Data.Map (Map, (!), (!?))
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.Text (Text, append)
import Errors (unreachable')
import StdLib (stdLibFuncs)
import Prelude hiding (id)

-- | Interpreter entry point. Assumes that program is checked.
interpret :: Ast.Program -> Result ()
interpret = interpretProgram

getInterpretationOut :: Result () -> Either Error AccOut
getInterpretationOut res = do
  Success (Env _ out) _ <- res
  Right $ reverse out

interpretProgram :: Ast.Program -> Result ()
interpretProgram (Ast.Program _ funcs) = do
  s <- interpretFunc (Env [Scope funcsScopeMap] []) (funcsMap ! "main") []
  Right s {result = ()}
  where
    funcSelector f = let Ast.FunctionDef id _ = f in id
    funcsMap = Map.fromList $ (funcSelector <$> (stdLibFuncs ++ funcs)) `zip` (Ast.funcLit <$> (stdLibFuncs ++ funcs))
    funcsScopeMap = Map.fromList $ (funcSelector <$> (stdLibFuncs ++ funcs)) `zip` (Ast.LitFunction . Ast.funcLit <$> (stdLibFuncs ++ funcs))

interpretFunc :: Env -> Ast.FunctionLiteral -> [Ast.Literal] -> Result (Maybe Ast.Value)
interpretFunc env (Ast.FunctionLiteral (Ast.FunctionSignature params _) body) args = do
  s@(Success _ res) <- interpretStmts env (Scope $ Map.fromList $ (fst <$> params) `zip` args) body
  case res of
    Ret val -> Right $ s {result = val}
    Unit -> Left NoReturn
interpretFunc env@(Env _ accOut) (Ast.StdLibFunction _ impl) args =
  let (res, out) = impl args
   in Right $ Success env {out = out ++ accOut} res
interpretFunc _ _ _ = unreachable'

interpretStmt :: Env -> Ast.Statement -> Result StmtInterpretationResult
interpretStmt env stmt = case stmt of
  Ast.StmtReturn expr -> case expr of
    Just expr' -> do
      Success env' v' <- interpretExpr env expr'
      Right $ Success env' $ Ret v'
    Nothing -> Right $ Success env $ Ret Nothing
  Ast.StmtBreak -> undefined -- TODO
  Ast.StmtContinue -> undefined -- TODO
  Ast.StmtFor _ -> undefined -- TODO
  Ast.StmtVarDecl (Ast.VarDecl varSpecs) -> case varSpecs of
    [Ast.VarSpec name _ expr] -> do
      Success env' v' <- interpretExpr env expr
      Right $ Success (addNewIdentifier env' name $ fromJust v') Unit
    _ -> undefined -- TODO
  Ast.StmtIfElse (Ast.IfElse _ condition block _) -> do
    -- TODO : Else, SimpleStmt
    s@(Success env' condV) <- interpretExpr env condition
    let Just (Ast.LitBool condBool) = condV
    if condBool
      then interpretStmts env' (Scope Map.empty) block
      else Right $ s {result = Unit}
  Ast.StmtBlock _ -> undefined -- TODO
  Ast.StmtSimple simpleStmt -> case simpleStmt of
    Ast.StmtAssignment _ _ -> undefined -- TODO
    Ast.StmtInc _ -> undefined -- TODO
    Ast.StmtDec _ -> undefined -- TODO
    Ast.StmtShortVarDecl _ _ -> undefined -- TODO
    Ast.StmtExpression expr -> do
      s <- interpretExpr env expr
      Right s {result = Unit}

interpretStmts :: Env -> Scope -> [Ast.Statement] -> Result StmtInterpretationResult
interpretStmts env scope stmts = do
  s@(Success env' _) <- foldl f (Right $ Success (pushScope env scope) Unit) stmts
  Right s {env = popScope env'}
  where
    f res stmt = do
      s@(Success env' r') <- res
      case r' of
        Ret _ -> Right s
        Unit -> interpretStmt env' stmt

    pushScope env@(Env scopes _) scope = env {scopes = scope : scopes}

    popScope env@(Env (_ : scopes) _) = env {scopes = scopes}
    popScope _ = undefined

interpretExpr :: Env -> Ast.Expression -> Result (Maybe Ast.Value)
interpretExpr env expr = case expr of
  Ast.ExprLiteral lit -> Right $ Success env $ Just lit
  Ast.ExprIdentifier id -> Right $ Success env $ Just $ getIdentifierValue env id
  Ast.ExprUnaryOp unOp expr -> do
    Success env' v <- interpretExpr env expr
    let ok v' = Right $ Success env' $ Just v'
    case (unOp, v) of
      (Ast.UnaryPlusOp, Just (Ast.LitInt v')) -> ok $ Ast.LitInt v'
      (Ast.UnaryMinusOp, Just (Ast.LitInt v')) -> ok $ Ast.LitInt $ -v'
      (Ast.NotOp, Just (Ast.LitBool v')) -> ok $ Ast.LitBool $ not v'
      (Ast.BitwiseComplementOp, Just (Ast.LitInt v')) -> ok $ Ast.LitInt $ complement v'
      _ -> undefined -- Impossible route
  Ast.ExprBinaryOp Ast.OrOp lhs rhs -> do
    Success env' v <- interpretExpr env lhs
    let Just (Ast.LitBool v') = v
    if v'
      then Right $ Success env' $ Just $ Ast.LitBool True
      else do
        Success env'' v'' <- interpretExpr env' rhs
        Right $ Success env'' $ Just $ fromJust v''
  Ast.ExprBinaryOp Ast.AndOp lhs rhs -> do
    Success env' v <- interpretExpr env lhs
    let Just (Ast.LitBool v') = v
    if not v'
      then Right $ Success env' $ Just $ Ast.LitBool False
      else do
        Success env'' v'' <- interpretExpr env' rhs
        Right $ Success env'' $ Just $ fromJust v''
  Ast.ExprBinaryOp binOp lhs rhs -> do
    Success env' t' <- interpretExpr env lhs
    Success env'' t'' <- interpretExpr env' rhs
    let ok v' = Right $ Success env'' $ Just v'
    case (binOp, t', t'') of
      (Ast.MulOp Ast.MultOp, Just (Ast.LitInt lhs'), Just (Ast.LitInt rhs')) -> ok $ Ast.LitInt $ lhs' * rhs'
      (Ast.MulOp Ast.DivOp, Just (Ast.LitInt lhs'), Just (Ast.LitInt rhs')) -> ok $ Ast.LitInt $ lhs' `div` rhs' -- TODO
      (Ast.MulOp Ast.ModOp, Just (Ast.LitInt lhs'), Just (Ast.LitInt rhs')) -> ok $ Ast.LitInt $ lhs' `mod` rhs' -- TODO
      (Ast.MulOp Ast.BitShiftLeftOp, Just (Ast.LitInt _), Just (Ast.LitInt _)) -> undefined -- TODO
      (Ast.MulOp Ast.BitShiftRightOp, Just (Ast.LitInt _), Just (Ast.LitInt _)) -> undefined -- TODO
      (Ast.MulOp Ast.BitClearOp, Just (Ast.LitInt _), Just (Ast.LitInt _)) -> undefined -- TODO
      (Ast.MulOp Ast.BitAndOp, Just (Ast.LitInt lhs'), Just (Ast.LitInt rhs')) -> ok $ Ast.LitInt $ lhs' .&. rhs'
      (Ast.AddOp Ast.PlusOp, Just (Ast.LitInt lhs'), Just (Ast.LitInt rhs')) -> ok $ Ast.LitInt $ lhs' + rhs'
      (Ast.AddOp Ast.PlusOp, Just (Ast.LitString lhs'), Just (Ast.LitString rhs')) -> ok $ Ast.LitString $ append lhs' rhs'
      (Ast.AddOp Ast.MinusOp, Just (Ast.LitInt lhs'), Just (Ast.LitInt rhs')) -> ok $ Ast.LitInt $ lhs' - rhs'
      (Ast.AddOp Ast.BitOrOp, Just (Ast.LitInt lhs'), Just (Ast.LitInt rhs')) -> ok $ Ast.LitInt $ lhs' .|. rhs'
      (Ast.AddOp Ast.BitXorOp, Just (Ast.LitInt lhs'), Just (Ast.LitInt rhs')) -> ok $ Ast.LitInt $ lhs' `xor` rhs'
      (Ast.RelOp Ast.EqOp, Just (Ast.LitInt lhs'), Just (Ast.LitInt rhs')) -> ok $ Ast.LitBool $ lhs' == rhs'
      (Ast.RelOp Ast.NeOp, Just (Ast.LitInt lhs'), Just (Ast.LitInt rhs')) -> ok $ Ast.LitBool $ lhs' /= rhs'
      (Ast.RelOp Ast.EqOp, Just _, Just _) -> undefined -- TODO
      (Ast.RelOp Ast.NeOp, Just _, Just _) -> undefined -- TODO
      (Ast.RelOp Ast.LeOp, Just (Ast.LitInt lhs'), Just (Ast.LitInt rhs')) -> ok $ Ast.LitBool $ lhs' <= rhs'
      (Ast.RelOp Ast.LtOp, Just (Ast.LitInt lhs'), Just (Ast.LitInt rhs')) -> ok $ Ast.LitBool $ lhs' < rhs'
      (Ast.RelOp Ast.MeOp, Just (Ast.LitInt lhs'), Just (Ast.LitInt rhs')) -> ok $ Ast.LitBool $ lhs' >= rhs'
      (Ast.RelOp Ast.MtOp, Just (Ast.LitInt lhs'), Just (Ast.LitInt rhs')) -> ok $ Ast.LitBool $ lhs' > rhs'
      _ -> unreachable'
  Ast.ExprFuncCall func args -> do
    Success env' v' <- interpretExpr env func
    case fromJust v' of
      Ast.LitFunction func -> do
        Success env'' values <- interpretExprs env' args
        interpretFunc env'' func values
      _ -> Left Npe
  Ast.ExprArrayAccessByIndex _ _ -> undefined -- TODO

interpretExprs :: Env -> [Ast.Expression] -> Result [Ast.Value]
interpretExprs env = foldl acc (Right $ Success env [])
  where
    acc res expr = do
      Success env' vs <- res
      Success env'' v <- interpretExpr env' expr
      Right $ Success env'' (fromJust v : vs)

getIdentifierValue :: Env -> Ast.Identifier -> Ast.Value
getIdentifierValue (Env scopes _) id = fromJust $ foldl acc Nothing scopes
  where
    acc t (Scope ids) = case t of
      Nothing -> ids !? id
      _ -> t

addNewIdentifier :: Env -> Ast.Identifier -> Ast.Value -> Env
addNewIdentifier env@(Env (Scope ids : scopes) _) name v = env {scopes = Scope (Map.insert name v ids) : scopes}
addNewIdentifier _ _ _ = undefined

-- Interpretation result

-- | Represents the result of interpretation.
type Result a = Either Error (Success a)

-- | Represents unsuccessful interpretation.
data Error
  = -- | Identifier redeclaration error
    DivisionByZero
  | -- | Identifier redeclaration error
    IndexOutOfBounds
  | -- | Identifier redeclaration error
    NoReturn
  | -- | Null dereference error
    Npe
  deriving (Show)

-- | Represents successful interpretation,
-- it contains the current environment and the result of interpretation.
data Success a = Success {env :: Env, result :: a}
  deriving (Show)

-- | Environment contains scopes stack
data Env = Env {scopes :: [Scope], out :: AccOut}
  deriving (Show)

-- | Scope contains identifiers mapped to their types
newtype Scope = Scope {vars :: Map Ast.Identifier Ast.Value}
  deriving (Show)

type AccOut = [Text]

data StmtInterpretationResult = Unit | Ret (Maybe Ast.Value)
