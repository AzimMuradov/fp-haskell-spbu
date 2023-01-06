{-# LANGUAGE InstanceSigs #-}

module Interpreter.Runtime where

import Ast (Expr, Identifier)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)

type Env = [Frame]

type Frame = [Scope]

type Scope = Map Identifier IValue

data IValue
  = IVBool Bool
  | IVInt Integer
  | IVDouble Double
  | IVFun [Identifier] [Expr] -- ( fun x y -> x + y )
  deriving (Show, Eq)

instance Ord IValue where
  compare :: IValue -> IValue -> Ordering
  compare (IVInt valL) (IVInt valR) = compare valL valR
  compare (IVDouble valL) (IVDouble valR) = compare valL valR
  compare _ _ = undefined

instance Num IValue where
  (+) :: IValue -> IValue -> IValue
  (+) (IVInt valL) (IVInt valR) = IVInt (valL + valR)
  (+) (IVDouble valL) (IVDouble valR) = IVDouble (valL + valR)
  (+) _ _ = undefined
  (*) :: IValue -> IValue -> IValue
  (*) (IVInt valL) (IVInt valR) = IVInt (valL * valR)
  (*) (IVDouble valL) (IVDouble valR) = IVDouble (valL * valR)
  (*) _ _ = undefined
  abs :: IValue -> IValue
  abs (IVInt val) = IVInt $ abs val
  abs (IVDouble val) = IVDouble $ abs val
  abs _ = undefined
  signum :: IValue -> IValue
  signum (IVInt val) = IVInt $ signum val
  signum (IVDouble val) = IVDouble $ signum val
  signum _ = undefined
  fromInteger :: Integer -> IValue
  fromInteger = IVInt
  negate :: IValue -> IValue
  negate (IVInt v) = IVInt $ -v
  negate (IVDouble v) = IVDouble $ -v
  negate _ = undefined

getVar :: Identifier -> Env -> IValue
getVar var (scopes : _) = getVar' var scopes
  where
    getVar' x (sc : scs) = fromMaybe (getVar' x scs) (sc M.!? var)
    getVar' _ [] = undefined
getVar _ [] = undefined

addVar :: Identifier -> IValue -> Env -> Env
addVar x val ((sc : scs) : fs) = (M.insert x val sc : scs) : fs
addVar x val ([] : fs) = [M.singleton x val] : fs
addVar x val [] = [[M.singleton x val]]

pushScope :: Env -> Env
pushScope (scs : fs) = (M.empty : scs) : fs
pushScope [] = [[M.empty]]

popScope :: Env -> Env
popScope ((_ : scs) : fs) = scs : fs
popScope ([] : fs) = fs
popScope [] = []

pushFrame :: Env -> Env
pushFrame = ([] :)

popFrame :: Env -> Env
popFrame = tail
