{-# LANGUAGE InstanceSigs #-}
module Interpreter.PrettyPrint where
import Interpreter.Runtime (IValue (IVInt, IVDouble, IVBool, IVFun, IVMeasureDecl, IVFunDecl, IVRecFunDecl, IVVarDecl))
import Data.Text (unpack)
import Interpreter.Interpreter (InterpError (DivisionByZero, Unreachable))

type Prec = String

class PrettyInterp p where
  prettyInterp :: p -> String
  prettyInterp = prettyPrecInterp ""

  prettyPrecInterp :: Prec -> p -> String
  prettyPrecInterp _ = prettyInterp

instance PrettyInterp IValue where
  prettyPrecInterp :: Prec -> IValue -> String
  prettyPrecInterp s (IVInt int) = if s == "" then show int else "val it: " <> s <> " = " <> show int
  prettyPrecInterp s (IVDouble d) = if s == "" then show d else "val it: " <> s <> " = " <> show d
  prettyPrecInterp s (IVBool b) = if s == "" then show b else "val it: " <> s <> " = " <> show b
  prettyPrecInterp s (IVFun _ _) = if s == "" then "" else "val it: " <> s
  prettyPrecInterp s (IVVarDecl var val) = "val " <> unpack var <> ": " <> s <> " = " <> prettyInterp val
  prettyPrecInterp s (IVMeasureDecl ident) = "[<Measure>]\ntype " <> unpack ident <> " = " <> s
  prettyPrecInterp s (IVFunDecl f _) = "val " <> unpack f <> ": " <> s
  prettyPrecInterp s (IVRecFunDecl f _) = "val " <> unpack f <> ": " <> s

instance PrettyInterp InterpError where
  prettyPrecInterp _ DivisionByZero = "Attempted to divide by zero."
  prettyPrecInterp _ Unreachable = "Unreachable situation."