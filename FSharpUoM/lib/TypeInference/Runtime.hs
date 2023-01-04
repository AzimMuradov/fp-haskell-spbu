{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module TypeInference.Runtime where

import Ast (Program (Program), Statement)
import Control.Category ((>>>))
import Control.Monad.Except
import Control.Monad.Reader
import Control.Unification.IntVar (evalIntBindingT)
import Data.Functor.Identity (Identity (runIdentity))
import qualified Data.Map as M
import Data.Text (pack)
import Parser (parse)
import System.Console.Repline
  ( CompleterStyle (Word),
    ExitDecision (Exit),
    evalRepl,
  )
import TypeInference.HindleyMilner
  ( Infer,
    Polytype,
    TypeError,
    UType,
    applyBindings,
    fromUPolytype,
    generalize,
  )
import TypeInference.PrettyPrint (Pretty (pretty))
import TypeInference.TIRealization (inferStatement)

runInfer :: Infer UType -> Either TypeError Polytype
runInfer =
  (>>= applyBindings)
    >>> (>>= (generalize >>> fmap fromUPolytype))
    >>> flip runReaderT M.empty
    >>> runExceptT
    >>> evalIntBindingT
    >>> runIdentity

inferPolytype :: [Statement] -> Either TypeError Polytype
inferPolytype = runInfer . inferStatement

-- eval :: String -> IO ()
-- eval s = case parse expr "" s of
--   Left err -> print err
--   Right e -> case inferPolytype e of
--     Left tyerr -> putStrLn $ pretty tyerr
--     Right ty   -> do
--       putStrLn $ pretty e ++ " : " ++ pretty ty
--       when (ty == Forall [] TyInt) $ putStrLn $ pretty (interp e)

-- eval :: Maybe Ast.Program -> String
-- eval s = case s of
--   Nothing -> "err"
--   Just (Ast.Program p) -> case inferPolytype p of
--     Left tyerr -> pretty tyerr
--     Right ty -> show p ++ " : " ++ pretty ty

-- main :: IO ()
-- main = evalRepl (const (pure "HM> ")) (liftIO . eval') [] Nothing Nothing (Word (const (return []))) (return ()) (return Exit)
