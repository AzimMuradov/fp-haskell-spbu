module Result where

import qualified Ast
import Data.Map (Map)

-- Interpretation Result

type IResult a = Either IError (ISuccess a)

data IError = UndefinedError

data ISuccess a = ISuccess
  { env :: Env,
    out :: AccOut,
    result :: a
  }

-- | Environment
data Env = Env
  { scopeStack :: [Scope],
    funcs :: Map Ast.Identifier Ast.FunctionDef
  }

-- | Scope
newtype Scope = Scope {vars :: Map Ast.Identifier Ast.Value}

type AccOut = [String]
