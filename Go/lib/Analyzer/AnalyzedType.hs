-- | Types representation during the analysis process.
module Analyzer.AnalyzedType where

-- | Type representation during the analysis process.
data Type
  = -- | 32-bit/64-bit (depending on the machine) integer type.
    TInt
  | -- | Boolean type.
    TBool
  | -- | String type.
    TString
  | -- | Array type, it contains the length of the array and its elements type.
    TArray Type Int
  | -- | Function type, see 'FunctionType'.
    TFunction FunctionType
  | -- | Null type.
    TNil
  deriving (Show, Eq)

-- | Function type,
-- it contains the result of the function (which can be @void@ if the result is equal to 'Nothing')
-- and its parameters types.
data FunctionType = FunctionType {parameters :: [Type], returnType :: Maybe Type}
  deriving (Show, Eq)
