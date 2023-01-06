module Prettier where

import           AST

prettify :: String -> Program -> IO ()
prettify filename program = writeFile filename (pProg program)
  where
    pProg :: Program -> String
    pProg [] = ""
    pProg (fdef:fdefx)
      | null fdefx = pFdef fdef
      | otherwise  = pFdef fdef ++ "\n" ++ pProg fdefx
    pFdef :: FDefinition -> String
    pFdef fdef =
      case fdef of
        Entry name snts ->
          "$ENTRY " ++ name ++ " {\n" ++ pBlock snts ++ "}\n"
        NEntry name snts -> name ++ " {\n" ++ pBlock snts ++ "}\n"
    pBlock :: [Sentence] -> String
    pBlock []     = ""
    pBlock (s:sx) = pSnt s ++ pBlock sx
    pSnt :: Sentence -> String
    pSnt (Stc l c r) =
      '\t' : (pPatt l) ++ show c ++ " = " ++ (pFexpr r) ++ ";\n"
    pPatt :: Pattern -> String
    pPatt [] = ""
    pPatt (t:tx)
      | null tx   = show t
      | otherwise = show t ++ " " ++ pPatt tx
    pFexpr :: FExpr -> String
    pFexpr [] = ""
    pFexpr (Term t:fexs)
      | null fexs = show t
      | otherwise = show t ++ " " ++ pFexpr fexs
    pFexpr (FAct act:fexs) = pFact act ++ pFexpr fexs
    pFact :: FApp -> String
    pFact (FApp name fexp) = '<' : name ++ " " ++ pFexpr fexp ++ ">"
