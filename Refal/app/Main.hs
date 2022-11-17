module Main where

import           System.Environment (getArgs)

import           Parser

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> putStrLn "Provide file name!"
    [filename] -> do
      expr <- readFile filename
      case symParser expr of
        Left err  -> print err
        Right sym -> print sym
    _ -> putStrLn "Provide one file name!"
