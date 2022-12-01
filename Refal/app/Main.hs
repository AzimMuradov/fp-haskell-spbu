module Main where

import           System.Environment (getArgs)

import Interpreter ( interpret )
import Parser ( progParser )

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> putStrLn "Provide file name!"
    [filename] -> do
      prog <- readFile filename
      case progParser prog of
        Left err   -> print err
        Right code -> interpret code
    _ -> putStrLn "Provide one file name!"
