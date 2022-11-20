module Main where

  import           System.Environment (getArgs)
  
  import           Interpreter
  import           Parser
  
  main :: IO ()
  main = do
    args <- getArgs
    case args of
      [] -> putStrLn "Provide file name!"
      [filename] -> do
        program <- readFile filename
        case progParser program of
          Left err      -> print err
          Right program -> interpret program
      _ -> putStrLn "Provide one file name!"