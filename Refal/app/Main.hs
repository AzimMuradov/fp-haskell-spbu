module Main where

import           System.Environment (getArgs)

import           Interpreter        (interpret)
import           Parser             (progParser)
import           Prettier           (prettify)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> putStrLn "Provide file name!"
    filename:[option] -> do
      prog <- readFile filename
      case progParser prog of
        Left err -> print err
        Right code ->
          case option of
            "-i" -> interpret code
            "-p" -> prettify filename code
            _    -> putStrLn "Unknown option"
    _ -> putStrLn "Provide one file name!"
