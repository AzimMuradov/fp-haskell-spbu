module Main where

import Data.Char (toUpper)

import Data.Maybe (fromJust)
import Data.Text (pack)

import System.Environment (getArgs)
import Parser (parse)

-- main :: IO () 
-- main = do
--   args <- getArgs
--   let f = case args of
--         ["-p"] -> parseAndShow 
--         ["--parse"] -> parseAndShow
--         ["-h"] -> const helpMsg
--         ["--help"] -> const helpMsg
--         _ -> const "Wrong usage, to call help use `--help` or `-h`.\n"
--    in putStrLn $ f . filter (/= '\n')
  
main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> putStrLn "Provide file name!"
    [filename, opt] -> do
      prog <- readFile filename
      let f = case opt of 
            "-p" -> parseAndShow prog
            "--parse" -> parseAndShow prog
            "-h" -> helpMsg
            "--help" -> helpMsg
            _ -> "Wrong usage, to call help use `--help` or `-h`.\n"
        in putStrLn f
    _ -> putStrLn "Provide one file name!"

parseAndShow :: String -> String
parseAndShow fileText = show (parse $ pack fileText) ++ "\n"

helpMsg :: String -- putStrLn $ show (parse (pack "let f x y = x + y"))
helpMsg =
  unlines
    [ "                                                                                ",
      "--------------------------Mini Go Parser & Interpreter--------------------------",
      "- Run:                                                                         -",
      "-   `minigo [arg]`                                                             -",
      "-   `cat file | minigo [arg]`                                                  -",
      "-                                                                              -",
      "- Arguments:                                                                   -",
      "-   --interpret | (-i)        - interpret mini-go file                         -",
      "-   --check     | (-c)        - check mini-go file                             -",
      "-   --parse     | (-p)        - parse mini-go file                             -",
      "-   --debug     | (-d)        - debug program using given mini-go file         -",
      "-   --help      | (-h)        - print this message                             -",
      "--------------------------------------------------------------------------------"
    ]