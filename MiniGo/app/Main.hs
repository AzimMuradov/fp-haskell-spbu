module Main where

import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  let f = case args of
        ["-i"] -> show . interpret
        ["--interpret"] -> show . interpret
        ["-p"] -> show . parse
        ["--parse"] -> show . parse
        ["-h"] -> const helpMsg
        ["--help"] -> const helpMsg
        [] -> show . interpret
        _ -> const "Wrong usage, to call help use `--help` or `-h`.\n"
   in interact f

interpret :: String -> String
interpret fileText = "TODO : interpreter\n"

parse :: String -> String
parse fileText = "TODO : parser\n"

helpMsg :: String
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
      "-   --parse     | (-p)        - parse mini-go file                             -",
      "-   --help      | (-h)        - print this message                             -",
      "--------------------------------------------------------------------------------"
    ]