module Main where

import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  let f = case args of
        ["-i"] -> interpretAndShow
        ["--interpret"] -> interpretAndShow
        ["-p"] -> parseAndShow
        ["--parse"] -> parseAndShow
        ["-h"] -> const helpMsg
        ["--help"] -> const helpMsg
        [] -> interpretAndShow
        _ -> const "Wrong usage, to call help use `--help` or `-h`.\n"
   in interact f

interpretAndShow :: String -> String
interpretAndShow fileText = "TODO : interpreter\n"

parseAndShow :: String -> String
parseAndShow fileText = "TODO : parser\n"

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
