module Main where

import Data.Maybe (fromJust)
import Data.Text (pack)
import Interpreter (getInterpretationOut, interpret)
import Parser (parse)
import ProgramChecker (check)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  let f = case args of
        ["-i"] -> interpretAndShow
        ["--interpret"] -> interpretAndShow
        ["-c"] -> checkAndShow
        ["--check"] -> checkAndShow
        ["-p"] -> parseAndShow
        ["--parse"] -> parseAndShow
        ["-d"] -> debug
        ["--debug"] -> debug
        ["-h"] -> const helpMsg
        ["--help"] -> const helpMsg
        [] -> interpretAndShow
        _ -> const "Wrong usage, to call help use `--help` or `-h`.\n"
   in interact f

interpretAndShow :: String -> String
interpretAndShow fileText = (getInterpretationOut . interpret $ fromJust $ parse (pack fileText)) ++ "\n"

checkAndShow :: String -> String
checkAndShow fileText = show (check <$> parse (pack fileText)) ++ "\n"

parseAndShow :: String -> String
parseAndShow fileText = show (parse $ pack fileText) ++ "\n"

debug :: String -> String
debug fileText =
  show (parse $ pack fileText)
    ++ "\n\n"
    ++ show (check <$> parse (pack fileText))
    ++ "\n\n"
    ++ show (interpret <$> parse (pack fileText))
    ++ "\n"

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
      "-   --check     | (-c)        - check mini-go file                             -",
      "-   --parse     | (-p)        - parse mini-go file                             -",
      "-   --debug     | (-d)        - debug program using given mini-go file         -",
      "-   --help      | (-h)        - print this message                             -",
      "--------------------------------------------------------------------------------"
    ]
