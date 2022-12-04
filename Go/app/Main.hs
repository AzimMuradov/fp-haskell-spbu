module Main where

import Analyzer.Analyzer (analyze)
import Data.Text (pack)
import Parser.Parser (parse)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  let f = case args of
        ["-i"] -> interpretAndShow
        ["--interpret"] -> interpretAndShow
        ["-a"] -> analyzeAndShow
        ["--analyze"] -> analyzeAndShow
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
interpretAndShow _ = undefined
-- interpretAndShow fileText = getInterpretationOut . interpret $ fromJust $ parse (pack fileText)

analyzeAndShow :: String -> String
analyzeAndShow fileText = show (analyze <$> parse (pack fileText)) ++ "\n"

parseAndShow :: String -> String
parseAndShow fileText = show (parse $ pack fileText) ++ "\n"

debug :: String -> String
debug _ = undefined

-- debug fileText =
--   show (parse $ pack fileText)
--     ++ "\n\n"
--     ++ show (check <$> parse (pack fileText))
--     ++ "\n\n"
--     ++ show (interpret <$> parse (pack fileText))
--     ++ "\n"

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
      "-   --analyze   | (-a)        - analyze mini-go file                           -",
      "-   --parse     | (-p)        - parse mini-go file                             -",
      "-   --debug     | (-d)        - debug program using given mini-go file         -",
      "-   --help      | (-h)        - print this message                             -",
      "--------------------------------------------------------------------------------"
    ]
