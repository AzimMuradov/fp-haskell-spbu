module Main where

import qualified Analyzer.AnalysisResult
import qualified Analyzer.AnalyzedAst
import Analyzer.Analyzer (analyze)
import Data.Text (pack)
import Data.Void (Void)
import Interpreter.Interpreter (interpret)
import Options.Applicative
import qualified Parser.Ast
import Parser.Parser (parse)
import Text.Megaparsec (Parsec, choice, parseMaybe)
import Text.Megaparsec.Char (char)

-- * Main

main :: IO ()
main = runApp =<< execParser opts
  where
    opts =
      info
        (appP <**> helper)
        ( fullDesc
            <> header "-- Mini Go Interpreter --"
            <> progDesc "minigo can interpret, analyze, or parse any mini-go file"
        )

-- * Run app

runApp :: App -> IO ()
runApp App {mode = m, input = i} = runApp' (getRunner m) i
  where
    runApp' :: (String -> String) -> Input -> IO ()
    runApp' f (FileInput path) = readFile path >>= \s -> putStr (f s)
    runApp' f StdInput = interact f

-- ** Subcommands

getRunner :: Mode -> (String -> String)
getRunner m = case m of
  Interpret -> interpretAndShow
  Analyze -> analyzeAndShow
  Parse -> parseAndShow
  Debug -> debug

interpretAndShow :: String -> String
interpretAndShow fileText = interpreterResultMsg fileText ++ "\n"

-- interpretAndShow fileText = getInterpretationOut . interpret $ fromJust $ parse (pack fileText)

analyzeAndShow :: String -> String
analyzeAndShow fileText = analyzerResultMapper fileText (show . fst) ++ "\n"

parseAndShow :: String -> String
parseAndShow fileText = parseResultMsg fileText ++ "\n"

debug :: String -> String
debug fileText = parseResultMsg fileText ++ "\n\n" ++ analyzerResultMsg fileText ++ "\n"

-- ** Utils

parseResult :: String -> Maybe Parser.Ast.Program
parseResult fileText = parse $ pack fileText

parseResultMapper :: String -> (Parser.Ast.Program -> String) -> String
parseResultMapper fileText f = maybe "parse failed" f (parseResult fileText)

parseResultMsg :: String -> String
parseResultMsg fileText = parseResultMapper fileText show

analyzerResultMapper :: String -> ((Analyzer.AnalyzedAst.Program, Analyzer.AnalysisResult.Env) -> String) -> String
analyzerResultMapper fileText f = parseResultMapper fileText $ either (const "analysis failed") f . analyze

analyzerResultMsg :: String -> String
analyzerResultMsg fileText = analyzerResultMapper fileText show

interpreterResultMsg :: String -> String
interpreterResultMsg fileText = analyzerResultMapper fileText $ interpret . fst

-- * Command line options parsing

data App = App {mode :: Mode, input :: Input}
  deriving (Show)

data Mode
  = Interpret
  | Analyze
  | Parse
  | Debug
  deriving (Show)

data Input
  = StdInput
  | FileInput FilePath
  deriving (Show)

appP :: Parser App
appP = App <$> modeP <*> inputP

modeP :: Parser Mode
modeP =
  option
    (maybeReader $ parseMaybe modeParser)
    ( long "mode"
        <> short 'm'
        <> metavar "i|a|p|d"
        <> value Interpret
        <> showDefaultWith (const "i")
        <> help "Set minigo mode, either \"interpreter\" (i), \"analyzer\" (a), \"parser\" (p), or \"debugger\" (d)"
    )
  where
    modeParser :: Parsec Void String Mode
    modeParser = choice [Interpret <$ char 'i' <|> Analyze <$ char 'a' <|> Parse <$ char 'p' <|> Debug <$ char 'd']

inputP :: Parser Input
inputP =
  FileInput
    <$> strOption
      ( long "file"
          <> short 'f'
          <> metavar "FILENAME"
          <> help "Read from the file (optional)"
      )
    <|> pure StdInput
