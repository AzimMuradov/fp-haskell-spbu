module Main where

import qualified Analyzer.AnalyzedAst as AAst
import Analyzer.Analyzer (analyze)
import qualified Analyzer.Result as AResult
import Data.Either.Extra (fromEither, mapLeft, maybeToEither)
import Data.Text (Text, pack, unpack)
import Data.Void (Void)
import Interpreter.Interpreter (getInterpretationOut, interpret)
import qualified Interpreter.Result as IResult
import Options.Applicative
import qualified Parser.Ast as Ast
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
interpretAndShow fileText = fromEither (interpreterResultMapper' fileText (\(out, err) -> unpack out ++ maybe "" ((++ "\n") . unpack) err))

analyzeAndShow :: String -> String
analyzeAndShow fileText = fromEither (analyzerResultMapper' fileText ((++ "\n") . show))

parseAndShow :: String -> String
parseAndShow = parseResultMsg

debug :: String -> String
debug fileText = parseResultMsg fileText ++ "\n" ++ analyzerResultMsg fileText ++ "\n" ++ interpreterResultMsg fileText

-- ** Utils

-- *** Interpreter

interpreterResult :: String -> Either String (IResult.ResultValue (), IResult.Env')
interpreterResult fileText = analyzerResultMapper' fileText interpret

interpreterResultMapper :: String -> ((IResult.ResultValue (), IResult.Env') -> a) -> Either String a
interpreterResultMapper fileText f = f <$> interpreterResult fileText

interpreterResultMapper' :: String -> ((Text, Maybe Text) -> a) -> Either String a
interpreterResultMapper' fileText f = f <$> interpreterResultMapper fileText getInterpretationOut

interpreterResultMsg :: String -> String
interpreterResultMsg fileText = fromEither (interpreterResultMapper fileText ((++ "\n") . show))

-- *** Analyzer

analyzerResult :: String -> Either String (AResult.ResultValue AAst.Program, AResult.Env)
analyzerResult fileText = parseResultMapper fileText analyze

analyzerResultMapper :: String -> ((AResult.ResultValue AAst.Program, AResult.Env) -> a) -> Either String a
analyzerResultMapper fileText f = f <$> analyzerResult fileText

analyzerResultMapper' :: String -> (AAst.Program -> a) -> Either String a
analyzerResultMapper' fileText f = analyzerResultMapper fileText fst >>= (fmap f . mapLeft ((++ "\n") . show))

analyzerResultMsg :: String -> String
analyzerResultMsg fileText = fromEither (analyzerResultMapper fileText ((++ "\n") . show))

-- *** Parser

parseResult :: String -> Maybe Ast.Program
parseResult fileText = parse $ pack fileText

parseResultMapper :: String -> (Ast.Program -> a) -> Either String a
parseResultMapper fileText f = f <$> maybeToEither "parse failed\n" (parseResult fileText)

parseResultMsg :: String -> String
parseResultMsg fileText = fromEither (parseResultMapper fileText ((++ "\n") . show))

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
