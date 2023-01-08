module Main where

import qualified Parser.Ast as Ast
import Data.Text (dropWhileEnd, isSuffixOf, pack, strip, unlines)
import Options.Applicative
import Parser.Parser (parse, fileP, programP)
import System.IO (hFlush, stdout)
import Prelude hiding (concat, unlines, lines)

import TypeInference.Runtime
import TypeInference.PrettyPrint

import Interpreter.Interpreter (interp)
import Interpreter.PrettyPrint

-- * Main

main :: IO ()
main = runApp =<< execParser opts
  where
    opts =
      info
        (appP <**> helper)
        ( fullDesc
            <> header "F Sharp with Units Of Measure"
            <> progDesc "This program can interpret f# code"
        )

-- Command line options

newtype App = App {input :: Input}
  deriving (Show)

data Input
  = StdInput
  | FileInput FilePath
  deriving (Show)

appP :: Parser App
appP = App <$> inputP

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

-- Run app

runApp :: App -> IO ()
runApp (App i) = runApp' i
  where
    runApp' :: Input -> IO ()
    runApp' (FileInput path) = do
      text <- readFile path
      mapM_ putStrLn (eval <$> sequence (parse fileP (pack text)))
    runApp' StdInput =
      let repl lines = do
            putStr "> " >> hFlush stdout
            line <- getLine
            let lineText = pack line
            if pack ";;" `isSuffixOf` strip lineText
              then putStrLn (eval . parse programP $ unlines $ reverse (dropWhileEnd (== ';') lineText : lines)) >> repl []
              else repl (lineText : lines)
       in repl []

eval :: Maybe Ast.Program -> String
eval s = case s of
  Nothing -> "Please, try again. Can't parse your program."
  Just st@(Ast.Program p) -> case inferPolytype p of
    Left tyerr -> pretty tyerr
    Right ty -> case interp st of
      Left interpErr -> prettyInterp interpErr
      Right interpVal -> prettyPrecInterp (pretty ty) interpVal