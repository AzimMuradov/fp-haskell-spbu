module Main where

import qualified Ast
import Data.Text (Text, dropWhileEnd, isSuffixOf, pack, splitOn, strip, unlines, unpack)
import Data.Void (Void)
import Options.Applicative
import Parser (parse)
import System.IO (hFlush, stdout)
import Prelude hiding (concat, unlines)

import TypeInference.Runtime
import TypeInference.PrettyPrint

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


-- runApp :: App -> IO ()
-- runApp (App i) = runApp' i
--   where
--     runApp' :: Input -> IO ()
--     runApp' (FileInput path) = do
--       text <- readFile path
--       mapM_ putStrLn (show . parse <$> splitOn (pack ";;") (pack text))
--     runApp' StdInput =
--       let repl lines = do
--             putStr "> " >> hFlush stdout
--             line <- getLine
--             let lineText = pack line
--             if pack ";;" `isSuffixOf` strip lineText
--               then print (parse $ unlines $ reverse (dropWhileEnd (== ';') lineText : lines)) >> repl []
--               else repl (lineText : lines)
--        in repl []

runApp :: App -> IO ()
runApp (App i) = runApp' i
  where
    runApp' :: Input -> IO ()
    runApp' (FileInput path) = do
      text <- readFile path
      mapM_ putStrLn (eval . parse <$> splitOn (pack ";;") (pack text))
    runApp' StdInput =
      let repl lines = do
            putStr "> " >> hFlush stdout
            line <- getLine
            let lineText = pack line
            if pack ";;" `isSuffixOf` strip lineText
              then putStrLn (eval . parse $ unlines $ reverse (dropWhileEnd (== ';') lineText : lines)) >> repl []
              else repl (lineText : lines)
       in repl []


-- eval' :: String -> IO ()
-- eval' s = case parse (pack s) of
--   Nothing -> print (pack "err")
--   Just (Ast.Program p) -> case inferPolytype p of
--     Left tyerr -> print $ pretty tyerr
--     Right ty -> putStrLn $ show p ++ " : " ++ show ty


eval :: (Maybe Ast.Program) -> String
eval s = case s of
  Nothing -> "err"
  Just (Ast.Program p) -> case inferPolytype p of
    Left tyerr -> pretty tyerr
    Right ty -> show p ++ " : " ++ pretty ty

-- main :: IO ()
-- main = evalRepl (const (pure "HM> ")) (liftIO . eval') [] Nothing Nothing (Word (const (return []))) (return ()) (return Exit)
