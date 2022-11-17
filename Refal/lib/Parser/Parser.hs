module Parser where

import           Control.Applicative ((<$>))
import qualified Data.Map            as Map (empty)
import           Data.Maybe
import           Text.Parsec
import qualified Text.Parsec.Expr    as Ex
import           Text.Parsec.String  (Parser)
import qualified Text.Parsec.Token   as Tok

import           AST
import           Lexer

betweenWs :: Char -> Parser a -> Char -> Parser a
betweenWs o vals c = char o *> vals <* char c

{-
An identifier is a sequence of characters which begins with a letter
and may include letters, digits, and characters dash - , and underscore _ .
-}
identifier :: Parser FName
identifier = do
  c <- letter
  cs <- many (letter <|> digit <|> char '-' <|> char '_')
  return (c : cs)

-- >>> parse Parser.identifier "" "a_sdasd-sadsd asd"
-- Right "a_sdasd-sadsd"
--
{-
Ids of variables. s/t/e.asd123 parses into s/t/e
-}
indicator :: Parser TypeInd
indicator = (char 's' <|> char 't' <|> char 'e') <* char '.'
-- >>> parse indicator "" "s.31"
-- (Error while loading modules for evaluation)
-- [4 of 5] Compiling Parser           ( /home/jennsy/StudyStuff/Функционально программиров/Refal/lib/Parser/Parser.hs, interpreted )
-- <BLANKLINE>
-- /home/jennsy/StudyStuff/Функционально программиров/Refal/lib/Parser/Parser.hs:73:1: error:
--     parse error (possibly incorrect indentation or mismatched brackets)
-- Failed, three modules loaded.
--

-- >>> parse integer "" "123 "
-- Right 123
--
compound :: Parser Symbol
compound = lexeme $
  Comp <$>
  between
    (char '"')
    (char '"')
    (many (notFollowedBy (char '\\') *> letter <|> space))

-- >>> parse compound "" "\"asdads\" "
-- Right Comp "asdads"
--
macrodigit :: Parser Symbol
macrodigit = MacroDigit <$> integer

-- >>> parse macrodigit "" "1231sss1111132"
-- Right MDig 1231
--

empty = string "" 

sym :: Parser Symbol
sym = choice [ try macrodigit, compound]


var :: Parser Var
var = choice $ try <$> [
    var' 's' SVar,
    var' 't' TVar,
    var' 'e' EVar]
    where
        var' ch consr = char ch *> char '.' *> (consr <$> Parser.identifier)
        
term :: Parser Term
term = try (Sym <$> Parser.sym)
    <|> try (Variable <$> var)
    <|> Par <$> parens expr

expr :: Parser Expr
expr = try (Cons <$> term <*> expr)  <|> AST.Empty <$ empty


symParser :: String -> Either ParseError Expr
symParser = parse (expr <* eof) "asdasd"

-- >>> parser expr "" ""
-- ( ("abc") "d" (("gh") "j" ()) )