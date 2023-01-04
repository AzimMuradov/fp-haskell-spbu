module Parser where

import           Data.Maybe         ()
import           Text.Parsec
import           Text.Parsec.String (Parser)

import           AST
import           Lexer

-------------------------------Expressions------------------------------------
empty :: Parser String
empty = sym ""

entry :: Parser a -> Parser a
entry p = spaces *> p <* eof


-- tested
identifier' :: Parser FName
identifier' =
  lexeme $ do
    c <- letter
    cs <- many (letter <|> digit <|> char '-' <|> char '_')
    return (c : cs)

char' :: Parser Symbol
char' =
  lexeme $
  Ch <$> between (char '\'') (char '\'') (notFollowedBy (char '\\') *> anyChar)


-- tested
compound :: Parser Symbol
compound =
  lexeme $ Comp <$> between (char '\"') (char '\"') (many $ noneOf "\"")

macrodigit :: Parser Symbol
macrodigit = lexeme $ MDig <$> integer


-- tested
symbol :: Parser Symbol
symbol = choice $ try <$> [macrodigit, compound, char', ID <$> identifier']


-- tested
var :: Parser Var
var = lexeme $ choice $ try <$> [var' 's' SVar, var' 't' TVar, var' 'e' EVar]
  where
    var' ch constr = char ch *> char '.' *> (constr <$> identifier)


-- tested
term :: Parser Term
term = try (Var <$> var) <|> try (Sym <$> symbol) <|> Par <$> parens expr


-- tested
expr :: Parser Pattern
expr = many term


----------------------------------Program------------------------------------
-- tested
condition :: Parser Cond
condition =
  try (WIs <$> (comma *> fExpr <* sym ":") <*> expr <*> condition) <|>
  Nil <$ empty


-- tested
fApp :: Parser FApp
fApp = angles (FApp <$> identifier' <*> fExpr)


-- tested
fExpr :: Parser FExpr
fExpr = many $ try (Term <$> term) <|> (FAct <$> fApp)
  -- try (FTCons <$> term <*> fExpr) <|> try (FACons <$> fApp <*> fExpr) <|>
  -- FEmpt <$ empty


-- tested
sentence :: Parser Sentence
sentence = do
  left <- expr
  cond <- condition
  _ <- sym "="
  try (Stc left cond <$> fExpr)


-- tested. TODO not all with ';'
block :: Parser [Sentence]
block =
  many $ do
    sen <- sentence
    _ <- semi
    return sen


-- tested
fDefine :: Parser FDefinition
fDefine =
  do _ <- try (lexeme $ string "$ENTRY")
     Entry <$> identifier' <*> braces block
     <|> NEntry <$> identifier' <*> braces block

program :: Parser [FDefinition]
program = many fDefine

progParser :: String -> Either ParseError Program
progParser = parse (program <* eof) ""
