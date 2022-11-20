module Parser where

import           Data.Maybe
import           Text.Parsec
import           Text.Parsec.String (Parser)
import qualified Text.Parsec.Token  as Tok

import           AST
import           Lexer

-------------------------------Expressions--DONE----------------------------------
empty = sym ""

entry :: Parser a -> Parser a
entry p = spaces *> p <* eof

-- >>> parse (entry identifier') "" " Sub "
-- >>> parse (entry identifier') "" " as1_ADS- " 
-- Right - 
-- Right "as1_ADS-"
--
identifier' :: Parser FName
identifier' =
  lexeme $
  try $
  Op <$> choice [Add <$ string "Add", Sub <$ string "Sub", Mul <$ string "Mul"] <|> do
    c <- letter
    cs <- many (letter <|> digit <|> char '-' <|> char '_')
    return $ Usr (c : cs)


-- >>> parse (entry char') "" " \'s\'   "
-- Right 's'
--
char' :: Parser Symbol
char' =
  lexeme $ do
    Ch <$>
      between (char '\'') (char '\'') (notFollowedBy (char '\\') *> anyChar)

-- >>> parse (entry compound) "" "  \"asd ads\""
-- Right "asd ads"
--
compound :: Parser Symbol
compound =
  lexeme $ do
    Comp <$>
      between
        (char '"')
        (char '"')
        (many (notFollowedBy (char '\\') *> letter <|> space))

-- >>> parse (entry macrodigit) "" " 1231  "
-- Right 1231
--
macrodigit :: Parser Symbol
macrodigit = lexeme $ do MDig <$> integer

-- >>> parse (entry symbol) "" " 213 "
-- >>> parse (entry symbol) "" " asd_ASD- "
-- >>> parse (entry symbol) "" " 'a' "
-- >>> parse (entry symbol) "" " \"i am symbol \" "
-- Right 213
-- Right id_"asd_ASD-"
-- Right 'a'
-- Right "i am symbol "
--
symbol :: Parser Symbol
symbol = do
  choice $ try <$> [macrodigit, compound, char', ID <$> identifier']

-- >>> parse (entry var) "" "  s.a_1-2   "
-- Right s."a_1-2"
--
var :: Parser Var
var = lexeme $ do choice $ try <$> [var' 's' SVar, var' 't' TVar, var' 'e' EVar]
  where
    var' ch constr = char ch *> char '.' *> (constr <$> identifier')

-- >>> parse (entry term) "" " \'a\' "
-- >>> parse (entry term) "" " s.a12 "
-- >>> parse (entry term) "" " (        s.a12  )   "
-- Right 'a'
-- Right s."a12"
-- Right ( s."a12" )
--
term :: Parser Term
term = do
  try (Var <$> var) <|> try (Sym <$> symbol) <|> Par <$> parens expr

-- >>> parse (entry expr) "" "\'a\'"
-- >>> parse (entry expr) "" "s.a12"
-- >>> parse (entry expr) "" "( s.a12    (sad)   \'a\' )  () "
-- Right 'a'
-- Right s."a12"
-- Right ( s."a12" ( id_"sad" ) 'a' ) ( )
--
expr :: Parser Expr
expr = try (Cons <$> term <*> expr) <|> Empt <$ empty

----------------------------------Program--DONE----------------------------------
-- >>> parse (entry condition) "" " , 'a' 'b' : e.f, 'e': s.c "
-- Right , 'a' 'b'  : e."f" , 'e'  : s."c"
--
condition :: Parser Cond
condition =
  try (WIs <$> (comma *> expr <* sym ":") <*> expr <*> condition) <|>
  Nil <$ empty

-- >>> parse (entry fApp) "" " <some s.N 1> "
-- >>> parse (entry fApp) "" " <Sub s.N 1> "
-- Right <"some" s."N" 1 >
-- Right < -  s."N" 1 >
--
fApp :: Parser FApp
fApp = angles (FApp <$> identifier' <*> fExpr)

-- >>> parse (entry fExpr) "" " ('a' ('b' 'f')) 2 <Go 213 'a'>   "
-- Right ( 'a' ( 'b' 'f' ) ) 2 <"Go" 213 'a' >
--
fExpr :: Parser FExpr
fExpr =
  try (FTCons <$> term <*> fExpr) <|> try (FACons <$> fApp <*> fExpr) <|>
  FEmpt <$ empty

-- >>> parse (entry sentence) "" "  'a' = <Mul <Fact <Sub s.N 1>> s.N>    "
-- Right 'a' = < *  <"Fact" < -  s."N" 1 >>s."N" >
--
sentence :: Parser Sentence
sentence = do
  left <- expr
  cond <- condition
  sym "="
  try (Cond left cond <$> fExpr)

-- >>> parse (entry block) ""  "0 = 1; s.N = \'a\'; "
-- Right [0 = 1 ,s."N" = 'a' ]
--
block :: Parser [Sentence]
block =
  many $ do
    sen <- sentence
    semi
    return sen

-- >>> parse (entry fDefine) ""  "$ENTRY go {  0 = 1; } "
-- Right (Entry "go" [0 = 1 ])
--
fDefine :: Parser FDefinition
fDefine =
  do try (lexeme $ string "$ENTRY")
     Entry <$> identifier' <*> braces block
     <|> NEntry <$> identifier' <*> braces block

-- >>> parse (entry program) ""  "$ENTRY Go { = <Prout <Fact 4>>; } Fact { 0 = 1; s.N = <Mul <Fact <Sub s.N 1>> s.N>;}"
-- Right [Entry "Go" [= <"Prout" <"Fact" 4 >>],NEntry "Fact" [0 = 1 ,s."N" = < *  <"Fact" < -  s."N" 1 >>s."N" >]]
--
program :: Parser [FDefinition]
program = many fDefine

progParser :: String -> Either ParseError Program
progParser = parse (program <* eof) ""
