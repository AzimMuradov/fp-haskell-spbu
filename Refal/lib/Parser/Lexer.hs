module Lexer where

import           Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token    as Tok


lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser style
  where
    names = ["&ENTRY"]
    style =
      emptyDef
        { Tok.commentLine = "*"
        , Tok.commentStart = "/*"
        , Tok.commentEnd = "*/"
        , Tok.caseSensitive = True
        , Tok.reservedNames = names
        }

integer = Tok.natural lexer -- int

parens = Tok.parens lexer -- ( _ )  

angles = Tok.angles lexer -- < _ >    activation (evaluation)

braces = Tok.braces lexer -- { _ }    begin end

comma = Tok.comma lexer -- _ , _    where

semi = Tok.semi lexer -- _ ; _    otherwise

dot = Tok.dot lexer -- _ . _    index follows s.1 e.2 t.3

sym = Tok.symbol lexer

lexeme = Tok.lexeme lexer

identifier = Tok.identifier lexer