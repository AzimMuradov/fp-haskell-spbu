module Main where

import           Parser

import           AST

import           Test.Tasty
import           Test.Tasty.HUnit
import           Text.Parsec

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Unit" [unitTests]

unitTests :: TestTree
unitTests =
  testGroup
    "Parser"
    [ idTest
    , cmpTest
    , symTest
    , varTest
    , termTest
    , exprTest
    , condTest
    , fAppTest
    , fExprTest
    , senTest
    , blockTest
    , fDefTest
    ]

idTest :: TestTree
idTest = testGroup "Identifier" [usrTest]

usrTest :: TestTree
usrTest =
  testGroup
    "identifier"
    [ testCase "Over Mul" $
      parse (entry identifier') "" " Mul123 " @?= Right "Mul123"
    , testCase "Default sting" $
      parse (entry identifier') "" " as1_ADS- " @?= Right "as1_ADS-"
    , testCase "Under Sub" $
      parse (entry identifier') "" " S " @?= Right "S"
    ]

cmpTest :: TestTree
cmpTest =
  testGroup
    "Compound"
    [ testCase "Random symbol" $
      parse (entry compound) "" "\"asd ads\"" @?= Right (Comp "asd ads")
    , testCase "With \\" $
      parse (entry compound) "" "\" \r\t\n asd \\ \'\"" @?=
      Right (Comp " \r\t\n asd \\ \'")
    , testCase "with \\xXX" $
      parse (entry compound) "" "\"I AM \x00 \"" @?= Right (Comp "I AM \x00 ")
    ]

symTest :: TestTree
symTest =
  testGroup
    "Symbol"
    [ testCase "Macrodigit" $
      parse (entry symbol) "" " 213 " @?= Right (MDig 213)
    , testCase "Identifier" $
      parse (entry symbol) "" " asd_ASD- " @?= Right (ID "asd_ASD-")
    , testCase "Char" $ parse (entry symbol) "" " 'a' " @?= Right (Ch 'a')
    , testCase "Compound" $
      parse (entry symbol) "" " \"i am symbol \" " @?=
      Right (Comp "i am symbol ")
    ]

varTest :: TestTree
varTest =
  testGroup
    "Variable"
    [ testCase "S-Variable" $
      parse (entry var) "" " s.a_1b2 " @?= Right (SVar "a_1b2")
    , testCase "T-Variable" $
      parse (entry var) "" " t.asdAS " @?= Right (TVar "asdAS")
    , testCase "E-Variable" $ parse (entry var) "" " e.a " @?= Right (EVar "a")
    ]

termTest =
  testGroup
    "Term"
    [ testCase "Symbol" $
      parse (entry term) "" " \'a\' " @?= Right (Sym $ Ch 'a')
    , testCase "Variable" $
      parse (entry term) "" " s.a12 " @?= Right (Var $ SVar "a12")
    , testCase "Parenthesis" $
      parse (entry term) "" " (   s.a12  ) " @?= Right (Par [Var $ SVar "a12"])
    ]

exprTest =
  testGroup
    "Expression"
    [ testCase "Empty" $ parse (entry expr) "" " " @?= Right []
    , testCase "Not empty" $
      parse (entry expr) "" " s.a12 " @?= Right [Var $ SVar "a12"]
    , testCase "Complicated" $
      parse (entry expr) "" "( s.a12    (\"sad\")   \'a\' )  () " @?=
      Right
        [Par [Var $ SVar "a12", Par [Sym $ Comp "sad"], Sym $ Ch 'a'], Par []]
    ]

condTest =
  testGroup
    "Stcition"
    [ testCase "Empty" $ parse (entry condition) "" "   " @?= Right Nil
    , testCase "Simple" $
      parse (entry condition) "" " , \'a\' \'a\': s.c s.c" @?=
      Right
        (WIs
           [Term $ Sym $ Ch 'a', Term $ Sym $ Ch 'a']
           [Var $ SVar "c", Var $ SVar "c"]
           Nil)
    , testCase "Complicated" $
      parse (entry condition) "" " , 'a' 'b' : e.f, 'e': s.c " @?=
      Right
        (WIs
           [Term $ Sym $ Ch 'a', Term $ Sym $ Ch 'b']
           [Var $ EVar "f"]
           (WIs [Term $ Sym $ Ch 'e'] [Var $ SVar "c"] Nil))
    ]

fAppTest =
  testGroup
    "Application"
    [ testCase "User defined" $
      parse (entry fApp) "" " <some s.N 'a'> " @?=
      Right (FApp "some" [Term $ Var $ SVar "N", Term $ Sym $ Ch 'a'])
    , testCase "Built-in bin ops" $
      parse (entry fApp) "" " <Sub s.N 'a'> " @?=
      Right (FApp "Sub" [Term $ Var $ SVar "N", Term $ Sym $ Ch 'a'])
    ]

fExprTest =
  testGroup
    "Fun expression"
    [ testCase "Empty" $ parse (entry fExpr) "" "  " @?= Right []
    , testCase "Expression w\\o app" $
      parse (entry fExpr) "" "( s.a12    (\"sad\")   \'a\' )  () " @?=
      Right
        [ Term $ Par [Var $ SVar "a12", Par [Sym $ Comp "sad"], Sym $ Ch 'a']
        , Term $ Par []
        ]
    , testCase "Fun Expr" $
      parse (entry fExpr) "" " ('a' ) 2 <Go 213 'a'> " @?=
      Right
        [ Term $ Par [Sym $ Ch 'a']
        , Term $ Sym $ MDig 2
        , FAct $ FApp "Go" [Term $ Sym $ MDig 213, Term $ Sym $ Ch 'a']
        ]
    ]

senTest =
  testGroup
    "Sentence"
    [ testCase "Empty left" $
      parse (entry sentence) "" "   = <Mul 4 5>    " @?=
      Right
        (Stc
           []
           Nil
           [FAct $ FApp "Mul" [Term $ Sym $ MDig 4, Term $ Sym $ MDig 5]])
    , testCase "Standard" $
      parse (entry sentence) "" " s.b 'a' = <Mul s.b 5>   " @?=
      Right
        (Stc
           [Var $ SVar "b", Sym $ Ch 'a']
           Nil
           [FAct $ FApp "Mul" [Term $ Var $ SVar "b", Term $ Sym $ MDig 5]])
    , testCase "W/ condition" $
      parse (entry sentence) "" "s.a s.b, <Al>: s.a s.b = \'T\' " @?=
      Right
        (Stc
           [Var $ SVar "a", Var $ SVar "b"]
           (WIs [FAct $ FApp "Al" []] [Var $ SVar "a", Var $ SVar "b"] Nil)
           [Term $ Sym $ Ch 'T'])
    ]

blockTest =
  testGroup
    "Block"
    [ testCase "E" $
      parse (entry block) "" "0 = 1; s.N = \'a\'; " @?=
      Right
        [ Stc [Sym $ MDig 0] Nil [Term $ Sym $ MDig 1]
        , Stc [Var $ SVar "N"] Nil [Term $ Sym $ Ch 'a']
        ]
    ]

fDefTest =
  testGroup
    "Fun definition"
    [ testCase "Entry fun" $
      parse (entry fDefine) "" "$ENTRY Go {  0 = 1; } " @?=
      Right (Entry "Go" [Stc [Sym $ MDig 0] Nil [Term $ Sym $ MDig 1]])
    , testCase "Not Entry fun" $
      parse (entry fDefine) "" "MyFun {'a' s.n = <Mul 1 s.n>; } " @?=
      Right
        (NEntry
           "MyFun"
           [ Stc
               [Sym $ Ch 'a', Var $ SVar "n"]
               Nil
               [FAct $ FApp "Mul" [Term $ Sym $ MDig 1, Term $ Var $ SVar "n"]]
           ])
    ]
