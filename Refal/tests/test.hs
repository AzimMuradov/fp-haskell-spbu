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
idTest = testGroup "Identifier" [usrTest, opTest]

usrTest :: TestTree
usrTest =
  testGroup
    "Usr identifier"
    [ testCase "Over Mul" $
      parse (entry identifier') "" " Mul123 " @?= Right (Usr "Mul123")
    , testCase "Default sting" $
      parse (entry identifier') "" " as1_ADS- " @?= Right (Usr "as1_ADS-")
    , testCase "Under Sub" $
      parse (entry identifier') "" " S " @?= Right (Usr "S")
    ]

opTest :: TestTree
opTest =
  testGroup
    "Bin Op identifier"
    [ testCase "Multiplication:" $
      parse (entry identifier') "" " Mul " @?= Right (Op Mul)
    , testCase "Addition" $
      parse (entry identifier') "" " Add " @?= Right (Op Add)
    , testCase "Subtraction" $
      parse (entry identifier') "" " Sub " @?= Right (Op Sub)
    ]

cmpTest :: TestTree
cmpTest =
  testGroup
    "Compound"
    [ testCase "Random sumbol" $
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
      parse (entry symbol) "" " asd_ASD- " @?= Right (ID $ Usr "asd_ASD-")
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
      parse (entry term) "" " (   s.a12  ) " @?=
      Right (Par $ Cons (Var $ SVar "a12") Empt)
    ]

exprTest =
  testGroup
    "Expression"
    [ testCase "Empty" $ parse (entry expr) "" " " @?= Right Empt
    , testCase "Not empty" $
      parse (entry expr) "" " s.a12 " @?= Right (Cons (Var $ SVar "a12") Empt)
    , testCase "Complicated" $
      parse (entry expr) "" "( s.a12    (\"sad\")   \'a\' )  () " @?=
      Right
        (Cons
           (Par $
            Cons
              (Var $ SVar "a12")
              (Cons
                 (Par $ Cons (Sym $ Comp "sad") Empt)
                 (Cons (Sym $ Ch 'a') Empt)))
           (Cons (Par $ Empt) Empt))
    ]

condTest =
  testGroup
    "Condition"
    [ testCase "Empty" $ parse (entry condition) "" "   " @?= Right Nil
    , testCase "Simple" $
      parse (entry condition) "" " , \'a\' \'a\': s.c s.c" @?=
      Right
        (WIs
           (FTCons (Sym $ Ch 'a') (FTCons (Sym $ Ch 'a') FEmpt))
           (Cons (Var $ SVar "c") (Cons (Var $ SVar "c") Empt))
           Nil)
    , testCase "Complicated" $
      parse (entry condition) "" " , 'a' 'b' : e.f, 'e': s.c " @?=
      Right
        (WIs
           (FTCons (Sym $ Ch 'a') (FTCons (Sym $ Ch 'b') FEmpt))
           (Cons (Var $ EVar "f") Empt)
           (WIs (FTCons (Sym $ Ch 'e') FEmpt) (Cons (Var $ SVar "c") Empt) Nil))
    ]

fAppTest =
  testGroup
    "Application"
    [ testCase "User defined" $
      parse (entry fApp) "" " <some s.N 'a'> " @?=
      Right
        (FApp
           (Usr $ "some")
           (FTCons (Var $ SVar "N") (FTCons (Sym $ Ch 'a') FEmpt)))
    , testCase "Built-in bin ops" $
      parse (entry fApp) "" " <Sub s.N 'a'> " @?=
      Right
        (FApp (Op $ Sub) (FTCons (Var $ SVar "N") (FTCons (Sym $ Ch 'a') FEmpt)))
    ]

fExprTest =
  testGroup
    "Fun expression"
    [ testCase "Empty" $ parse (entry fExpr) "" "  " @?= Right FEmpt
    , testCase "Expression w\\o app" $
      parse (entry fExpr) "" "( s.a12    (\"sad\")   \'a\' )  () " @?=
      Right
        (FTCons
           (Par $
            Cons
              (Var $ SVar "a12")
              (Cons
                 (Par $ Cons (Sym $ Comp "sad") Empt)
                 (Cons (Sym $ Ch 'a') Empt)))
           (FTCons (Par $ Empt) FEmpt))
    , testCase "Fun Expr" $
      parse (entry fExpr) "" " ('a' ) 2 <Go 213 'a'> " @?=
      Right
        (FTCons
           (Par $ Cons (Sym $ Ch 'a') Empt)
           (FTCons
              (Sym $ MDig 2)
              (FACons
                 (FApp
                    (Usr "Go")
                    (FTCons (Sym $ MDig 213) (FTCons (Sym $ Ch 'a') FEmpt)))
                 FEmpt)))
    ]

senTest =
  testGroup
    "Sentence"
    [ testCase "Empty left" $
      parse (entry sentence) "" "   = <Mul 4 5>    " @?=
      Right
        (Cond
           Empt
           Nil
           (FACons
              (FApp
                 (Op $ Mul)
                 (FTCons (Sym $ MDig 4) (FTCons (Sym $ MDig 5) FEmpt)))
              (FEmpt)))
    , testCase "Standard" $
      parse (entry sentence) "" " s.b 'a' = <Mul s.b 5>   " @?=
      Right
        (Cond
           (Cons (Var $ SVar "b") (Cons (Sym $ Ch 'a') Empt))
           Nil
           (FACons
              (FApp
                 (Op $ Mul)
                 (FTCons (Var $ SVar "b") (FTCons (Sym $ MDig 5) FEmpt)))
              (FEmpt)))
    , testCase "W/ condition" $
      parse (entry sentence) "" "s.a s.b, <Al>: s.a s.b = \'T\' " @?=
      Right
        (Cond
           (Cons (Var $ SVar "a") (Cons (Var $ SVar "b") Empt))
           (WIs
              (FACons (FApp (Usr "Al") FEmpt) FEmpt)
              (Cons (Var $ SVar "a") (Cons (Var $ SVar "b") Empt))
              Nil)
           (FTCons (Sym $ Ch 'T') FEmpt))
    ]

blockTest =
  testGroup
    "Block"
    [ testCase "E" $
      parse (entry block) "" "0 = 1; s.N = \'a\'; " @?=
      Right
        [ Cond (Cons (Sym $ MDig 0) Empt) Nil (FTCons (Sym $ MDig 1) FEmpt)
        , Cond (Cons (Var $ SVar "N") Empt) Nil (FTCons (Sym $ Ch 'a') FEmpt)
        ]
    ]

fDefTest =
  testGroup
    "Fun definition"
    [ testCase "Entry fun" $
      parse (entry fDefine) "" "$ENTRY Go {  0 = 1; } " @?=
      Right
        (Entry
           (Usr "Go")
           [Cond (Cons (Sym $ MDig 0) Empt) Nil (FTCons (Sym $ MDig 1) FEmpt)])
    , testCase "Not Entry fun" $
      parse (entry fDefine) "" "MyFun {'a' s.n = <Mul 1 s.n>; } " @?=
      Right
        (NEntry
           (Usr "MyFun")
           [ Cond
               (Cons (Sym $ Ch 'a') (Cons (Var $ SVar "n") Empt))
               Nil
               (FACons
                  (FApp
                     (Op $ Mul)
                     (FTCons (Sym $ MDig 1) (FTCons (Var $ SVar "n") FEmpt)))
                  (FEmpt))
           ])
    ]
