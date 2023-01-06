### Refal 5 interpreter

Done:

- AST of all possible constructions (Exept  $EXTRN)
- Parser supports everything that is described in the AST
- Abstract Refal Mashine
- Matching by S-vars
- Add, Sub and Mul
- Tests on Factorial and Power.

TO DO:
- Matching by S-vars
- $EXTRN - importing modules
- Much more tests and nice Ci
- Exceprions
- 'Condition' expression interpretetion
- Macrodigits and sugar for many chars : 'a' 'b' ... ~ 'ab...'

### Setup

1. Clone (`https://github.com/SuldinVyacheslav/Refal.git`)
2. Update (`cabal update`)
3. Build (`cabal build`)
4. Run (`cabal v2-run Refal X`)

X = test/factorial.ref or test/power.ref
