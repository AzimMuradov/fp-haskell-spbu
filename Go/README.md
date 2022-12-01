## An implementation of Go mini-language

This is a homework for functional programming course.

**License**: APACHE LICENSE, VERSION 2.0

**Author**: Azim Muradov, azim.muradov.dev@gmail.com


### Features:

Project parts:

|     To do      |            In progress            |    Done     |
| :------------: | :-------------------------------: | :---------: |
| pretty printer | grammar definition in Pest syntax |     CLI     |
|                |         program analyzer          |    lexer    |
|                |            interpreter            |     AST     |
|                |               docs                | POC mini go |
|                |           analyzed AST            |   parser    |


#### Done:

- `int`, `bool`, `string` support
- function definitions
- variable declarations
- `printlnInt`, `printlnBool`, `printlnStr`
- `void` support
- `if`
- recursion, mutual recursion
- some of the operators
- lexer
- most of the parser
- check for name collision or missing names, part of the type checker
- part of the interpreter
- Const expressions converters (w/o `const` keyword)

#### Missing:

- function literals
- arrays, loops
- closures
- globals
- several stdlib functions (`len`, `panic`, ...)
- a lot of operators
- `else`
- various assignments and short variable declaration
- increment, decrement
