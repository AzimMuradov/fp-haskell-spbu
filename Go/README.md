## An implementation of Go mini-language

This is a homework for functional programming course.

**License**: APACHE LICENSE, VERSION 2.0

**Author**: Azim Muradov, azim.muradov.dev@gmail.com


### Features:

Project parts:

|     To do      |            In progress            |     Done     |
| :------------: | :-------------------------------: | :----------: |
| pretty printer | grammar definition in Pest syntax |     CLI      |
|                |            interpreter            |    lexer     |
|                |               docs                |     AST      |
|                |                                   |    parser    |
|                |                                   |   analyzer   |
|                |                                   | analyzed AST |


#### Done:

- `int`, `bool`, `string` support
- function definitions
- variable declarations
- `void` support
- `if`, `else`
- recursion, mutual recursion
- operators (...)
- parser, lexer
- analyzer (check for name collision or missing names, type checker, const expressions converters (w/o `const` keyword))
- various assignments and short variable declaration
- increment, decrement
- several stdlib functions (`len`, `panic`, `print`, `println`)
- globals
- arrays, loops
- function literals

#### Missing:

- closures
- interpreter
