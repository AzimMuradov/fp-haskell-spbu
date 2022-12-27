## An implementation of Go mini-language

This is a homework for functional programming course.

**License**: APACHE LICENSE, VERSION 2.0

**Author**: Azim Muradov, azim.muradov.dev@gmail.com


### Features:

Project parts:

|     To do      | In progress |     Done     |
| :------------: | :---------: | :----------: |
| pretty printer | interpreter |     CLI      |
|   unit tests   |    docs     |     AST      |
|                |             |    lexer     |
|                |             |    parser    |
|                |             | analyzed AST |
|                |             |   analyzer   |


#### Done:

- `int`, `bool`, `string` support
- `void` support
- function literals (anonymous functions)
- operators (arithmetic, logic, comparison)
- `if`, `else`
- recursion, mutual recursion
- function definitions
- variable declarations
- variable assignments
- short variable declarations
- increment, decrement
- parser, lexer
- analyzer (check for name collision or missing names, type checker, const expressions converters (w/o `const` keyword))
- interpreter (everything except mentioned in missing)
- several stdlib functions (`len`, `print`, `println`, `panic`)

#### Missing:

- interpreter (arrays, loops, globals, closures)
