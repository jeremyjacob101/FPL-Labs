module Definitions

type TokenType =
    | Keyword
    | Symbol
    | Identifier
    | IntConst
    | StringConst

[<Literal>]
let MaxIntConstant = 32767

let keywordStrings =
    [ "class"
      "constructor"
      "function"
      "method"
      "field"
      "static"
      "var"
      "int"
      "char"
      "boolean"
      "void"
      "true"
      "false"
      "null"
      "this"
      "let"
      "do"
      "if"
      "else"
      "while"
      "return" ]

let symbolChars =
    [ '{'
      '}'
      '('
      ')'
      '['
      ']'
      '.'
      ','
      ';'
      '+'
      '-'
      '*'
      '/'
      '&'
      '|'
      '<'
      '>'
      '='
      '~' ]
