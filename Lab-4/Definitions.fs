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

type Token = { Kind: TokenType; Value: string }

type Node =
    { Kind: string
      Children: Node list | null
      Token: Token option }
