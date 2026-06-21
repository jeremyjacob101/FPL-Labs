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

// Declare Token as its own type, but also as a case of Node - it seems to work

type Token = TokenType * string

type Node =
    | Token of TokenType * string
    | Node of string * Node list
    | Empty


type Segment =
    | CONST
    | ARG
    | LOCAL
    | STATIC
    | THIS
    | THAT
    | POINTER
    | TEMP

type ArithmeticCommand =
    | ADD
    | SUB
    | NEG
    | EQ
    | GT
    | LT
    | AND
    | OR
    | NOT
