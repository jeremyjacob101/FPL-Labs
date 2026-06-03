module Tokenizer

open System
open Definitions

type Token = { Kind: TokenType; Value: string }

let isWhitespace = false
let isLineComment = false
let isBlockComment = false

// Jack Lexicon
let isSymbol (c: char) = List.contains c symbolChars
let isDigit (c: char) = Char.IsDigit c
let isIdentifierStart (c: char) = Char.IsLetter c || c = '_'
let isIdentifierPart (c: char) = Char.IsLetterOrDigit c || c = '_'

let skipWhitespaceAndComments (text: string) (startIndex: int) =
    let mutable i = startIndex

    while i < text.Length do
        if isWhitespace then
            printfn "iterate"

        elif isLineComment then
            printfn "iterate"

        elif isBlockComment then
            printfn "iterate"

    i

let tokenize (text: string) : Token list =
    let mutable i = 0
    let tokens = ResizeArray<Token>()

    while i < text.Length do
        i <- skipWhitespaceAndComments text i

        if i < text.Length then
            let c = text[i]

            if isSymbol c then // Symbol
                tokens.Add({ Kind = Symbol; Value = string c })

            elif isDigit c then // Digit
                let startIndex = i

                while i < text.Length && isDigit text[i] do
                    i <- i + 1

                let value = text.Substring(startIndex, i - startIndex)

                if int value > MaxIntConstant then
                    failwith "integerConst too large"

                tokens.Add({ Kind = Keyword; Value = value })

            elif c = '"' then // String
                tokens.Add({ Kind = Keyword; Value = "" })

            elif isIdentifierStart c then // Identifier/Keyword
                tokens.Add({ Kind = Keyword; Value = "" })

    List.ofSeq tokens
