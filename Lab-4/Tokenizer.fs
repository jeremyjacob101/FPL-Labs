module Tokenizer

open System
open Definitions

type Token = { Kind: TokenType; Value: string }

// Jack Lexicon
let isSymbol (c: char) = List.contains c symbolChars
let isDigit (c: char) = Char.IsDigit c
let isIdentifierStart (c: char) = Char.IsLetter c || c = '_'
let isIdentifierPart (c: char) = Char.IsLetterOrDigit c || c = '_'

let skipWhitespaceAndComments (text: string) (startIndex: int) =
    let mutable i = startIndex
    let mutable skipping = true

    while i < text.Length && skipping do
        if Char.IsWhiteSpace text[i] then // Whitespace
            i <- i + 1

        elif text[i] = '/' && i + 1 < text.Length && text[i + 1] = '/' then // Line Comment
            i <- i + 2
            while i < text.Length && text[i] <> '\n' do
                i <- i + 1

        elif text[i] = '/' && i + 1 < text.Length && text[i + 1] = '*' then // Block Comment
            i <- i + 2
            while i + 1 < text.Length && not (text[i] = '*' && text[i + 1] = '/') do
                i <- i + 1
            i <- i + 2

        else
            skipping <- false

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
                i <- i + 1

            elif isDigit c then // Digit
                let startIndex = i

                while i < text.Length && isDigit text[i] do
                    i <- i + 1

                let value = text.Substring(startIndex, i - startIndex)
                if int value > MaxIntConstant then
                    failwith "integerConst too large"
                tokens.Add({ Kind = IntConst; Value = value })

            elif c = '"' then // String
                let startIndex = i
                
                i <- i + 1
                while text[i] <> '"' do
                    i <- i + 1
                i <- i + 1

                let value = text.Substring(startIndex + 1, i - startIndex - 2)
                tokens.Add({ Kind = StringConst; Value = value })

            elif isIdentifierStart c then // Identifier/Keyword
                let startIndex = i

                while i < text.Length && isIdentifierPart text[i] do
                    i <- i + 1
                
                let value = text.Substring(startIndex, i - startIndex)

                let mutable kind = Identifier
                
                if List.contains value keywordStrings then
                    kind <- Keyword

                tokens.Add({ Kind = kind; Value = value })

    List.ofSeq tokens
