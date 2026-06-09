module Parser

open Tokenizer
open Definitions

//#region Parser state and utilities

let mutable tokens: Token list = []
let mutable index = 0
let currentToken () = tokens[index]
let advance () = index <- index + 1

let matchToken (kind: TokenType) (value: string list) : bool =
    let (tokenKind, tokenValue) = currentToken ()

    tokenKind = kind
    && match value with
       | [] -> true
       | _ -> List.contains tokenValue value

let expect (kind: TokenType) (value: string list) : Node =
    let (tokenKind, tokenValue) = currentToken ()

    if matchToken kind value then
        advance ()

        Token(tokenKind, tokenValue)
    else
        failwithf "Expected %A '%A' but got %A '%A'" kind value tokenKind tokenValue

// curried function to create a Node with a list of children
let makeNode (kind: string) (children: Node list) : Node = Node(kind, children)

//#endregion

// take a function that parses a single option node and repeatedly apply it until it fails, collecting the results in a list
let parseStarFlat fn =
    let rec loop acc =
        match fn () with
        | Some node -> loop (acc @ node)
        | None -> acc

    loop []

let parseStar fn =
    parseStarFlat (fun () ->
        match fn () with
        | Some node -> Some [ node ]
        | None -> None)

let parseIdentifier () = expect Identifier []

let parseType () =
    match currentToken () with
    | (Keyword, _) -> expect Keyword [ "int"; "char"; "boolean" ]
    | (Identifier, _) -> parseIdentifier ()
    | _ -> failwithf "Expected type but got %A" (currentToken ())

let parseClassVarDec () =
    if matchToken Keyword [ "static"; "field" ] then
        Some(
            [ expect Keyword [ "static"; "field" ]; parseType (); parseIdentifier () ]
            @ parseStarFlat (fun () ->
                if matchToken Symbol [ "," ] then
                    Some [ expect Symbol [ "," ]; parseIdentifier () ]
                else
                    None)
            @ [ expect Symbol [ ";" ] ]
            |> makeNode "classVarDec"
        )
    else
        None

let parseClass () =
    [ expect Keyword [ "class" ]; parseIdentifier (); expect Symbol [ "{" ] ]
    @ parseStar parseClassVarDec
    // @ [ expect Symbol [ "}" ] ]
    |> makeNode "class"


let parseProgram tokenList =
    tokens <- tokenList
    index <- 0
    parseClass ()
