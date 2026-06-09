module Parser

open Tokenizer
open Definitions

//#region Parser state and utilities

let mutable fileName = ""
let mutable tokens: Token list = []
let mutable index = 0
let currentToken () = tokens[index]
let advance () = index <- index + 1

// check if the current token matches the expected kind and value (if provided), without advancing
let matchToken (kind: TokenType) (value: string list) : bool =
    let tokenKind, tokenValue = currentToken ()

    tokenKind = kind
    && match value with
       | [] -> true
       | _ -> List.contains tokenValue value

// expect the current token to match the given kind and value, advance, and return it as a Node; otherwise throw an error
let expect (kind: TokenType) (value: string list) : Node =
    let tokenKind, tokenValue = currentToken ()

    if matchToken kind value then
        advance ()

        Token(tokenKind, tokenValue)
    else
        failwithf "[%s] Expected %A '%A' but got %A '%A'" fileName kind value tokenKind tokenValue

// try to match the current token against the given kind and value, and if it matches, advance and return an optional Node list containing it followed by the result of getRest; otherwise return None
// useful for parsing optional tokens like commas in a list, where we want to return the parsed item if it exists, but also continue parsing the rest of the list, or elements which can appear multiple times, like variable declarations, where we want to return the parsed item if it exists, but also continue parsing the next declaration
let tryMatchToken (kind: TokenType) (value: string list) getRest : Node list option =
    if matchToken kind value then
        Some([ expect kind value ] @ getRest ())
    else
        None

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

//#region Parsing functions

let parseIdentifier () = expect Identifier []

//#region statement parsing functions

let parseStatements () = [] // TODO: fill in
//#endregion

//#region program structure parsing functions

let parseType () =
    // 'int' | 'char' | 'boolean' | className
    match currentToken () with
    | (Keyword, _) -> expect Keyword [ "int"; "char"; "boolean" ]
    | (Identifier, _) -> parseIdentifier () // class name
    | _ -> failwithf "[%s] Expected type but got %A" fileName (currentToken ())

let parseClassVarDec () =
    // ('static' | 'field' ) type varName (',' varName)* ';'
    tryMatchToken Keyword [ "static"; "field" ] (fun () ->
        [ parseType (); parseIdentifier () ] // var name
        @ parseStarFlat (fun () -> tryMatchToken Symbol [ "," ] (fun () -> [ parseIdentifier () ]))
        @ [ expect Symbol [ ";" ] ])
    |> Option.map (makeNode "classVarDec")

let parseParameterList () =
    // ( (type varName) (',' type varName)*)?
    let parseIndividualParam () = [ parseType (); parseIdentifier () ]

    match currentToken () with
    | (Symbol, ")") -> None
    | _ ->
        Some(
            parseIndividualParam ()
            @ parseStarFlat (fun () -> tryMatchToken Symbol [ "," ] parseIndividualParam)
            |> makeNode "parameterList"
        )

let parseVarDec () =
    // 'var' type varName (',' varName)* ';
    tryMatchToken Keyword [ "var" ] (fun () ->
        [ parseType (); parseIdentifier () ] // var name
        @ parseStarFlat (fun () -> tryMatchToken Symbol [ "," ] (fun () -> [ parseIdentifier () ]))
        @ [ expect Symbol [ ";" ] ])
    |> Option.map (makeNode "varDec")

let parseSubroutineBody () =
    // '{' varDec* statements '}'
    [ expect Symbol [ "{" ] ] @ parseStar parseVarDec @ parseStatements ()
    // @ [ expect Symbol [ "}" ] ] // TODO: enable after implementing statements parsing
    |> makeNode "subroutineBody"

let parseSubroutineDec () =
    // ('constructor' | 'function' | 'method') ('void' | type) subroutineName '('parameterList ')' subroutineBody
    tryMatchToken Keyword [ "constructor"; "function"; "method" ] (fun () ->
        [ (if matchToken Keyword [ "void" ] then
               expect Keyword [ "void" ]
           else
               parseType ())
          parseIdentifier () // subroutine name
          expect Symbol [ "(" ] ]
        @ (parseParameterList () |> Option.toList)
        @ [ expect Symbol [ ")" ]; parseSubroutineBody () ])
    |> Option.map (makeNode "subroutineDec")

let parseClass () =
    // 'class' className '{' classVarDec* subroutineDec* '}'
    [ expect Keyword [ "class" ]; parseIdentifier (); expect Symbol [ "{" ] ]
    @ parseStar parseClassVarDec
    @ parseStar parseSubroutineDec
    // @ [ expect Symbol [ "}" ] ]
    |> makeNode "class"

//#endregion

//#endregion


let parseProgram className tokenList =
    fileName <- className
    tokens <- tokenList
    index <- 0
    parseClass ()
