module Parser

open Tokenizer
open Definitions

//#region Parser state and utilities

let mutable fileName = ""
let mutable tokens: Token list = []
let mutable index = 0
let currentToken () = tokens[index]
let advance () = index <- index + 1

//lookahead for identifiers to distinguish between varName, varName[expression], and subroutineCall, which all start with an identifier.
let peekToken offset = tokens[index + offset]

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

//#region expression parsing functions

let parseOp () =
    expect Symbol [ "+"; "-"; "*"; "/"; "&"; "|"; "<"; ">"; "=" ]

let parseUnaryOp () = expect Symbol [ "-"; "~" ]

let parseKeywordConstant () =
    expect Keyword [ "true"; "false"; "null"; "this" ]

// use `let rec` and `and` to allow mutually recursive parsing functions,
//which we will need for expressions since they can be nested and can also contain subroutine calls
// which can contain expressions as arguments, while still allowing access to the functions in the outer scope, like parseSubroutineCall.
let rec parseOptionalExpression () : Node option =
    match currentToken () with
    | (IntConst, _)
    | (StringConst, _)
    | (Keyword, ("true" | "false" | "null" | "this"))
    | (Identifier, _)
    | (Symbol, ("(" | "-" | "~")) ->
        [ parseTerm () ]
        @ parseStarFlat (fun () ->
            if matchToken Symbol [ "+"; "-"; "*"; "/"; "&"; "|"; "<"; ">"; "=" ] then
                Some [ parseOp (); parseTerm () ]
            else
                None)
        |> makeNode "expression"
        |> Some
    | _ -> None

and parseExpression () =
    match parseOptionalExpression () with
    | Some x -> x
    | None -> failwithf "[%s] Expected expression but got %A" fileName (currentToken ())

and parseTerm () =
    match currentToken () with
    | (IntConst, _) -> [ expect IntConst [] ]
    | (StringConst, _) -> [ expect StringConst [] ]
    | (Keyword, ("true" | "false" | "null" | "this")) -> [ parseKeywordConstant () ]
    | (Symbol, "(") -> [ expect Symbol [ "(" ]; parseExpression (); expect Symbol [ ")" ] ]
    | (Symbol, ("-" | "~")) -> [ parseUnaryOp (); parseTerm () ]
    | (Identifier, _) ->
        match peekToken 1 with
        | (Symbol, "[") ->
            [ parseIdentifier ()
              expect Symbol [ "[" ]
              parseExpression ()
              expect Symbol [ "]" ] ]
        | (Symbol, ("(" | ".")) -> parseSubroutineCall ()
        | _ -> [ parseIdentifier () ]
    | _ -> failwithf "[%s] Expected term but got %A" fileName (currentToken ())
    |> makeNode "term"

and parseSubroutineCall () =
    [ parseIdentifier () ] // subroutine name or className|varName
    @ (tryMatchToken Symbol [ "." ] (fun () -> [ parseIdentifier () ])
       |> Option.defaultValue []) // retroactively resolve first id as class or var name, and find the subroutine name
    @ [ expect Symbol [ "(" ]; parseExpressionList (); expect Symbol [ ")" ] ]

and parseExpressionList () =
    match currentToken () with
    | (Symbol, ")") -> [] // empty expression list -- AFTER(expressionList) = {")"}
    | _ ->
        [ parseExpression () ]
        @ parseStarFlat (fun () -> tryMatchToken Symbol [ "," ] (fun () -> [ parseExpression () ]))
    |> makeNode "expressionList"

//#endregion

//#region statement parsing functions


let rec parseStatements () =
    // statement*

    // define internal statement parsing so we can recursively call parseStatements() for nested statements in if and while

    let parseLetStatement () =
        // 'let' varName ('[' expression ']')? '=' expression ';'
        [ expect Keyword [ "let" ]; parseIdentifier () ] // var name
        // optional array access
        @ (tryMatchToken Symbol [ "[" ] (fun () -> [ parseExpression (); expect Symbol [ "]" ] ])
           |> Option.defaultValue [])
        @ [ expect Symbol [ "=" ]; parseExpression (); expect Symbol [ ";" ] ]
        |> makeNode "letStatement"

    let parseIfStatement () =
        // 'if' '(' expression ')' '{' statements '}' ( 'else' '{' statements '}' )?
        [ expect Keyword [ "if" ]
          expect Symbol [ "(" ]
          parseExpression ()
          expect Symbol [ ")" ]
          expect Symbol [ "{" ]
          parseStatements ()
          expect Symbol [ "}" ] ]
        // optional else clause
        @ (tryMatchToken Keyword [ "else" ] (fun () ->
            [ expect Symbol [ "{" ]; parseStatements (); expect Symbol [ "}" ] ])
           |> Option.defaultValue [])
        |> makeNode "ifStatement"

    let parseWhileStatement () =
        // 'while' '(' expression ')' '{' statements '}'
        [ expect Keyword [ "while" ]
          expect Symbol [ "(" ]
          parseExpression ()
          expect Symbol [ ")" ]
          expect Symbol [ "{" ]
          parseStatements ()
          expect Symbol [ "}" ] ]
        |> makeNode "whileStatement"

    let parseDoStatement () =
        // 'do' subroutineCall ';'
        [ expect Keyword [ "do" ] ] @ parseSubroutineCall () @ [ expect Symbol [ ";" ] ]
        |> makeNode "doStatement"

    let parseReturnStatement () =
        // 'return' expression? ';'
        [ expect Keyword [ "return" ] ]
        @ (parseOptionalExpression () |> Option.toList)
        @ [ expect Symbol [ ";" ] ]
        |> makeNode "returnStatement"

    let parseStatement () =
        // letStatement | ifStatement | whileStatement | doStatement | returnStatement
        match currentToken () with
        | (Keyword, "let") -> Some(parseLetStatement ())
        | (Keyword, "if") -> Some(parseIfStatement ())
        | (Keyword, "while") -> Some(parseWhileStatement ())
        | (Keyword, "do") -> Some(parseDoStatement ())
        | (Keyword, "return") -> Some(parseReturnStatement ())
        | _ -> None

    // statements: statement*
    parseStar parseStatement |> makeNode "statements"
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
    | (Symbol, ")") -> []
    | _ ->
        parseIndividualParam ()
        @ parseStarFlat (fun () -> tryMatchToken Symbol [ "," ] parseIndividualParam)
    |> makeNode "parameterList"

let parseVarDec () =
    // 'var' type varName (',' varName)* ';
    tryMatchToken Keyword [ "var" ] (fun () ->
        [ parseType (); parseIdentifier () ] // var name
        @ parseStarFlat (fun () -> tryMatchToken Symbol [ "," ] (fun () -> [ parseIdentifier () ]))
        @ [ expect Symbol [ ";" ] ])
    |> Option.map (makeNode "varDec")

let parseSubroutineBody () =
    // '{' varDec* statements '}'
    [ expect Symbol [ "{" ] ]
    @ parseStar parseVarDec
    @ [ parseStatements (); expect Symbol [ "}" ] ]
    |> makeNode "subroutineBody"

let parseSubroutineDec () =
    // ('constructor' | 'function' | 'method') ('void' | type) subroutineName '('parameterList ')' subroutineBody
    tryMatchToken Keyword [ "constructor"; "function"; "method" ] (fun () ->
        [ (if matchToken Keyword [ "void" ] then
               expect Keyword [ "void" ]
           else
               parseType ())
          parseIdentifier () // subroutine name
          expect Symbol [ "(" ]
          parseParameterList ()
          expect Symbol [ ")" ]
          parseSubroutineBody () ])
    |> Option.map (makeNode "subroutineDec")

let parseClass () =
    // 'class' className '{' classVarDec* subroutineDec* '}'
    [ expect Keyword [ "class" ]; parseIdentifier (); expect Symbol [ "{" ] ]
    @ parseStar parseClassVarDec
    @ parseStar parseSubroutineDec
    @ [ expect Symbol [ "}" ] ]
    |> makeNode "class"

//#endregion

//#endregion


let parseProgram className tokenList =
    fileName <- className
    tokens <- tokenList
    index <- 0
    parseClass ()
