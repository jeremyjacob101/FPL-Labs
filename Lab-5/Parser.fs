module Parser

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
let expectToken kind value =
    let tokenKind, tokenValue = currentToken ()

    if matchToken kind value then
        advance ()

        Token(tokenKind, tokenValue)
    else
        failwithf "[%s] Expected %A '%A' but got %A '%A'" fileName kind value tokenKind tokenValue

let expect kind value =
    expectToken kind value |> ignore // match and advance but don't create a node
    Empty

// try to match the current token against the given kind and value, and if it matches, advance and return an optional Node list containing it followed by the result of getRest; otherwise return None
// useful for parsing optional tokens like commas in a list, where we want to return the parsed item if it exists, but also continue parsing the rest of the list, or elements which can appear multiple times, like variable declarations, where we want to return the parsed item if it exists, but also continue parsing the next declaration
let tryMatchToken (keep: bool) (kind: TokenType) (value: string list) getRest : Node list option =
    if matchToken kind value then
        let token = expectToken kind value
        Some([ if keep then token else Empty ] @ getRest ())
    else
        None

// curried function to create a Node with a list of children
let makeNode (kind: string) (children: Node list) : Node =
    let nonEmptyChildren = children |> List.except [ Empty ]
    Node(kind, nonEmptyChildren)

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

let parseIdentifier () = expectToken Identifier []

//#region expression parsing functions

let parseOp () =
    expectToken Symbol [ "+"; "-"; "*"; "/"; "&"; "|"; "<"; ">"; "=" ]

let parseUnaryOp () = expectToken Symbol [ "-"; "~" ]

let parseKeywordConstant () =
    expectToken Keyword [ "true"; "false"; "null"; "this" ]

// use `let rec` and `and` to allow mutually recursive parsing functions,
//which we will need for expressions since they can be nested and can also contain subroutine calls
// which can contain expressions as arguments, while still allowing access to the functions in the outer scope, like parseSubroutineCall.
let rec parseOptionalExpression () : Node option =
    // term (op term)*
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
    // integerConstant | stringConstant | keywordConstant | varName | varName '[' expression ']' | subroutineCall | '(' expression ')' | unaryOp term
    match currentToken () with
    | (IntConst, _) -> expectToken IntConst []
    | (StringConst, _) -> expectToken StringConst []
    | (Keyword, ("true" | "false" | "null" | "this")) -> parseKeywordConstant ()
    | (Symbol, "(") -> [ expect Symbol [ "(" ]; parseExpression (); expect Symbol [ ")" ] ][1] // return the expression node, not the parentheses
    | (Symbol, ("-" | "~")) -> [ parseUnaryOp (); parseTerm () ] |> makeNode "unaryOpTerm"
    | (Identifier, _) ->
        match peekToken 1 with
        | (Symbol, "[") ->
            [ parseIdentifier ()
              expect Symbol [ "[" ]
              parseExpression ()
              expect Symbol [ "]" ] ]
            |> makeNode "arrayAccess"
        | (Symbol, ("(" | ".")) -> parseSubroutineCall ()
        | _ -> parseIdentifier ()
    | _ -> failwithf "[%s] Expected term but got %A" fileName (currentToken ())
// |> makeNode "term"

and parseSubroutineCall () =
    // subroutineName '(' expressionList ')' | ( className | varName) '.' subroutineName '(' expressionList ')'
    [ [ parseIdentifier () ] // subroutine name or className|varName
      @ (tryMatchToken false Symbol [ "." ] (fun () -> [ parseIdentifier () ])
         |> Option.defaultValue []) // retroactively resolve first id as class or var name, and find the subroutine name
      |> makeNode "subroutineCallPrefix" ]
    @ [ expect Symbol [ "(" ]; parseExpressionList (); expect Symbol [ ")" ] ]
    |> makeNode "subroutineCall"

and parseExpressionList () =
    // (expression (',' expression)* )?
    match currentToken () with
    | (Symbol, ")") -> [] // empty expression list -- FOLLOW(expressionList) = {")"}
    | _ ->
        [ parseExpression () ]
        @ parseStarFlat (fun () -> tryMatchToken false Symbol [ "," ] (fun () -> [ parseExpression () ]))
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
        @ (tryMatchToken false Symbol [ "[" ] (fun () -> [ parseExpression (); expect Symbol [ "]" ] ])
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
        @ (tryMatchToken false Keyword [ "else" ] (fun () ->
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
        [ expect Keyword [ "do" ]; parseSubroutineCall (); expect Symbol [ ";" ] ]
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
    | (Keyword, _) -> expectToken Keyword [ "int"; "char"; "boolean" ]
    | (Identifier, _) -> parseIdentifier () // class name
    | _ -> failwithf "[%s] Expected type but got %A" fileName (currentToken ())

let parseClassVarDec () =
    // ('static' | 'field' ) type varName (',' varName)* ';'
    tryMatchToken true Keyword [ "static"; "field" ] (fun () ->
        [ parseType (); parseIdentifier () ] // var name
        @ parseStarFlat (fun () -> tryMatchToken false Symbol [ "," ] (fun () -> [ parseIdentifier () ]))
        @ [ expect Symbol [ ";" ] ])
    |> Option.map (makeNode "classVarDec")

let parseParameterList () =
    // ( (type varName) (',' type varName)*)?
    let parseIndividualParam () =
        [ parseType (); parseIdentifier () ] |> makeNode "parameter"

    match currentToken () with
    | (Symbol, ")") -> []
    | _ ->
        [ parseIndividualParam () ]
        @ parseStarFlat (fun () -> tryMatchToken false Symbol [ "," ] (fun () -> [ parseIndividualParam () ]))
    |> makeNode "parameterList"

let parseVarDec () =
    // 'var' type varName (',' varName)* ';
    tryMatchToken false Keyword [ "var" ] (fun () ->
        [ parseType (); parseIdentifier () ] // var name
        @ parseStarFlat (fun () -> tryMatchToken false Symbol [ "," ] (fun () -> [ parseIdentifier () ]))
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
    tryMatchToken true Keyword [ "constructor"; "function"; "method" ] (fun () ->
        [ (if matchToken Keyword [ "void" ] then
               expectToken Keyword [ "void" ]
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
