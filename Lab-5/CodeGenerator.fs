module CodeGenerator

open System.IO
open Definitions
open SymbolTable

let mutable writer: StreamWriter option = None

let mutable labelCounter = 0
let mutable currentFunctionName = "" // fully resolved function name eg Main.main

let nextLabel prefix =
    labelCounter <- labelCounter + 1
    currentFunctionName + "$" + prefix + string labelCounter

let writeData (text: string) =
    // indent all lines except for comments and function declarations for better readability of the generated VM code
    let formattedText =
        if text.StartsWith("//") || text.StartsWith("function") then
            text + "\n"
        else
            "\t" + text + "\n"

    Option.get(writer).Write formattedText

//#region VM command abstractions

let writePush (segment: Segment) (index: int) =
    writeData ([ "push"; segment.ToString().ToLower(); index.ToString() ] |> String.concat " ")

let writePop (segment: Segment) (index: int) =
    writeData ([ "pop"; segment.ToString().ToLower(); index.ToString() ] |> String.concat " ")

let writeArithmetic (command: ArithmeticCommand) =
    writeData (command.ToString().ToLower())

let writeLabel (label) = writeData ("label " + label)
let writeGoTo label = writeData ("goto " + label)
let writeIf label = writeData ("if-goto " + label)

let writeCall (functionName: string) (numArgs: int) =
    writeData ([ "call"; functionName; numArgs.ToString() ] |> String.concat " ")

let writeFunction (functionName: string) (numLocals: int) =
    writeData ([ "function"; functionName; numLocals.ToString() ] |> String.concat " ")

let writeReturn () = writeData "return"

//#endregion

let segmentOfSymbolKind kind =
    match kind with
    | Static -> STATIC
    | Field -> THIS
    | Argument -> ARG
    | Var -> LOCAL

let matchNode (expectedKind: string) (node: Node) =
    match node with
    | Node(kind, _) when kind = expectedKind -> true
    | _ -> false

let getChildren (expectedKind: string) (node: Node) =
    match node with
    | Node(kind, children) when kind = expectedKind -> children
    | _ -> failwithf "Expected node of kind %A, received %A" expectedKind node

let getTokenValue (expectedKind: TokenType) (node: Node) =
    match node with
    | Token(kind, value) when kind = expectedKind -> value
    | _ -> failwithf "Expected token of kind %A, received %A" expectedKind node

let getTypeName node =
    // TODO: integrate with symbol table
    match node with
    | Token(Identifier, value) -> value
    | Token(Keyword, value) ->
        if List.contains value [ "void"; "int"; "char"; "boolean" ] then
            value
        else
            ""
    | _ -> ""

//#region recursively travserse the parse tree and generate code



let rec writeExpression (node: Node) =
    let children = getChildren "expression" node
    writeTerm children[0]

    if children.Length > 1 then
        writeTerm children[2]
        writeOp children[1]

and writeTerm (node: Node) =
    match node with
    | Token(IntConst, value) -> writePush CONST (int value)
    | Token(Identifier, id) -> writeData ("// push " + id) // TODO: fill in
    | Token(Keyword, "true") ->
        writePush CONST 1
        writeArithmetic NEG // true is represented as -1 in Hack
    | Token(Keyword, "false") -> writePush CONST 0
    | Token(Keyword, "null") -> writePush CONST 0
    | Node("unaryOpTerm", children) ->
        writeTerm children[1]

        match children[0] with
        | Token(Symbol, "-") -> writeArithmetic NEG
        | Token(Symbol, "~") -> writeArithmetic NOT
        | _ -> failwithf "Unknown unary operator %A" children[0]
    | Node("subroutineCall", children) ->
        let functionName =
            getChildren "subroutineCallPrefix" children[0]
            |> List.map (getTokenValue Identifier)
            |> String.concat "."

        let expressions = getChildren "expressionList" children[1]
        expressions |> List.iter writeExpression
        writeCall functionName expressions.Length
    | Node("expression", _) -> writeExpression node
    | _ -> writeData (sprintf "// unhandled node %A" node)

and writeOp (node: Node) =
    match node with
    | Token(Symbol, "+") -> writeArithmetic ADD
    | Token(Symbol, "-") -> writeArithmetic SUB
    | Token(Symbol, "*") -> writeCall "Math.multiply" 2
    | Token(Symbol, "/") -> writeCall "Math.divide" 2
    | Token(Symbol, "=") -> writeArithmetic EQ
    | Token(Symbol, ">") -> writeArithmetic GT
    | Token(Symbol, "<") -> writeArithmetic LT
    | Token(Symbol, "&") -> writeArithmetic AND
    | Token(Symbol, "|") -> writeArithmetic OR
    | _ -> failwithf "Unknown operator %A" node


let rec writeStatement node =

    let writeLetStatement (nodes: Node list) =
        let varSegment, varIndex = (LOCAL, 0) // TODO: use symbol table
        writeExpression nodes[1]
        writePop varSegment varIndex

    let writeIfStatement (nodes: Node list) =
        let afterLabel = nextLabel "IF_AFTER_"
        let elseLabel = if nodes.Length = 3 then nextLabel "ELSE_" else ""
        let falseLabel = if elseLabel = "" then afterLabel else elseLabel

        writeExpression nodes[0] // condition
        writeArithmetic NOT // invert the condition - no short circuiting and true/false values to track
        writeIf falseLabel // if false, jump to else/after
        getChildren "statements" nodes[1] |> List.iter writeStatement // true statements

        if elseLabel <> "" then // if there is an else
            writeGoTo afterLabel // jump to `next` after finishing the if
            writeLabel elseLabel
            getChildren "statements" nodes[2] |> List.iter writeStatement // else statements

        writeLabel afterLabel

    let writeWhileStatement (nodes: Node list) =
        let loopLabel = nextLabel "LOOP_"
        let condLabel = nextLabel "DO_COND_"

        // write the `while` more like a `do-while` to avoid inverting the condition
        writeGoTo condLabel // test condition first
        writeLabel loopLabel
        getChildren "statements" nodes[1] |> List.iter writeStatement // loop statements
        writeLabel condLabel
        writeExpression nodes[0]
        writeIf loopLabel // jump back to top if true, fall if false

    let writeDoStatement (node: Node) =
        writeTerm node // subroutine call is also a term of an expression
        // do statements discard the return value, so we pop it off the stack
        writePop TEMP 0

    let writeReturnStatement (nodes: Node list) =
        if nodes.Length = 0 then
            writePush CONST 0 // return void
        else
            writeExpression nodes[0] // return expression

        writeReturn ()

    // write statement
    match node with
    | Node("letStatement", children) -> writeLetStatement children
    | Node("ifStatement", children) -> writeIfStatement children
    | Node("whileStatement", children) -> writeWhileStatement children
    | Node("doStatement", children) -> writeDoStatement children[0]
    | Node("returnStatement", children) -> writeReturnStatement children
    | x -> failwithf "Unknown statement %A" x

let writeSubroutine className node =
    resetSubScope ()

    let children = getChildren "subroutineDec" node
    let subroutineType = getTokenValue Keyword children[0]

    if subroutineType = "method" then
        addToSymbolTable "this" className Argument

    let returnType = getTypeName children[1]
    let subroutineName = className + "." + getTokenValue Identifier children[2]
    currentFunctionName <- subroutineName

    let parameterList = children[3]

    let userParamLength = getChildren "parameterList" parameterList |> List.length
    // methods also take a `this` param
    let paramLength = userParamLength + (if subroutineType = "method" then 1 else 0)

    writeData (
        [ "//"
          subroutineType
          subroutineName
          paramLength.ToString()
          "->"
          returnType ]
        |> String.concat " "
    )

    let subroutineBodyChildren = getChildren "subroutineBody" children[4]

    let varDecNodes = List.filter (matchNode "varDec") subroutineBodyChildren

    // TODO: Maybe use symbol table for this? But this also works
    let varDecCount =
        varDecNodes
        |> List.map (getChildren "varDec")
        |> List.map List.tail
        |> List.map List.length
        |> List.sum

    writeFunction subroutineName varDecCount

    let statementsNode =
        List.tryFind (matchNode "statements") subroutineBodyChildren
        |> Option.defaultValue (Node("statements", []))

    let statements = getChildren "statements" statementsNode

    statements |> List.iter writeStatement

    writeData "\n"


let writeClass node =
    resetScopes ()

    let children = getChildren "class" node

    let className = getTokenValue Identifier (children[0])

    writeData ("// class " + className + "\n")

    // TODO: handle fields

    // handle subroutines
    children
    |> List.filter (matchNode "subroutineDec")
    |> List.iter (writeSubroutine className)

//#endregion

let writeProgram (f: FileStream) (rootNode: Node) =
    use _writer = new StreamWriter(f)
    writer <- Some(_writer)
    writeClass rootNode
