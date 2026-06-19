module CodeGenerator

open System.IO
open Definitions

let mutable writer: StreamWriter option = None

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
    | Node("expression", _) -> writeExpression node
    | _ -> writeData (sprintf "// unhandled node %A" node)

and writeOp (node: Node) =
    match node with
    | Token(Symbol, "+") -> writeArithmetic ADD
    | Token(Symbol, "-") -> writeArithmetic SUB
    | Token(Symbol, "*") -> writeCall "Math.multiply" 2
    | Token(Symbol, "/") -> writeCall "Math.divide" 2
    | _ -> failwithf "Unknown operator %A" node


let writeDoStatement (node: Node) =
    // node should be a subroutineCall node
    let nodes = getChildren "subroutineCall" node

    let functionName =
        getChildren "subroutineCallPrefix" nodes[0]
        |> List.map (getTokenValue Identifier)
        |> String.concat "."

    let expressions = getChildren "expressionList" nodes[1]

    expressions |> List.iter writeExpression

    writeCall functionName expressions.Length
    // do statements discard the return value, so we pop it off the stack
    writePop TEMP 0

let writeReturnStatement (nodes: Node list) =
    // TODO: handle return expressions
    writePush CONST 0
    writeReturn ()

let writeStatement node =
    match node with
    | Node("letStatement", children) -> writeData "letStatement"
    | Node("ifStatement", children) -> writeData "ifStatement"
    | Node("whileStatement", children) -> writeData "whileStatement"
    | Node("doStatement", children) -> writeDoStatement children[0]
    | Node("returnStatement", children) -> writeReturnStatement children
    | x -> failwithf "Unknown statement %A" x

let writeSubroutine className node =
    let children = getChildren "subroutineDec" node
    let subroutineType = getTokenValue Keyword children[0]
    let returnType = getTypeName children[1]
    let subroutineName = className + "." + getTokenValue Identifier children[2]

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

    writeFunction subroutineName paramLength

    let subroutineBodyChildren = getChildren "subroutineBody" children[4]

    let varDecNodes = List.filter (matchNode "varDec") subroutineBodyChildren

    let statementsNode =
        List.tryFind (matchNode "statements") subroutineBodyChildren
        |> Option.defaultValue (Node("statements", []))

    let statements = getChildren "statements" statementsNode

    statements |> List.iter writeStatement


let writeClass node =
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
