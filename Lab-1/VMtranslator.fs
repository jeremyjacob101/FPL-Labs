module VMtranslator

open System
open System.IO

let mutable currentFileName = ""

type Command =
    | Arithmetic of string
    | Push of string * int
    | Pop of string * int

let stripComments (line: string) =
    let parts = line.Split("//")
    parts[0].Trim()

// Reusable assembly code snippets
let popToD = [ "@SP"; "AM=M-1"; "D=M" ]
let pushFromD = [ "@SP"; "M=M+1"; "A=M-1"; "M=D" ]
let pointToTop = [ "@SP"; "A=M-1" ]
let pointToPreviousA = [ "A=A-1" ]

let writeArithmetic operation =
    match operation with
    | "add" -> popToD @ pointToPreviousA @ [ "M=D+M" ]
    | "sub" -> popToD @ pointToPreviousA @ [ "M=M-D" ]
    | "neg" -> pointToTop @ [ "M=-M" ]
    | _ -> []

let getStaticVariableName index = currentFileName + "." + string index

let writePush segment index =
    match segment with
    | "constant" -> [ "@" + string index; "D=A" ] @ pushFromD
    | "static" -> [ "@" + getStaticVariableName index; "D=M" ] @ pushFromD
    | _ -> [] // Handle other segments as needed

let writePop segment index =
    match segment with
    | "static" -> popToD @ [ "@" + getStaticVariableName index; "M=D" ]
    | _ -> [] // Handle other segments as needed

let translateCommandToAsm command =
    match command with
    | Arithmetic operation -> writeArithmetic operation
    | Push(segment, index) -> writePush segment index
    | Pop(segment, index) -> writePop segment index

let separateCommandToParts (line: string) =
    let parts = line.Split([| ' ' |], StringSplitOptions.RemoveEmptyEntries)

    match parts[0] with
    | "push" -> Push(parts[1], int parts[2])
    | "pop" -> Pop(parts[1], int parts[2])
    | operation -> Arithmetic operation

let translate (inputPath: string) =
    let outputPath = Path.ChangeExtension(inputPath, ".asm")
    currentFileName <- Path.GetFileName(Path.ChangeExtension(inputPath, null))

    let commands =
        File.ReadAllLines inputPath
        |> Array.map stripComments
        |> Array.filter (fun line -> line <> "")
        |> Array.map separateCommandToParts

    let assemblyLines =
        commands
        |> Array.collect (fun command -> translateCommandToAsm command |> List.toArray)

    File.WriteAllLines(outputPath, assemblyLines)
