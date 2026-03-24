module VMtranslator

open System
open System.IO

type Command =
    | Arithmetic of string
    | Push of string * int
    | Pop of string * int

let stripComments (line: string) =
    let parts = line.Split("//")
    parts[0].Trim()

let popToD = [ "@SP"; "AM=M-1"; "D=M" ]
let pointToTop = [ "@SP"; "A=M-1" ]
let pointToPreviousA = [ "A=A-1" ]

let writeArithmetic operation =
    match operation with
    | "add" -> popToD @ pointToPreviousA @ [ "M=M+D" ]
    | "sub" -> popToD @ pointToPreviousA @ [ "M=M-D" ]
    | "neg" -> pointToTop @ [ "M=-M" ]
    | _ -> []

let translateCommandToAsm command =
    match command with
    | Arithmetic operation -> writeArithmetic operation
    | _ -> []

let separateCommandToParts (line: string) =
    let parts = line.Split([| ' ' |], StringSplitOptions.RemoveEmptyEntries)

    match parts[0] with
    | "push" -> Push(parts[1], int parts[2])
    | "pop" -> Pop(parts[1], int parts[2])
    | operation -> Arithmetic operation

let translate (inputPath: string) =
    let outputPath = Path.ChangeExtension(inputPath, ".asm")

    let commands =
        File.ReadAllLines inputPath
        |> Array.map stripComments
        |> Array.filter (fun line -> line <> "")
        |> Array.map separateCommandToParts

    let assemblyLines =
        commands
        |> Array.collect (fun command -> translateCommandToAsm command |> List.toArray)

    File.WriteAllLines(outputPath, assemblyLines)
