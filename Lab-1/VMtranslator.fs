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
let pushFromM = [ "D=M" ] @ pushFromD
let pointToTop = [ "@SP"; "A=M-1" ]
let pointToPreviousA = [ "A=A-1" ]

let storeDInTemp tempIndex = [ "@" + tempIndex; "M=D" ]
let loadTempIntoA tempIndex = [ "@" + tempIndex; "A=M" ]

let GENERAL_PURPOSE_REG = "R15" // Temporary register for general use in complex operations

let writeArithmetic operation =
    match operation with
    | "add" -> popToD @ pointToPreviousA @ [ "M=D+M" ]
    | "sub" -> popToD @ pointToPreviousA @ [ "M=M-D" ]
    | "neg" -> pointToTop @ [ "M=-M" ]
    | _ -> []


let TEMP_SEGMENT_BASE = 5 // Base address for temp segment (R5-R12)

let getStaticVariableName index = currentFileName + "." + string index
let getPointerSegmentName index = if index = 0 then "THIS" else "THAT"
let getTempSegmentAddress index = string (TEMP_SEGMENT_BASE + index)

// load the address of (baseSegment + offset) = RAM[BASE] + offset into either A or D, depending on the reg parameter
let loadOffsetIntoTrueReg baseSegment offset reg =
    [ "@" + baseSegment; "D=M"; "@" + string offset; reg + "=D+A" ]

let loadOffsetIntoA baseSegment offset =
    loadOffsetIntoTrueReg baseSegment offset "A"

let loadOffsetIntoD baseSegment offset =
    loadOffsetIntoTrueReg baseSegment offset "D"
// To pop into a segment, we need to calculate the target address first and store it in a temporary register, then pop the value into D, and finally store D into the target address.
let getPopCommands baseSegment index =
    loadOffsetIntoD baseSegment index
    @ storeDInTemp GENERAL_PURPOSE_REG
    @ popToD
    @ loadTempIntoA GENERAL_PURPOSE_REG
    @ [ "M=D" ]


let writePush segment index =
    match segment with
    | "constant" -> [ "@" + string index; "D=A" ] @ pushFromD
    | "static" -> [ "@" + getStaticVariableName index ] @ pushFromM
    | "temp" -> [ "@" + getTempSegmentAddress index ] @ pushFromM
    | "pointer" -> [ "@" + getPointerSegmentName index ] @ pushFromM
    | "local" -> loadOffsetIntoA "LCL" index @ pushFromM
    | "argument" -> loadOffsetIntoA "ARG" index @ pushFromM
    | "this" -> loadOffsetIntoA "THIS" index @ pushFromM
    | "that" -> loadOffsetIntoA "THAT" index @ pushFromM
    | _ -> [] // Handle other segments as needed

let writePop segment index =
    match segment with
    | "static" -> popToD @ [ "@" + getStaticVariableName index; "M=D" ]
    | "temp" -> popToD @ [ "@" + getTempSegmentAddress index; "M=D" ]
    | "pointer" -> popToD @ [ "@" + getPointerSegmentName index; "M=D" ]
    | "local" -> getPopCommands "LCL" index
    | "argument" -> getPopCommands "ARG" index
    | "this" -> getPopCommands "THIS" index
    | "that" -> getPopCommands "THAT" index
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
