// Eyal Schachter | TZ 209792266
// Jeremy Jacob | TZ 345570451

module VMtranslator

open System
open System.IO

let mutable currentFileName = ""
let mutable labelCounter = 0
let mutable currentFunctionName = ""

let FRAME_REG = "R13"
let RETURN_REG = "R14"
let GENERAL_PURPOSE_REG = "R15" // Temporary register for general use in complex operations
let TEMP_SEGMENT_BASE = 5 // Base address for temp segment (R5-R12)

type Command =
    | Arithmetic of string
    | Push of string * int
    | Pop of string * int
    | Label of string
    | GoTo of string
    | IfGoTo of string
    | Function of string * int
    | Call of string * int
    | Return

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

let getFileScopedLabel label = currentFileName + "." + label

let getFunctionScopedLabel label =
    if currentFunctionName = "" then
        getFileScopedLabel label
    else
        currentFunctionName + "$" + label

// Label counters for incrementing jump locations to ensure there's no overlap
let nextLabel prefix =
    labelCounter <- labelCounter + 1
    prefix + string labelCounter

let writeComparison jumpCommand =
    let trueLabel = nextLabel "TRUE_"
    let endLabel = nextLabel "END_"

    popToD
    @ pointToPreviousA
    @ [ "D=M-D"
        "@" + trueLabel // If true
        "D;" + jumpCommand
        "@SP" // If false
        "A=M-1"
        "M=0"
        "@" + endLabel // Skip true jump
        "0;JMP"
        "(" + trueLabel + ")" // Land for true jump
        "@SP"
        "A=M-1"
        "M=-1"
        "(" + endLabel + ")" ] // Land for skip jump

let writeArithmetic operation =
    match operation with
    | "add" -> popToD @ pointToPreviousA @ [ "M=D+M" ]
    | "sub" -> popToD @ pointToPreviousA @ [ "M=M-D" ]
    | "neg" -> pointToTop @ [ "M=-M" ]
    | "and" -> popToD @ pointToPreviousA @ [ "M=D&M" ]
    | "or" -> popToD @ pointToPreviousA @ [ "M=D|M" ]
    | "not" -> pointToTop @ [ "M=!M" ]
    | "eq" -> writeComparison "JEQ"
    | "gt" -> writeComparison "JGT"
    | "lt" -> writeComparison "JLT"
    | _ -> []

let writeLabel label =
    [ "(" + getFunctionScopedLabel label + ")" ]

let writeGoTo label =
    [ "@" + getFunctionScopedLabel label; "0;JMP" ]

let writeIfGoTo label =
    popToD @ [ "@" + getFunctionScopedLabel label; "D;JNE" ]

let getStaticVariableName index = currentFileName + "." + string index
let getTempSegmentAddress index = string (TEMP_SEGMENT_BASE + index)
let getPointerSegmentName index = if index = 0 then "THIS" else "THAT"

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

let writeFunction functionName numLocals = []

let writeCall functionName numArgs = []

let writeReturn = []

let writeBootstrap = [ "@256"; "D=A"; "@SP"; "M=D" ] @ writeCall "Sys.init" 0

let translateCommandToAsm command =
    match command with
    | Arithmetic operation -> writeArithmetic operation
    | Push(segment, index) -> writePush segment index
    | Pop(segment, index) -> writePop segment index
    | Label label -> writeLabel label
    | GoTo label -> writeGoTo label
    | IfGoTo label -> writeIfGoTo label
    | Function(functionName, numLocals) -> writeFunction functionName numLocals
    | Call(functionName, numArgs) -> writeCall functionName numArgs
    | Return -> writeReturn

let separateCommandToParts (line: string) =
    let parts = line.Split([| ' ' |], StringSplitOptions.RemoveEmptyEntries)

    match parts[0] with
    | "push" -> Push(parts[1], int parts[2])
    | "pop" -> Pop(parts[1], int parts[2])
    | "label" -> Label(parts[1])
    | "goto" -> GoTo(parts[1])
    | "if-goto" -> IfGoTo(parts[1])
    | "function" -> Function(parts[1], int parts[2])
    | "call" -> Call(parts[1], int parts[2])
    | "return" -> Return
    | operation -> Arithmetic operation

let translateFile filePath =
    currentFileName <- Path.GetFileName(Path.ChangeExtension(filePath, null))
    currentFunctionName <- ""

    File.ReadAllLines filePath
    |> Array.map stripComments
    |> Array.filter (fun line -> line <> "")
    |> Array.map separateCommandToParts
    |> Array.collect (fun command -> translateCommandToAsm command |> List.toArray)

let translate (inputPath: string) =
    let outputPath =
        if Directory.Exists inputPath then
            Path.Combine(inputPath, DirectoryInfo(inputPath).Name + ".asm")
        else
            Path.ChangeExtension(inputPath, ".asm")

    let assemblyLines =
        if Directory.Exists inputPath then
            Array.append
                (writeBootstrap |> List.toArray)
                (Directory.GetFiles(inputPath, "*.vm") |> Array.collect translateFile)
        else
            [| inputPath |] |> Array.collect translateFile

    File.WriteAllLines(outputPath, assemblyLines)
