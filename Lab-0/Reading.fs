module Reading

open System
open System.IO

let mutable directoryPath = ""
let mutable currentFileName = ""
let mutable writer: StreamWriter = null

let getInputDirectoryPath () =
    printf "Enter directory path: "
    directoryPath <- Console.ReadLine()

let getOutputFileName () =
    let pathParts = directoryPath.Split(Path.DirectorySeparatorChar)
    let lastPart = pathParts[pathParts.Length - 1]
    lastPart + ".asm"

let isLogicalCommand command =
    command = "eq" || command = "gt" || command = "lt"

let isMemoryAccessCommand command = command = "push" || command = "pop"

let writeMemoryAccess command segment index =
    writer.WriteLine("command: " + command + " segment: " + segment + " index: " + string index)

let handlePush segment index = writeMemoryAccess "push" segment index
let handlePop segment index = writeMemoryAccess "pop" segment index

let handleAdd () = writer.WriteLine "command: add"
let handleSub () = writer.WriteLine "command: sub"
let handleNeg () = writer.WriteLine "command: neg"
let handleEq () = writer.WriteLine "command: eq"
let handleGt () = writer.WriteLine "command: gt"
let handleLt () = writer.WriteLine "command: lt"

let simpleCommandHandler command =
    match command with
    | "add" -> handleAdd ()
    | "sub" -> handleSub ()
    | "neg" -> handleNeg ()
    | "eq" -> handleEq ()
    | "gt" -> handleGt ()
    | "lt" -> handleLt ()
    | _ -> ()

let memoryAccessCommandHandler command segment index =
    match command with
    | "push" -> handlePush segment index
    | "pop" -> handlePop segment index
    | _ -> ()

let traverseAllVmFiles () =
    getInputDirectoryPath ()

    writer <- new StreamWriter(getOutputFileName ())

    Directory.GetFiles(directoryPath, "*.vm")
    |> Array.iter (fun file ->
        currentFileName <- Path.GetFileName(file)
        let mutable counter = 0

        File.ReadLines file
        |> Seq.iter (fun line ->
            let parts = line.Split(' ')
            let command = parts[0]

            simpleCommandHandler command

            if isMemoryAccessCommand command then
                let segment = parts[1]
                let index = parts[2]
                memoryAccessCommandHandler command segment index

            if isLogicalCommand command then
                counter <- counter + 1
                writer.WriteLine("counter: " + string counter))

        printfn "End of input file: %s" currentFileName)

    writer.Close()

    printfn "Output file is ready: %s" (getOutputFileName ())
