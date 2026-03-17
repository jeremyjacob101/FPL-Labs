module Reading

open System
open System.IO

let mutable directoryPath = ""
let mutable currentFileName = ""
let mutable writer: StreamWriter = null

let getInputDirectoryPath () =
    printf "Enter directory path: "
    directoryPath <- Console.ReadLine()

let getOutputFileName () = directoryPath + ".asm"

let isLogicalCommand command =
    command = "eq" || command = "gt" || command = "lt"

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
            let command = parts.[0]

            simpleCommandHandler command

            if isLogicalCommand command then
                counter <- counter + 1
                writer.WriteLine("counter: " + string counter))

        printfn "End of input file: %s" currentFileName)

    writer.Close()

    printfn "Output file is ready: %s" (getOutputFileName ())
