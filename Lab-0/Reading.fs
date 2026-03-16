module Reading

open System
open System.IO

let getFilePath () =
    printf "Enter file path: "
    Console.ReadLine()

let isLogicalCommand command =
    command = "eq" || command = "gt" || command = "lt"

// Apply func1
let function1 = printfn "func1"
// Apply func2
let function2 = printfn "func2"
// Apply func1
let function3 = printfn "func3"

let traverseAllVmFiles () =
    let directoryPath = getFilePath ()

    Directory.GetFiles(directoryPath, "*.vm")
    |> Array.iter (fun file ->
        let mutable counter = 0

        File.ReadLines file
        |> Seq.iter (fun line ->
            let command = line.Split(' ').[0]

            if isLogicalCommand command then
                counter <- counter + 1

            function1
            function2
            function3)


        printfn "counter: %d" counter
        printfn "End of input file: %s" (Path.GetFileName(file)))
