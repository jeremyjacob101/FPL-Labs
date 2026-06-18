// Eyal Schachter | TZ 209792266
// Jeremy Jacob | TZ 345570451

open System.IO
open Definitions
open Tokenizer
open Parser
open XMLWriter

let getJackFiles path =
    if File.Exists path then
        [ path ]
    else
        Directory.GetFiles(path, "*.jack") |> Array.toList

let tokenizeFile path =
    let code = File.ReadAllText path
    let tokens = tokenize code
    path, tokens

[<EntryPoint>]
let main argv =
    let tokenizedFiles =
        argv |> Array.toList |> List.collect getJackFiles |> List.map tokenizeFile

    for path, tokens in tokenizedFiles do
        let outputPath =
            Path.GetDirectoryName path
            + "/"
            + Path.GetFileNameWithoutExtension path
            + "AST"
            + ".xml"

        let tree = parseProgram (Path.GetFileNameWithoutExtension path) tokens

        let xmlContent = writeNode 0 tree
        // let xmlContent = writeNode tree 0

        File.WriteAllText(outputPath, xmlContent + "\n")

    0
