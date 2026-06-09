// Eyal Schachter | TZ 209792266
// Jeremy Jacob | TZ 345570451

open System.IO
open Tokenizer
open Definitions
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
            + "TOut"
            + ".xml"

        let nodes =
            tokens
            |> List.map (fun token ->
                { Kind = "token"
                  Children = null
                  Token = Some token })

        let xmlContent =
            writeNode
                { Kind = "tokens"
                  Children = nodes
                  Token = None }
                0

        File.WriteAllText(outputPath, xmlContent)

    0
