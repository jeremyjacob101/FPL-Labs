// Eyal Schachter | TZ 209792266
// Jeremy Jacob | TZ 345570451

open System.IO
open Tokenizer

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
        printfn "\n%s\n" path

        for token in tokens do
            printfn "%A %s" token.Kind token.Value

    0
