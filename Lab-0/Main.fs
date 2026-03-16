module Main

open System.IO

let addOOOOOBeforeEachLine fromTextFileName toTextFileName =
    let lines =
        File.ReadAllLines fromTextFileName |> Array.map (fun line -> "OOOO" + line)

    File.WriteAllLines(toTextFileName, lines)
