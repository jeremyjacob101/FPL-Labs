module XMLWriter

open Definitions

let indent (level: int) = String.replicate (level * 2) " "

let isNotEmptyString (s: string) = s.Trim() <> ""

let rec writeNode (node: Node) (level: int) =
    let indentStr = indent level

    match node with
    | Token(kind, value) ->
        sprintf "%s<%s> %s </%s>" indentStr (kind.ToString().ToLower()) value (kind.ToString().ToLower())
    | Node(kind, children) ->
        [ sprintf "%s<%s>" indentStr kind
          children
          |> List.map (fun child -> writeNode child (level + 1))
          |> String.concat "\n"
          sprintf "%s</%s>" indentStr kind ]
        |> List.filter isNotEmptyString
        |> String.concat "\n"
