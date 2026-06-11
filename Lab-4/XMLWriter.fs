module XMLWriter

open Definitions

let indent (level: int) = String.replicate (level * 2) " "

let isNotEmptyString (s: string) = s.Trim() <> ""

let tokenTagName kind =
    match kind with
    | IntConst -> "integerConstant"
    | StringConst -> "stringConstant"
    | _ -> kind.ToString().ToLower()

let rec writeNode (node: Node) (level: int) =
    let indentStr = indent level

    match node with
    | Token(kind, value) ->
        let tagName = tokenTagName kind
        sprintf "%s<%s> %s </%s>" indentStr tagName value tagName
    | Node(kind, children) ->
        [ sprintf "%s<%s>" indentStr kind
          children
          |> List.map (fun child -> writeNode child (level + 1))
          |> String.concat "\n"
          sprintf "%s</%s>" indentStr kind ]
        |> List.filter isNotEmptyString
        |> String.concat "\n"
