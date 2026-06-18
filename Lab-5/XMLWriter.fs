module XMLWriter

open Definitions

let indent (level: int) = String.replicate (level * 2) " "

let isNotEmptyString (s: string) = s.Trim() <> ""

let tokenTagName kind =
    match kind with
    | IntConst -> "integerConstant"
    | StringConst -> "stringConstant"
    | _ -> kind.ToString().ToLower()

let xmlValue (value: string) =
    value.Replace("&", "&amp;").Replace("<", "&lt;").Replace(">", "&gt;")

let rec writeNode (level: int) (node: Node) =
    let indentStr = indent level

    match node with
    | Token(kind, value) ->
        let tagName = tokenTagName kind
        sprintf "%s<%s> %s </%s>" indentStr tagName (xmlValue value) tagName
    | Node(kind, children) ->
        [ sprintf "%s<%s>" indentStr kind
          children
          |> List.map (writeNode (level + 1))
          |> List.filter isNotEmptyString
          |> String.concat "\n"
          sprintf "%s</%s>" indentStr kind ]
        |> List.filter isNotEmptyString
        |> String.concat "\n"
    | Empty -> ""
