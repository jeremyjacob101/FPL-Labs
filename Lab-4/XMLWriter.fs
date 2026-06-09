module XMLWriter

open Definitions

let indent (level: int) = String.replicate (level * 2) " "

let rec writeNode (node: Node) (level: int) =
    let indentStr = indent level

    if node.Token.IsSome then
        let token = node.Token.Value

        sprintf
            "%s<%s> %s </%s>"
            indentStr
            (token.Kind.ToString().ToLower())
            token.Value
            (token.Kind.ToString().ToLower())
    else
        [ sprintf "%s<%s>" indentStr node.Kind
          if node.Children <> null then
              node.Children
              |> List.map (fun child -> writeNode child (level + 1))
              |> String.concat "\n"
          sprintf "%s</%s>" indentStr node.Kind ]
        |> String.concat "\n"
