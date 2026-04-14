// Eyal Schachter | TZ 209792266
// Jeremy Jacob | TZ 345570451

open VMtranslator

[<EntryPoint>]
let main argv =
    translate argv[0]
    0
