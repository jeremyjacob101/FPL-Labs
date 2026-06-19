module SymbolTable

type SymbolKind =
    | Static
    | Field
    | Argument
    | Var

type SubroutineKind =
    | Constructor
    | Function
    | Method

// - - - - - - - - - Name * TypeName * Kind * Index
type SymbolInfo = string * string * SymbolKind * int

let mutable classTable: SymbolInfo list = []
let mutable subroutineTable: SymbolInfo list = []

let mutable staticCount = 0
let mutable fieldCount = 0
let mutable argumentCount = 0
let mutable varCount = 0
