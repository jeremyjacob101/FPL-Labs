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

// Counts for indexes
let mutable staticCount = 0
let mutable fieldCount = 0
let mutable argumentCount = 0
let mutable varCount = 0

// Clear tables
let startClass () =
    classTable <- []
    subroutineTable <- []

    staticCount <- 0
    fieldCount <- 0
    argumentCount <- 0
    varCount <- 0

let startSubroutine () =
    subroutineTable <- []

    argumentCount <- 0
    varCount <- 0

// Get next index to place in symbol table per kind
let nextIndex kind =
    match kind with
    | Static ->
        let index = staticCount
        staticCount <- staticCount + 1
        index
    | Field ->
        let index = fieldCount
        fieldCount <- fieldCount + 1
        index
    | Argument ->
        let index = argumentCount
        argumentCount <- argumentCount + 1
        index
    | Var ->
        let index = varCount
        varCount <- varCount + 1
        index

// Function that adds to respective symbol table
let addToSymbolTable name typeName kind =
    let index = nextIndex kind
    let symbol: SymbolInfo = name, typeName, kind, index

    match kind with
    | Static
    | Field -> classTable <- classTable @ [ symbol ]
    | Argument
    | Var -> subroutineTable <- classTable @ [ symbol ]

