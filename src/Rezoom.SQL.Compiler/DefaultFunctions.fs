module Rezoom.SQL.Compiler.DefaultFunctions
open Rezoom.SQL.Compiler.FunctionDeclarations

/// Functions that are supported by EVERY database. Surprisingly there aren't many of these.
let common =
    [|  func "abs" [ numeric (infect a') ] a'
        func "coalesce" [ nullable a'; vararg (nullable a'); infect a' ] a'
    |]

/// Erased functions that can always be supported, even if the DB doesn't have ANY functions.
let builtins =
    [|  // Used to prevent queries from being assumed idempotent, even though they otherwise seem to be.
        ErasedFunction(Name("impure"), infect a', a', idem = false) :> FunctionType
        // Force its argument to be assumed nullable. This can be used to pick which variable is nullable
        // in cases where we would otherwise make both nullable.
        // For example `coalesce(@a + @b, 1)` could be written `coalesce(nullable(@a) + @b, 1)` so @b would
        // not have to be assumed nullable.
        erased "nullable" (nullable a') (nullable a')
        // Ignore the inferred type (but not inferred nullability) of its argument.
        // Lets you override the typechecker and treat values like whatever you feel they should be.
        erased "unsafe_coerce" (infect scalar) scalar
        erased "unsafe_inject_raw" rawsql any
    |]

let extendedBy backendFunctions =
    Seq.concat
        [|  builtins
            common
            backendFunctions
        |] |> mapBy (fun f -> f.FunctionName)