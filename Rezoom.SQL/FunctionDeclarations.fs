module Rezoom.SQL.FunctionDeclarations
open System.Collections.Generic
open Rezoom.SQL

let a' = ArgumentTypeVariable (Name("a"), None)
let b' = ArgumentTypeVariable (Name("b"), None)
let c' = ArgumentTypeVariable (Name("c"), None)
let d' = ArgumentTypeVariable (Name("d"), None)

let constrained types arg =
    let types =
        [ for ty in types do
            match ty with
            | ArgumentConcrete t -> yield t
            | ArgumentTypeVariable (_, Some vs) -> yield! vs
            | ArgumentTypeVariable _ -> ()
        ]
    match arg with
    | ArgumentConcrete _ -> arg
    | ArgumentTypeVariable (name, Some constrs) ->
        ArgumentTypeVariable (name, List.append constrs types |> Some)
    | ArgumentTypeVariable (name, None) ->
        ArgumentTypeVariable (name, Some types)

let inline private concrete ty = ArgumentConcrete { Type = ty; Nullable = false }

let any = concrete AnyType
let boolean = concrete BooleanType
let string = concrete StringType
let float64 = concrete (FloatType Float64)
let float32 = concrete (FloatType Float32)
let int64 = concrete (IntegerType Integer64)
let int32 = concrete (IntegerType Integer32)
let int16 = concrete (IntegerType Integer16)
let int8 = concrete (IntegerType Integer8)
let binary = concrete BinaryType
let datetime = concrete DateTimeType
let datetimeoffset = concrete DateTimeOffsetType
let decimal = concrete DecimalType

let nullable arg =
    match arg with
    | ArgumentTypeVariable _ -> arg
    | ArgumentConcrete con -> ArgumentConcrete { con with Nullable = true }

let inline proc name args ret =
    {   FunctionName = Name(name)
        FixedArguments = args |> List.toArray
        VariableArgument = None
        Output = ret
        AllowWildcard = false
        AllowDistinct = false
        Aggregate = false
        Idempotent = false
    }

let inline func name args ret = { proc name args ret with Idempotent = true }
let inline aggregate name args ret = { func name args ret with Aggregate = true }

let inline withDistinct funcTy =
    { funcTy with AllowDistinct = true }
let inline withWildcard funcTy =
    { funcTy with AllowWildcard = true }
let inline withVarArg ty funcTy =
    { funcTy with VariableArgument = Some ty }