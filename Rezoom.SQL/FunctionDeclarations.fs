module Rezoom.SQL.FunctionDeclarations
open System.Collections.Generic
open Rezoom.SQL

let a' = ArgumentTypeVariable (Name("a"), None)
let b' = ArgumentTypeVariable (Name("b"), None)
let c' = ArgumentTypeVariable (Name("c"), None)
let d' = ArgumentTypeVariable (Name("d"), None)

let constrained ty arg =
    match arg with
    | ArgumentConcrete _ -> arg
    | ArgumentTypeVariable (name, Some constrs) ->
        ArgumentTypeVariable (name, Some (constrs.Unify(ty) |> resultAt SourceInfo.Invalid))
    | ArgumentTypeVariable (name, None) ->
        ArgumentTypeVariable (name, Some ty)

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
        Aggregate = fun _ -> None
        Idempotent = false
    }

let inline func name args ret = { proc name args ret with Idempotent = true }
let inline aggregate name args ret =
    { func name args ret with Aggregate = fun _ -> Some { AllowWildcard = false; AllowDistinct = true } }

let inline withWildcard (funcTy : FunctionType) =
    { funcTy with Aggregate = fun _ -> Some { AllowWildcard = true; AllowDistinct = true } }
let inline withVarArg ty funcTy =
    { funcTy with VariableArgument = Some { MaxCount = None; Type = ty } }
let inline withOptArg ty funcTy =
    { funcTy with VariableArgument = Some { MaxCount = Some 1; Type = ty } }