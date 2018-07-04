module Rezoom.SQL.Compiler.FunctionDeclarations
open System.Collections.Generic
open Rezoom.SQL.Compiler

let private argumentTypeVariable name =
    {   TypeConstraint = ScalarTypeClass
        TypeVariable = Some (Name(name))
        ForceNullable = false
        InfectNullable = false
        VarArg = None
    }

let a' = argumentTypeVariable "a"
let b' = argumentTypeVariable "b"
let c' = argumentTypeVariable "c"
let d' = argumentTypeVariable "d"

let constrained ty arg =
    { arg with
        TypeConstraint =
            match arg.TypeConstraint.Unify(ty) with
            | Ok t -> t
            | Error e -> bug e
    }

let fracish ty = ty |> constrained FractionalTypeClass
let intish ty = ty |> constrained IntegralTypeClass
let numeric ty = ty |> constrained NumericTypeClass
let stringish ty = ty |> constrained StringishTypeClass

let inline private concrete ty =
    {   TypeConstraint = ty
        TypeVariable = None
        ForceNullable = false
        InfectNullable = false
        VarArg = None
    }

let any = concrete AnyTypeClass
let scalar = concrete ScalarTypeClass
let boolean = concrete BooleanType
let string = concrete StringType
let num = concrete NumericTypeClass
let fractional = concrete FractionalTypeClass
let float64 = concrete (FloatType Float64)
let float32 = concrete (FloatType Float32)
let integral = concrete IntegralTypeClass
let int64 = concrete (IntegerType Integer64)
let int32 = concrete (IntegerType Integer32)
let int16 = concrete (IntegerType Integer16)
let binary = concrete BinaryType
let datetime = concrete DateTimeType
let datetimeoffset = concrete DateTimeOffsetType
let datetimey = concrete DateTimeishTypeClass
let decimal = concrete DecimalType
let guid = concrete GuidType
let rawsql = concrete RawSQLType

let nullable arg =
    { arg with
        ForceNullable = true  
    }

let optional arg =
    { arg with
        VarArg = Some { MinArgCount = 0; MaxArgCount = Some 1 }
    }

let vararg arg =
    { arg with
        VarArg = Some { MinArgCount = 0; MaxArgCount = None }
    }

let varargN count arg =
    { arg with
        VarArg = Some { MinArgCount = 0; MaxArgCount = Some count }
    }

let infect arg =
    { arg with
        InfectNullable = true
    }

type NonAggregateFunction(name, args, ret, idem) =
    inherit FunctionType(name, args, ret, idem)
    override __.Aggregate(_) = None

type AggregateFunction(name, args, ret, allowWildcard, allowDistinct) =
    inherit FunctionType(name, args, ret, idem = true)
    override __.Aggregate(_) =
        Some { AllowWildcard = allowWildcard; AllowDistinct = allowDistinct }

let inline proc name args ret = NonAggregateFunction(Name(name), List.toArray args, ret, idem = false) :> FunctionType
let inline func name args ret = NonAggregateFunction(Name(name), List.toArray args, ret, idem = true) :> FunctionType
let inline aggregate name args ret = AggregateFunction(Name(name), List.toArray args, ret, false, true) :> FunctionType
let inline aggregateW name args ret = AggregateFunction(Name(name), List.toArray args, ret, true, true) :> FunctionType

type ErasedFunction(name, arg, ret, idem) =
    inherit FunctionType(name, [| arg |], ret, idem)
    override __.Erased = true
    override __.Aggregate(_) = None

let inline erased name arg ret = ErasedFunction(Name(name), arg, ret, idem = true) :> FunctionType