module Rezoom.SQL.FunctionDeclarations
open System.Collections.Generic
open Rezoom.SQL

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

let inline private concrete ty =
    {   TypeConstraint = ty
        TypeVariable = None
        ForceNullable = false
        InfectNullable = false
        VarArg = None
    }

let any = concrete AnyTypeClass
let boolean = concrete BooleanType
let string = concrete StringType
let float64 = concrete (FloatType Float64)
let float32 = concrete (FloatType Float32)
let integral = concrete (IntegralTypeClass)
let int64 = concrete (IntegerType Integer64)
let int32 = concrete (IntegerType Integer32)
let int16 = concrete (IntegerType Integer16)
let int8 = concrete (IntegerType Integer8)
let binary = concrete BinaryType
let datetime = concrete DateTimeType
let datetimeoffset = concrete DateTimeOffsetType
let decimal = concrete DecimalType

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

let infect arg =
    { arg with
        InfectNullable = true
    }

type Procedure(name, args, ret) =
    inherit FunctionType(name, args, ret, idem = false)
    override __.Aggregate(_) = None

type Function(name, args, ret) =
    inherit FunctionType(name, args, ret, idem = true)
    override __.Aggregate(_) = None

type Aggregate(name, args, ret, allowWildcard, allowDistinct) =
    inherit FunctionType(name, args, ret, idem = true)
    override __.Aggregate(_) =
        Some { AllowWildcard = allowWildcard; AllowDistinct = allowWildcard }

let inline proc name args ret = Procedure(Name(name), List.toArray args, ret) :> FunctionType
let inline func name args ret = Function(Name(name), List.toArray args, ret) :> FunctionType
let inline aggregate name args ret = Aggregate(Name(name), List.toArray args, ret, false, true) :> FunctionType
let inline aggregateW name args ret = Aggregate(Name(name), List.toArray args, ret, true, true) :> FunctionType