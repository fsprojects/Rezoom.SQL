namespace Rezoom.SQL
open System
open System.Data
open System.Data.Common
open System.Collections.Generic

type CoreColumnType =
    | BooleanType
    | StringType
    | IntegerType of IntegerSize
    | FloatType of FloatSize
    | DecimalType
    | BinaryType
    | DateTimeType
    | DateTimeOffsetType
    | StringishTypeClass
    | NumericTypeClass
    | IntegralTypeClass
    | FractionalTypeClass
    | ScalarTypeClass
    | AnyTypeClass
    | ListType of CoreColumnType
    member this.ParentType =
        match this with
        | IntegerType Integer8 -> IntegralTypeClass
        | IntegerType Integer16 -> IntegerType Integer8
        | IntegerType Integer32 -> IntegerType Integer16
        | IntegerType Integer64 -> IntegerType Integer32
        | FloatType Float32 -> FractionalTypeClass
        | FloatType Float64 -> FloatType Float32
        | DecimalType -> FractionalTypeClass
        | StringType
        | BinaryType -> StringishTypeClass
        | IntegralTypeClass
        | FractionalTypeClass -> NumericTypeClass
        | BooleanType
        | DateTimeType
        | DateTimeOffsetType
        | NumericTypeClass
        | StringishTypeClass -> ScalarTypeClass
        | ScalarTypeClass
        | AnyTypeClass -> AnyTypeClass
        | ListType element ->
            let elementParent = element.ParentType
            if elementParent = element then AnyTypeClass
            else ListType elementParent
    member this.HasAncestor(candidate) =
        if this = candidate then true else
        let parent = this.ParentType
        if parent = this then false
        else parent.HasAncestor(candidate)
    member left.Unify(right) =
        if left.HasAncestor(right) then
            Ok left
        elif right.HasAncestor(left) then
            Ok right
        else
            Error <| sprintf "The types %O and %O cannot be unified" left right
    override this.ToString() =
        match this with
        | BooleanType -> "BOOL"
        | StringType -> "STRING"
        | IntegerType Integer8 -> "INT8"
        | IntegerType Integer16 -> "INT16"
        | IntegerType Integer32 -> "INT"
        | IntegerType Integer64 -> "INT64"
        | FloatType Float32 -> "FLOAT32"
        | FloatType Float64 -> "FLOAT64"
        | DecimalType -> "DECIMAL"
        | BinaryType -> "BINARY"
        | DateTimeType -> "DATETIME"
        | DateTimeOffsetType -> "DATETIMEOFFSET"
        | FractionalTypeClass -> "<fractional>"
        | IntegralTypeClass -> "<integral>"
        | NumericTypeClass -> "<numeric>"
        | StringishTypeClass -> "<stringish>"
        | ScalarTypeClass -> "<scalar>"
        | AnyTypeClass -> "<any>"
        | ListType t -> "[" + string t + "]"
    static member OfTypeName(typeName : TypeName) =
        match typeName with
        | StringTypeName _ -> StringType
        | BinaryTypeName _ -> BinaryType
        | IntegerTypeName sz -> IntegerType sz
        | FloatTypeName sz -> FloatType sz
        | DecimalTypeName -> DecimalType
        | BooleanTypeName -> BooleanType
        | DateTimeTypeName -> DateTimeType
        | DateTimeOffsetTypeName -> DateTimeOffsetType

type ColumnType =
    {   Type : CoreColumnType
        Nullable : bool
    }
    static member OfTypeName(typeName : TypeName, nullable) =
        {   Type = CoreColumnType.OfTypeName(typeName)
            Nullable = nullable
        }
    member private ty.TypeInfo =
        match ty.Type with
        | IntegerType Integer8 -> DbType.SByte, if ty.Nullable then typeof<Nullable<sbyte>> else typeof<sbyte>
        | IntegerType Integer16 -> DbType.Int16, if ty.Nullable then typeof<Nullable<int16>> else typeof<int16>
        | IntegralTypeClass
        | IntegerType Integer32 -> DbType.Int32, if ty.Nullable then typeof<Nullable<int32>> else typeof<int32>
        | IntegerType Integer64 -> DbType.Int64, if ty.Nullable then typeof<Nullable<int64>> else typeof<int64>
        | FloatType Float32 -> DbType.Single, if ty.Nullable then typeof<Nullable<single>> else typeof<single>
        | FloatType Float64 -> DbType.Double, if ty.Nullable then typeof<Nullable<double>> else typeof<double>
        | BooleanType -> DbType.Boolean, if ty.Nullable then typeof<Nullable<bool>> else typeof<bool>
        | FractionalTypeClass
        | NumericTypeClass
        | DecimalType -> DbType.Decimal, if ty.Nullable then typeof<Nullable<decimal>> else typeof<decimal>
        | DateTimeType ->
            DbType.DateTime, if ty.Nullable then typeof<Nullable<DateTime>> else typeof<DateTime>
        | DateTimeOffsetType ->
            DbType.DateTimeOffset, if ty.Nullable then typeof<Nullable<DateTimeOffset>> else typeof<DateTimeOffset>
        | StringType -> DbType.String, typeof<string>
        | BinaryType -> DbType.Binary, typeof<byte array>
        | StringishTypeClass
        | ScalarTypeClass
        | AnyTypeClass -> DbType.Object, typeof<obj>
        | ListType t ->
            let dbType, clrType = { Type = t; Nullable = ty.Nullable }.TypeInfo
            dbType, clrType.MakeArrayType()
    member ty.CLRType = snd ty.TypeInfo
    member ty.DbType = fst ty.TypeInfo
    override ty.ToString() =
        string ty.Type + (if ty.Nullable then "?" else "")

type FunctionTermType =
    {   TypeConstraint : CoreColumnType
        TypeVariable : Name option
        ForceNullable : bool
        InfectNullable : bool
        VarArg : FunctionTermVarArg option
    }
and FunctionTermVarArg =
    {   MinArgCount : int
        MaxArgCount : int option
    }
    
type AggregateType =
    {   AllowWildcard : bool
        AllowDistinct : bool
    }

[<AbstractClass>]
type FunctionType
    ( name : Name
    , parameters : FunctionTermType IReadOnlyList
    , returns : FunctionTermType
    , idem : bool
    ) =
    do
        let numVarArgs = parameters |> Seq.filter (fun p -> Option.isSome p.VarArg) |> Seq.truncate 2 |> Seq.length
        if numVarArgs > 1 then bug <| sprintf "Can't have more than one vararg to a function (%O)" name
    member __.FunctionName = name
    member __.Parameters = parameters
    member __.Returns = returns
    member __.Idempotent = idem
    /// Whether this function (of one argument) is erased when translated, i.e. `f(x)` becomes just `x`.
    abstract member Erased : bool
    default __.Erased = false
    abstract member Aggregate : FunctionArguments<'t, 'e> -> AggregateType option
    member __.MinimumParameters =
        parameters |> Seq.sumBy (fun p -> match p.VarArg with | None -> 1 | Some v -> v.MinArgCount)
    member internal this.ValidateArgs
        ( source : SourceInfo
        , argList : 'a IReadOnlyList
        , argSource : 'a -> SourceInfo
        , validate : 'a -> FunctionTermType -> unit
        ) =
        let mutable i = 0
        for par in parameters do
            if i >= argList.Count then
                failAt source <|
                    sprintf "Insufficient arguments to function ``%O`` (got %d, expected at least %d)"
                        name
                        argList.Count
                        this.MinimumParameters
            match par.VarArg with
            | None ->
                validate (argList.[i]) par
                i <- i + 1
            | Some varg ->
                let start = i
                // we can consume arguments until we get to this index
                let indexOfLastVarArg = argList.Count - (parameters.Count - i)
                let indexOfLastVarArg =
                    match varg.MaxArgCount with
                    | None -> indexOfLastVarArg
                    | Some maxCount -> min indexOfLastVarArg (i + maxCount - 1)
                while i <= indexOfLastVarArg do
                    validate (argList.[i]) par
                    i <- i + 1
                if i - start < varg.MinArgCount then
                    failAt source <|
                        sprintf "Insufficient arguments to function ``%O`` (expected at least %d varargs)"
                            name
                            varg.MinArgCount
        if i < argList.Count then
            failAt (argSource argList.[i]) <|
                sprintf "Too many arguments to function ``%O`` (expected at most %d)" name (i - 1)