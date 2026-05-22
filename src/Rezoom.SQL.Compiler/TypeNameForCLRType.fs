[<AutoOpen>]
module Rezoom.SQL.Compiler.TypeNameForCLRType
open System
open Rezoom.SQL.Mapping

let clrTypeDict =
    [|  // 1:1 mappings
        typeof<int16>, fun _ -> IntegerTypeName Integer16
        typeof<int32>, fun _ -> IntegerTypeName Integer32
        typeof<int64>, fun _ -> IntegerTypeName Integer64
        typeof<string>, fun ut -> StringTypeName ut.SQLTypeLength
        typeof<Guid>, fun _ -> GuidTypeName
        typeof<single>, fun _ -> FloatTypeName Float32
        typeof<double>, fun _ -> FloatTypeName Float64
        typeof<decimal>, fun _ -> DecimalTypeName
        typeof<bool>, fun _ -> BooleanTypeName
        typeof<DateTime>, fun _ -> DateTimeTypeName
        typeof<DateTimeOffset>, fun _ -> DateTimeOffsetTypeName
        // Looser mappings, not 1:1 match but the best we can choose to store this CLR type
        typeof<uint8>, fun _ -> IntegerTypeName Integer16
        typeof<int8>, fun _ -> IntegerTypeName Integer16
        typeof<uint16>, fun _ -> IntegerTypeName Integer32
        typeof<uint32>, fun _ -> IntegerTypeName Integer64
        typeof<uint64>, fun _ -> IntegerTypeName Integer64
            
    |] |> dict
type UserPrimitiveType with
    member this.UnderlyingSQLTypeName =
        let succ, found = clrTypeDict.TryGetValue(this.UnderlyingCLRType)
        if succ then found(this) else
        bug <|
            sprintf
                "Type %s was believed to be primitive but not found in clrTypeDict mapping"
                this.UserCLRType.FullName