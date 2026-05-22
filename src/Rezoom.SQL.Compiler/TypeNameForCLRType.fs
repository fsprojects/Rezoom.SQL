[<AutoOpen>]
module Rezoom.SQL.Compiler.TypeNameForCLRType
open Rezoom.SQL.Mapping

let clrTypeDict =
    [|  // Perfect mappings
        typeof<int16>, fun _ -> IntegerTypeName Integer16
        typeof<int32>, fun _ -> IntegerTypeName Integer32
        typeof<int64>, fun _ -> IntegerTypeName Integer64
        typeof<string>, fun ut -> StringTypeName ut.SQLTypeLength


        // Looser mappings, not 1:1 match
        typeof<int8>, fun _ -> IntegerTypeName Integer16
            
    |] |> dict
type UserPrimitiveType with
    member this.UnderlyingSQLTypeName =
        let succ, found = clrTypeDict.TryGetValue(this.UnderlyingCLRType)
        if succ then found(this) else
        bug <|
            sprintf
                "Type %s was believed to be primitive but not found in clrTypeDict mapping"
                this.UserCLRType.FullName