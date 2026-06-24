namespace Rezoom.SQL.Compiler
open Rezoom.SQL.Mapping

// First pass through after parsing to resolve user types by name.
// Simple dictionary lookup, no need to do inference here. Inference
// then has the benefit of knowing the underlying for each user primitive.



type private UserTypeResolutionPass(userTypeLibrary : UserTypeLibrary) =
    inherit ASTMapping<unit, unit, unit, unit>((fun _ -> ()), (fun _ -> ()))
    static let clrTypeName (tName : TypeName) =
        match tName with
        | GuidTypeName -> typeof<System.Guid>.FullName
        | StringTypeName _ -> typeof<System.String>.FullName
        | BinaryTypeName _ -> typeof<System.Byte[]>.FullName
        | IntegerTypeName Integer16 -> typeof<System.Int16>.FullName
        | IntegerTypeName Integer32 -> typeof<System.Int32>.FullName
        | IntegerTypeName Integer64 -> typeof<System.Int64>.FullName
        | FloatTypeName Float32 -> typeof<System.Single>.FullName
        | FloatTypeName Float64 -> typeof<System.Double>.FullName
        | DecimalTypeName -> typeof<System.Decimal>.FullName
        | BooleanTypeName -> typeof<System.Boolean>.FullName
        | DateTimeTypeName -> typeof<System.DateTime>.FullName
        | DateTimeOffsetTypeName -> typeof<System.DateTimeOffset>.FullName
        | UnresolvedTypeName name -> name
        | ResolvedUserType rty -> rty.Name
    override this.TypeName(typeName : TypeName WithSource) =
        match typeName.Value, userTypeLibrary.UserPrimitiveByName(clrTypeName typeName.Value) with
        | _, FoundType userType -> { typeName with Value = ResolvedUserType userType }
        | UnresolvedTypeName name, NotFoundType [||] ->
            failAt typeName.Source <| Error.typeNameNotFound name userTypeLibrary.Identity
        | UnresolvedTypeName name, NotFoundType candidates ->
            failAt typeName.Source <| Error.typeNameNotFoundButClose name candidates
        | UnresolvedTypeName name, AmbiguousType candidates ->
            let candidates = candidates |> Seq.map (fun t -> t.UserCLRType.FullName)
            failAt typeName.Source <| Error.typeNameAmbiguous name candidates
        | _ -> typeName
    override this.ResultColumns(resultColumns, topLevel : bool) =
        let resolveRowType (rowType : RowType WithSource) =
            let resolvedValue =
                match rowType.Value with
                | ResolvedRowType _ as r -> r
                | UnresolvedRowType name ->
                    match userTypeLibrary.UserRowTypeByName(name) with
                    | FoundType userType -> ResolvedRowType userType
                    | NotFoundType [||] ->
                        failAt rowType.Source <| Error.typeNameNotFound name userTypeLibrary.Identity
                    | NotFoundType candidates ->
                        failAt rowType.Source <| Error.typeNameNotFoundButClose name candidates
                    | AmbiguousType candidates ->
                        let candidates = candidates |> Seq.map (fun t -> t.UserCLRType.FullName)
                        failAt rowType.Source <| Error.typeNameAmbiguous name candidates
            { rowType with Value = resolvedValue }
        match topLevel, resultColumns.RowTypes with
        | false, Some xs when xs.Length > 0 ->
            failAt xs.[0].Source <| Error.rowTypesMayOnlyBeDeclaredAtTopLevel
        | _ -> ()
        {   Distinct = resultColumns.Distinct
            Columns = resultColumns.Columns |> rmap this.ResultColumn
            RowTypes =
                resultColumns.RowTypes
                |> Option.map (Array.map resolveRowType)
        }