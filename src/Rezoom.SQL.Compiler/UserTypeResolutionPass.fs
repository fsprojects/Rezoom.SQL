namespace Rezoom.SQL.Compiler
open Rezoom.SQL.Mapping

// First pass through after parsing to resolve user types by name.
// Simple dictionary lookup, no need to do inference here. Inference
// then has the benefit of knowing the underlying for each user primitive.

type private UserTypeResolutionPass(userTypeLibrary : UserTypeLibrary) =
    inherit ASTMapping<unit, unit, unit, unit>((fun _ -> ()), (fun _ -> ()))
    override this.TypeName(typeName : TypeName WithSource) =
        match typeName.Value with
        | UnresolvedTypeName name ->
            match userTypeLibrary.UserPrimitiveByName(name) with
            | FoundType userType -> { typeName with Value = ResolvedUserType userType }
            | NotFoundType [||] ->
                failAt typeName.Source <| Error.typeNameNotFound name userTypeLibrary.Identity
            | NotFoundType candidates ->
                failAt typeName.Source <| Error.typeNameNotFoundButClose name candidates
            | AmbiguousType candidates ->
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