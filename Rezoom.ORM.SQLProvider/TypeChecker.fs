module private Rezoom.ORM.SQLProvider.TypeChecker
open System
open System.Collections.Generic

type Scope =
    {
        ParentScope : Scope option
        Model : IModel
        QueryVariables : IReadOnlyDictionary<string, ISchemaQuery>
    }

type Result<'x, 'err> =
    | Ok of 'x
    | Error of 'err

let rec allQueryVariables (scope : Scope) =
    seq {
        yield! scope.QueryVariables
        match scope.ParentScope with
        | Some scope -> yield! allQueryVariables scope
        | None -> ()
    } |> Seq.distinctBy (fun kv -> kv.Key)

let resolveUnqualifiedColumnName (scope : Scope) (name : string) =
    let possible =
        seq {
            for KeyValue(varName, query) in allQueryVariables scope do
                let succ, col = query.ColumnsByName.TryGetValue(name)
                if succ then yield varName, col
        } |> Seq.truncate 2 |> Seq.toList
    match possible with
    | [] -> Error <| sprintf "No such column in scope: ``%s``" name
    | [ _, one ] -> Ok one
    | (v1, c1) :: (v2, c2) :: _ ->
        Error <| sprintf "Ambiguous column: ``%s`` (could be from %s.%s or %s.%s)"
            name v1 c2.ColumnName v2 c2.ColumnName

let resolveQueryNameBySchema (schema : ISchema) (name : string) =
    let succ, view = schema.Views.TryGetValue(name)
    if succ then Ok view.Query else
    let succ, table = schema.Tables.TryGetValue(name)
    if succ then Ok table.Query else
    Error <| sprintf "No such object in schema %s: ``%s``" schema.SchemaName name

let rec resolveUnqualifiedQueryName (scope : Scope) (name : string) =
    let succ, tbl = scope.QueryVariables.TryGetValue(name)
    if succ then Ok tbl else
    match scope.ParentScope with
    | None -> 
        let schema = scope.Model.Schemas.[scope.Model.DefaultSchema]
        resolveQueryNameBySchema schema name
    | Some parent -> resolveUnqualifiedQueryName parent name

let resolveQueryName (scope : Scope) (name : ObjectName) =
    match name.SchemaName with
    | None -> resolveUnqualifiedQueryName scope name.ObjectName
    | Some schemaName ->
        let succ, schema = scope.Model.Schemas.TryGetValue(schemaName)
        if not succ then Error <| sprintf "No such schema: ``%s``" schemaName else
        resolveQueryNameBySchema schema name.ObjectName

let resolveColumnName (scope : Scope) (name : ColumnName) =
    match name.Table with
    | None -> resolveUnqualifiedColumnName scope name.ColumnName
    | Some queryName ->
        let query = resolveQueryName scope queryName
        match query with
        | Error e -> Error e
        | Ok query ->
            let succ, col = query.ColumnsByName.TryGetValue(name.ColumnName)
            if succ then Ok col else
            Error <| sprintf "No such column in %O: ``%s``" queryName name.ColumnName


