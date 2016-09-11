module Rezoom.ORM.SQLProvider.Language
open System
open System.Collections.Generic
open Rezoom.ORM.SQLProvider.InferredTypes

type LanguageStatement =
    {
        ModelChange : IModel option
        ResultSets : ISchemaQuery IReadOnlyList
        TablesRead : ISchemaTable IReadOnlyList
        TablesWritten : ISchemaTable IReadOnlyList
        Parameters : (BindParameter * ColumnType) IReadOnlyList
    }


let nullStatement =
    {
        ModelChange = None
        ResultSets = [||] :> _ IReadOnlyList
        TablesRead = [||] :> _ IReadOnlyList
        TablesWritten = [||] :> _ IReadOnlyList
        Parameters = [||] :> _ IReadOnlyList
    }

let selectStatement model select = // TODO: primary key-ness
    let cxt = TypeCheckerContext()
    let checker = TypeChecker(cxt, InferredSelectScope.Root(model))
    let query = checker.InferQueryType(select)
    let tables = cxt.References |> toReadOnlyList
    let mutable query = Unchecked.defaultof<ISchemaQuery>
    let columns =
        seq {
            for col in query.Columns ->
                { new ISchemaQueryColumn with
                    member __.Query = query
                    member __.ColumnName = col.ColumnName
                    member __.ColumnType = col.ColumnType
                }
        } |> toReadOnlyList
    let columnsByName = lazy (columns |> ciDictBy (fun c -> c.ColumnName))
    query <-
        { new ISchemaQuery with
            member __.Columns = columns
            member __.ColumnsByName = columnsByName.Value
            member __.ReferencedTables = upcast tables
        }
    { nullStatement with
        TablesRead = tables
        Parameters = cxt.Parameters |> toReadOnlyList
        ResultSets = [| query |] :> _ IReadOnlyList
    }

let createTableStatement (model : IModel) (create : CreateTableStmt) =
    let defaultSchema = if create.Temporary then model.TemporarySchema else model.DefaultSchema
    let schema = defaultArg create.Name.Value.SchemaName defaultSchema
    let schema = model.Schemas.[schema] // TODO nice error if schema doesn't exist
    if schema.Tables.ContainsKey(create.Name.Value.ObjectName) then
        if create.IfNotExists then nullStatement
        else failAt create.Name.Source <| sprintf "Table ``%O`` already exists" create.Name.Value
    else
        let mutable table = Unchecked.defaultof<ISchemaTable>
        let columns =
            match create.As with
            | CreateAsSelect select ->
                let results = (selectStatement model select).ResultSets.[0] // TODO nice error if no results
                [| for column in results.Columns ->
                    { new ISchemaColumn with
                        member __.Table = table
                        member __.PrimaryKey = false
                        member __.ColumnName = column.ColumnName
                        member __.ColumnType = column.ColumnType
                    }
                |]
            | CreateAsDefinition def ->
                let tablePkColumns =
                    seq {
                        for constr in def.Constraints do
                            match constr.TableConstraintType with
                            | TableIndexConstraint { Type = PrimaryKey; IndexedColumns = indexed } ->
                                for expr, _ in indexed do
                                    match expr.Value with
                                    | ColumnNameExpr name -> yield name.ColumnName
                                    | _ -> ()
                            | _ -> ()
                    } |> ciDictBy id
                [| for column in def.Columns ->
                    let affinity =
                        match column.Type with
                        | None -> AnyType
                        | Some name -> InferredType.Affinity(name)
                    let hasNotNullConstraint =
                        column.Constraints
                        |> Seq.exists(function | { ColumnConstraintType = NotNullConstraint _ } -> true | _ -> false)
                    let isPrimaryKey =
                        tablePkColumns.ContainsKey(column.Name)
                        || column.Constraints |> Seq.exists(function
                            | { ColumnConstraintType = PrimaryKeyConstraint _ } -> true
                            | _ -> false)
                    { new ISchemaColumn with
                        member __.Table = table
                        member __.PrimaryKey = isPrimaryKey
                        member __.ColumnName = column.Name
                        member __.ColumnType =
                            {
                                Type = affinity
                                Nullable = not hasNotNullConstraint
                            }
                    }
                |]
        let columnsByName = lazy (columns |> ciDictBy (fun c -> c.ColumnName))
        let query = lazy (TableQuery(table) :> ISchemaQuery)
        table <-
            { new ISchemaTable with
                member __.SchemaName = schema.SchemaName
                member __.TableName = create.Name.Value.ObjectName
                member __.Columns = upcast columns
                member __.ColumnsByName = columnsByName.Value
                member __.Query = query.Value
            }
        { nullStatement with
            ModelChange = None // TODO update model
        }

let languageStatement (model : IModel) (stmt : Stmt) =
    match stmt with
    | AlterTableStmt alter -> failwith "not implemented"
    | AnalyzeStmt objectName -> nullStatement
    | AttachStmt (attach, name) -> failwith "not implemented"
    | BeginStmt transaction -> nullStatement
    | CommitStmt -> nullStatement
    | CreateIndexStmt create -> failwith "not implemented"
    | CreateTableStmt create -> createTableStatement model create
    | CreateTriggerStmt create -> failwith "not implemented"
    | CreateViewStmt create -> failwith "not implemented"
    | CreateVirtualTableStmt _ -> nullStatement
    | DeleteStmt delete -> failwith "not implemented"
    | DetachStmt detatch -> failwith "not implemented"
    | DropObjectStmt drop -> failwith "not implemented"
    | InsertStmt insert -> failwith "not implemented"
    | PragmaStmt pragma -> nullStatement
    | ReindexStmt objectName -> failwith "not implemented"
    | ReleaseStmt name -> nullStatement
    | RollbackStmt rollback -> nullStatement
    | SavepointStmt name -> nullStatement
    | SelectStmt select -> selectStatement model select
    | ExplainStmt stmt -> nullStatement
    | UpdateStmt update -> failwith "not implemented"
    | VacuumStmt -> nullStatement