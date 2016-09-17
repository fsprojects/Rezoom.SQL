/// A command is a series of SQL statements.
/// This module analyzes the effects of commands, including the tables they update, the changes they make to the model,
/// and the result sets they output.
module Rezoom.ORM.SQLProvider.Command
open SQLow
open System
open System.Collections.Generic
open Rezoom.ORM.SQLProvider.InferredTypes

type CommandEffect =
    {
        ModelChange : Model option
        ResultSets : SchemaQuery IReadOnlyList
        TablesWritten : SchemaTable IReadOnlyCollection
        Parameters : (BindParameter * ColumnType) IReadOnlyList
    }
    static member None =
        {
            ModelChange = None
            ResultSets = [||] :> _ IReadOnlyList
            TablesWritten = [||] :> _ IReadOnlyList
            Parameters = [||] :> _ IReadOnlyList
        }

type private StatementResultSet =
    {
        Query : InferredQuery
        ReferencedTables : SchemaTable IReadOnlyCollection
    }

type private StatementEffect =
    | ChangeModel of Model
    | WriteTable of SchemaTable
    | ResultSet of StatementResultSet // note: we keep query types inferred until the whole command has been processed

type CommandEffectBuilder(model : Model) =
    let inference = TypeInferenceContext() // shared throughout the whole command, since parameters are too
    member private this.ResultSet(select : SelectStmt) =
        let checkerContext = TypeCheckerContext(inference)
        let checker = TypeChecker(checkerContext, InferredSelectScope.Root(model))
        let query = checker.InferQueryType(select)
        {
            Query = query // TODO: track primary key-ness
            ReferencedTables = checkerContext.References |> toReadOnlyList
        }
    member private this.Effect(select : SelectStmt) =
        this.ResultSet(select) |> ResultSet |> Some
    member private this.CreateTableColumns(schemaName : Name, tableName : Name, def : CreateTableDefinition) =
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
            } |> Set.ofSeq
        [| for column in def.Columns ->
            let affinity = column.Type |> Option.map InferredType.Affinity |? AnyType
            let hasNotNullConstraint =
                column.Constraints
                |> Seq.exists(function | { ColumnConstraintType = NotNullConstraint _ } -> true | _ -> false)
            let isPrimaryKey =
                tablePkColumns.Contains(column.Name)
                || column.Constraints |> Seq.exists(function
                    | { ColumnConstraintType = PrimaryKeyConstraint _ } -> true
                    | _ -> false)
            {   SchemaName = schemaName
                TableName = tableName
                PrimaryKey = isPrimaryKey
                ColumnName = column.Name
                ColumnType = { Type = affinity; Nullable = not hasNotNullConstraint }
            }
        |]
    member private this.CreateTableColumns(schemaName : Name, tableName : Name, asSelect : SelectStmt) =
        let query = this.ResultSet(asSelect).Query
        [| for column in query.Columns ->
            {
                SchemaName = schemaName
                TableName = tableName
                ColumnName = column.ColumnName
                PrimaryKey = false // TODO get from inferred query
                ColumnType = inference.Concrete(column.InferredType) // unfortunate but necessary
            }
        |]
    member private this.Effect(create : CreateTableStmt) =
        let defaultSchema = if create.Temporary then model.TemporarySchema else model.DefaultSchema
        let schema = defaultArg create.Name.Value.SchemaName defaultSchema
        let schema = model.Schemas.[schema] // TODO nice error if schema doesn't exist
        let tableName = create.Name.Value.ObjectName
        if schema.Tables.ContainsKey(tableName) then
            if create.IfNotExists then None
            else failAt create.Name.Source <| sprintf "Table ``%O`` already exists" create.Name.Value
        else
            let columns =
                match create.As with
                | CreateAsSelect select -> this.CreateTableColumns(schema.SchemaName, tableName, select)
                | CreateAsDefinition def -> this.CreateTableColumns(schema.SchemaName, tableName, def)  
            let table =
                {
                    SchemaName = schema.SchemaName
                    TableName = create.Name.Value.ObjectName
                    Columns = columns :> _ IReadOnlyList
                }
            let schema =
                { schema with Tables = schema.Tables |> Map.add table.TableName table }
            Some (ChangeModel { model with Schemas = model.Schemas |> Map.add schema.SchemaName schema })
    member private this.Effect(stmt : Stmt) =
        match stmt with
        | AlterTableStmt alter -> failwith "not implemented"
        | AnalyzeStmt objectName -> None
        | AttachStmt (attach, name) -> failwith "not implemented"
        | BeginStmt transaction -> None
        | CommitStmt -> None
        | CreateIndexStmt create -> failwith "not implemented"
        | CreateTableStmt create -> this.Effect(create)
        | CreateTriggerStmt create -> failwith "not implemented"
        | CreateViewStmt create -> failwith "not implemented"
        | CreateVirtualTableStmt _ -> None
        | DeleteStmt delete -> failwith "not implemented"
        | DetachStmt detatch -> failwith "not implemented"
        | DropObjectStmt drop -> failwith "not implemented"
        | InsertStmt insert -> failwith "not implemented"
        | PragmaStmt pragma -> None
        | ReindexStmt objectName -> failwith "not implemented"
        | ReleaseStmt name -> None
        | RollbackStmt rollback -> None
        | SavepointStmt name -> None
        | SelectStmt select -> this.Effect(select)
        | ExplainStmt stmt -> None
        | UpdateStmt update -> failwith "not implemented"
        | VacuumStmt -> None