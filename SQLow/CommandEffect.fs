/// A command is a series of SQL statements.
/// This module analyzes the effects of commands, including the tables they update, the changes they make to the model,
/// and the result sets they output.
module SQLow.CommandEffect
open System
open System.Collections.Generic
open SQLow.InferredTypes

type CommandEffect =
    {   ModelChange : Model option
        ResultSets : SchemaQuery IReadOnlyList
        TablesWritten : SchemaTable IReadOnlyCollection
        Parameters : (BindParameter * ColumnType) IReadOnlyList
    }
    static member None =
        {   ModelChange = None
            ResultSets = [||] :> _ IReadOnlyList
            TablesWritten = [||] :> _ IReadOnlyList
            Parameters = [||] :> _ IReadOnlyList
        }

type private StatementResultSet =
    {   Query : InferredQuery
        ReferencedTables : SchemaTable IReadOnlyCollection
    }

type private StatementEffect =
    | ChangeModel of Model
    | WriteTable of SchemaTable
    | ResultSet of StatementResultSet // note: we keep query types inferred until the whole command has been processed

type private CommandEffectBuilder() =
    // shared throughout the whole command, since parameters are too.
    let inference = TypeInferenceContext() :> ITypeInferenceContext
    member this.CommandEffect(model : Model, statements : Stmt seq) =
        let mutable newModel = None
        let writeTables = ResizeArray()
        let resultSets = ResizeArray()
        for stmt in statements do
            let effect = this.StatementEffect(newModel |? model, stmt)
            match effect with
            | Some effect ->
                match effect with
                | ChangeModel m -> newModel <- Some m
                | WriteTable t -> writeTables.Add(t)
                | ResultSet r -> resultSets.Add(r)
            | None -> ()
        let parameters =
            [| for parameter in inference.Parameters ->
                parameter, inference.Concrete(inference.Variable(parameter))
            |]
        let resultSets =
            [| for resultSet in resultSets ->
                this.ResultSetToSchemaQuery(resultSet)
            |]
        {   ModelChange = newModel
            ResultSets = resultSets :> _ IReadOnlyList
            TablesWritten = writeTables :> _ IReadOnlyCollection
            Parameters = parameters :> _ IReadOnlyList
        }

    member private this.ResultSetToSchemaQuery(resultSet : StatementResultSet) =
        let columns =
            [| for col in resultSet.Query.Columns ->
                {   SchemaQueryColumn.ColumnName = col.ColumnName
                    ColumnType = inference.Concrete(col.InferredType)
                    PrimaryKey = col.PrimaryKey
                }
            |]
        {   Columns = columns :> _ IReadOnlyList
            ReferencedTables = resultSet.ReferencedTables
        }

    member private this.ResultSet(model, select : SelectStmt) =
        let checkerContext = TypeCheckerContext(inference)
        let checker = TypeChecker(checkerContext, InferredSelectScope.Root(model))
        let query = checker.InferQueryType(select)
        {   Query = query
            ReferencedTables = checkerContext.References |> toReadOnlyList
        }
    member private this.Effect(model, select : SelectStmt) =
        this.ResultSet(model, select) |> ResultSet |> Some

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
    member private this.CreateTableColumns(model, schemaName : Name, tableName : Name, asSelect : SelectStmt) =
        let query = this.ResultSet(model, asSelect).Query
        [| for column in query.Columns ->
            {   SchemaName = schemaName
                TableName = tableName
                ColumnName = column.ColumnName
                PrimaryKey = column.PrimaryKey // is this really desirable? at least it makes sense for temp...
                ColumnType = inference.Concrete(column.InferredType) // unfortunate but necessary
            }
        |]
    member private this.Effect(model, create : CreateTableStmt) =
        let defaultSchema = if create.Temporary then model.TemporarySchema else model.DefaultSchema
        let schema = create.Name.Value.SchemaName |? defaultSchema
        match model.Schemas.TryFind(schema) with
        | None -> failAt create.Name.Source <| sprintf "No such schema: ``%O``" schema
        | Some schema ->
            let tableName = create.Name.Value.ObjectName
            if schema.Tables.ContainsKey(tableName) then
                if create.IfNotExists then None
                else failAt create.Name.Source <| sprintf "Table ``%O`` already exists" create.Name.Value
            else
                let columns =
                    match create.As with
                    | CreateAsSelect select -> this.CreateTableColumns(model, schema.SchemaName, tableName, select)
                    | CreateAsDefinition def -> this.CreateTableColumns(schema.SchemaName, tableName, def)  
                let table =
                    {   SchemaName = schema.SchemaName
                        TableName = create.Name.Value.ObjectName
                        Columns = columns :> _ IReadOnlyList
                    }
                let schema =
                    { schema with Tables = schema.Tables |> Map.add table.TableName table }
                Some (ChangeModel { model with Schemas = model.Schemas |> Map.add schema.SchemaName schema })

    member private this.StatementEffect(model, stmt : Stmt) =
        match stmt with
        | AlterTableStmt alter -> failwith "not implemented"
        | AnalyzeStmt objectName -> None
        | AttachStmt (attach, name) -> failwith "not implemented"
        | BeginStmt transaction -> None
        | CommitStmt -> None
        | CreateIndexStmt create -> failwith "not implemented"
        | CreateTableStmt create -> this.Effect(model, create)
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
        | SelectStmt select -> this.Effect(model, select)
        | ExplainStmt stmt -> None
        | UpdateStmt update -> failwith "not implemented"
        | VacuumStmt -> None

type CommandWithEffect =
    {   Statements : Stmt IReadOnlyList
        Effect : CommandEffect
    }
    static member Parse(model, sourceName, sourceText) =
        let statements = Parser.parseStatements sourceName sourceText
        let builder = CommandEffectBuilder()
        {   Statements = statements :> _ IReadOnlyList
            Effect = builder.CommandEffect(model, statements)
        }


let ofSql str model = CommandWithEffect.Parse(model, "(anonymous)", str).Effect