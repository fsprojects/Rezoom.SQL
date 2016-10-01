namespace SQLow
open System
open System.Collections.Generic
open SQLow.InferredTypes

type private ModelChange(model : Model, inference : ITypeInferenceContext) =
    member private this.CreateTableColumns(schemaName : Name, tableName : Name, def : InfCreateTableDefinition) =
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
    member private this.CreateTableColumns(model, schemaName : Name, tableName : Name, asSelect : InfSelectStmt) =
        let query = asSelect.Value.Info.Table.Query
        [| for column in query.Columns ->
            {   SchemaName = schemaName
                TableName = tableName
                ColumnName = column.ColumnName
                PrimaryKey = column.Expr.Info.PrimaryKey
                ColumnType = inference.Concrete(column.Expr.Info.Type) // unfortunate but necessary
            }
        |]
    member private this.CreateTable(create : InfCreateTableStmt) =
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
                Some { model with Schemas = model.Schemas |> Map.add schema.SchemaName schema }
    member this.Statment(stmt : InfStmt) =
        match stmt with
        | AlterTableStmt alter -> failwith "not implemented"
        | CreateTableStmt create -> this.CreateTable(create)
        | CreateViewStmt create -> failwith "not implemented"
        | CreateVirtualTableStmt create -> failwith "not implemented"
        | CreateIndexStmt create -> failwith "not implemented"
        | CreateTriggerStmt create -> failwith "not implemented"
        | BeginStmt
        | CommitStmt
        | DeleteStmt _
        | DropObjectStmt _
        | InsertStmt _
        | RollbackStmt
        | SelectStmt _
        | UpdateStmt _ -> None

