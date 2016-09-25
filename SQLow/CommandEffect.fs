/// A command is a series of SQL statements.
/// This module analyzes the effects of commands, including the tables they update, the changes they make to the model,
/// and the result sets they output.
module SQLow.CommandEffect
open System
open System.Collections.Generic
open SQLow.InferredTypes

type CommandEffect =
    {   Statements : TStmt IReadOnlyList
        Parameters : (BindParameter * ColumnType) IReadOnlyList
        ModelChange : Model option
    }
    static member None =
        {   Statements = [||] :> _ IReadOnlyList
            Parameters = [||] :> _ IReadOnlyList
            ModelChange = None
        }

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
        | AttachStmt (attach, name) -> failwith "not implemented"
        | CreateTableStmt create -> this.CreateTable(create)
        | CreateViewStmt create -> failwith "not implemented"
        | CreateVirtualTableStmt create -> failwith "not implemented"
        | CreateIndexStmt create -> failwith "not implemented"
        | CreateTriggerStmt create -> failwith "not implemented"
        | AnalyzeStmt _
        | BeginStmt _
        | CommitStmt
        | DeleteStmt _
        | DetachStmt _
        | DropObjectStmt _
        | InsertStmt _
        | PragmaStmt _
        | ReindexStmt _
        | ReleaseStmt _
        | RollbackStmt _
        | SavepointStmt _
        | SelectStmt _
        | ExplainStmt _
        | UpdateStmt _
        | VacuumStmt -> None

type private CommandEffectBuilder(model : Model) =
    // shared throughout the whole command, since parameters are too.
    let inference = TypeInferenceContext() :> ITypeInferenceContext
    let inferredStmts = ResizeArray()
    let mutable newModel = None
    member this.AddStatement(stmt : Stmt) =
        let model = newModel |? model
        let checker = TypeChecker2(inference, InferredSelectScope.Root(model))
        let inferredStmt = checker.Stmt(stmt)
        inferredStmts.Add(inferredStmt)
        newModel <- ModelChange(model, inference).Statment(inferredStmt)
    member this.CommandEffect() =
        let mapping =
            ASTMapping<InferredType ObjectInfo, InferredType ExprInfo, _, _>
                ((fun t -> t.Map(inference.Concrete)), fun e -> e.Map(inference.Concrete))
        let stmts = inferredStmts |> Seq.map mapping.Stmt |> toReadOnlyList
        let pars =
            inference.Parameters
            |> Seq.map (fun p -> p, inference.Concrete(inference.Variable(p)))
            |> toReadOnlyList
        {   Statements = stmts
            ModelChange = newModel
            Parameters = pars
        }