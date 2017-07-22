namespace Rezoom.SQL.Compiler
open System
open System.Collections.Generic
open Rezoom.SQL.Compiler.InferredTypes

type private ModelChange(model : Model, inference : ITypeInferenceContext) =
    static member private ColumnConstraintType(colConstraint : ColumnConstraint<_, _>) =
        stateful {
            let! model = State.get
            return
                match colConstraint.ColumnConstraintType with
                | NullableConstraint -> NullableConstraintType
                | PrimaryKeyConstraint pk -> PrimaryKeyConstraintType pk.AutoIncrement
                | DefaultConstraint _ -> DefaultConstraintType
                | ForeignKeyConstraint fk ->
                    let toSchema = fk.ReferencesTable.SchemaName |? model.DefaultSchema
                    {   ToTable = { SchemaName = toSchema; ObjectName = fk.ReferencesTable.ObjectName }
                        ToColumns = fk.ReferencesColumns |> Seq.map (fun c -> c.Value) |> Set.ofSeq
                        OnDelete = fk.OnDelete
                    } |> ForeignKeyConstraintType
                | CollateConstraint _
                | UniqueConstraint -> OtherConstraintType
        }
    static member private TableConstraint(tblConstraint : TableConstraint<_, _>) =
        stateful {
            let! model = State.get
            return
                match tblConstraint.TableConstraintType with
                | TableIndexConstraint indexClause ->
                    let cols = indexClause.IndexedColumns |> Seq.map (fun c -> fst c.Value) |> Set.ofSeq
                    match indexClause.Type with
                    | PrimaryKey -> PrimaryKeyConstraintType false, cols
                    | Unique -> OtherConstraintType, cols
                | TableForeignKeyConstraint (names, fk) ->
                    let cols = names |> Seq.map (fun c -> c.Value) |> Set.ofSeq
                    let toSchema = fk.ReferencesTable.SchemaName |? model.DefaultSchema
                    {   ToTable = { SchemaName = toSchema; ObjectName = fk.ReferencesTable.ObjectName }
                        ToColumns = fk.ReferencesColumns |> Seq.map (fun c -> c.Value) |> Set.ofSeq
                        OnDelete = fk.OnDelete
                    } |> ForeignKeyConstraintType, cols
                | TableCheckConstraint _ ->
                    OtherConstraintType, Set.empty
        }
    member private this.CreateTable(create : InfCreateTableStmt) =
        stateful {
            let tableName =
                let defaultSchema = if create.Temporary then model.TemporarySchema else model.DefaultSchema
                {   Source = create.Name.Source
                    Value =
                        {   SchemaName = create.Name.SchemaName |? defaultSchema
                            ObjectName = create.Name.ObjectName
                        }
                }
            do! ModelOps.createEmptyTable tableName
            match create.As with
            | CreateAsSelect select ->
                let query = select.Value.Info.Query
                for column in query.Columns do
                    let ty = inference.Concrete(column.Expr.Info.Type)
                    let columnName = { Source = column.Expr.Source; Value = column.ColumnName }
                    do! ModelOps.addTableColumn tableName columnName ty
            | CreateAsDefinition def ->
                for column in def.Columns do
                    let ty = ColumnType.OfTypeName(column.Value.Type, nullable = false)
                    let columnName = { Source = column.Source; Value = column.Value.Name }
                    do! ModelOps.addTableColumn tableName columnName ty
                    for constr in column.Value.Constraints do
                        let! constraintType = ModelChange.ColumnConstraintType(constr)
                        // more specific source info here?
                        let constraintName = { Source = column.Source; Value = constr.Name }
                        let cols = Set.singleton column.Value.Name
                        do! ModelOps.addConstraint tableName constraintName constraintType cols
                for constr in def.Constraints do
                    let! constraintType, cols = ModelChange.TableConstraint(constr.Value)
                    let constraintName = constr.Map(fun c -> c.Name)
                    do! ModelOps.addConstraint tableName constraintName constraintType cols
        } |> State.runForOutputState model |> Some
    member this.AlterTable(alter : InfAlterTableStmt) =
        failwith "FIXME"
    member this.CreateView(create : InfCreateViewStmt) =
        let viewName = create.ViewName.ObjectName
        match model.Schema(create.ViewName.SchemaName) with
        | None -> failAt create.ViewName.Source <| Error.noSuchSchema create.ViewName
        | Some schema ->
            match schema.Objects |> Map.tryFind viewName with
            | Some _ ->
                failAt create.ViewName.Source <| Error.objectAlreadyExists create.ViewName
            | None ->
                let view =
                    {   SchemaName = schema.SchemaName
                        ViewName = viewName
                        CreateDefinition = ASTMapping.Stripper().CreateView(create)
                    } |> SchemaView
                let schema = { schema with Objects = schema.Objects |> Map.add create.ViewName.ObjectName view }
                Some { model with Schemas = model.Schemas |> Map.add schema.SchemaName schema }
    member this.DropObject(drop : InfDropObjectStmt) =
        let objName = drop.ObjectName.ObjectName
        let typeName =
            match drop.Drop with
            | DropTable -> "table"
            | DropView -> "view"
            | DropIndex -> "index"
        match model.Schema(drop.ObjectName.SchemaName) with
        | None -> failAt drop.ObjectName.Source <| Error.noSuchSchema objName
        | Some schema ->
            let dropped() =
                let droppedSchema = { schema with Objects = schema.Objects |> Map.remove objName }
                Some { model with Schemas = model.Schemas |> Map.add schema.SchemaName droppedSchema }
            match schema.Objects |> Map.tryFind objName with
            | None ->
                failAt drop.ObjectName.Source <| Error.noSuchObject typeName objName
            | Some o ->
                match drop.Drop, o with
                | DropTable, SchemaTable _
                | DropView, SchemaView _ ->
                    dropped()
                | _ ->
                    failAt drop.ObjectName.Source <| Error.objectIsNotA typeName objName
    member this.CreateIndex(create : InfCreateIndexStmt) =
        match model.Schema(create.IndexName.SchemaName), model.Schema(create.TableName.SchemaName) with
        | Some schema, Some tableSchema when schema.SchemaName = tableSchema.SchemaName ->
            let table =
                match schema.Objects |> Map.tryFind create.TableName.ObjectName with
                | None -> failAt create.TableName.Source <| Error.noSuchTable create.TableName
                | Some (SchemaTable table) -> table
                | Some _ -> failAt create.TableName.Source <| Error.objectIsNotA "table" create.TableName
            if schema.ContainsObject(create.IndexName.ObjectName) then
                failAt create.IndexName.Source <| Error.objectAlreadyExists create.IndexName
            let index =
                {   SchemaName = schema.SchemaName
                    TableName = table.TableName
                    IndexName = create.IndexName.ObjectName
                    Columns = create.IndexedColumns |> Seq.map (fun w -> fst w.Value) |> Set.ofSeq
                }
            let table =
                { table with
                    Indexes = Map.add index.IndexName index table.Indexes
                }
            let schema =
                { schema with
                    Objects = schema.Objects |> Map.add table.TableName (SchemaTable table)
                }
            Some { model with Schemas = model.Schemas |> Map.add schema.SchemaName schema }
        | Some _, Some _ ->
            failAt create.IndexName.Source <| Error.indexSchemasMismatch create.IndexName create.TableName
        | None, Some _ ->
            failAt create.IndexName.Source <| Error.noSuchSchema create.IndexName
        | _, None ->
            failAt create.TableName.Source <| Error.noSuchSchema create.TableName
            
    member this.Stmt(stmt : InfStmt) =
        match stmt with
        | AlterTableStmt alter -> this.AlterTable(alter)
        | CreateTableStmt create -> this.CreateTable(create)
        | CreateViewStmt create -> this.CreateView(create)
        | CreateIndexStmt create -> this.CreateIndex(create)
        | DropObjectStmt drop -> this.DropObject(drop)
        | DeleteStmt _
        | InsertStmt _
        | SelectStmt _
        | UpdateStmt _ -> None

