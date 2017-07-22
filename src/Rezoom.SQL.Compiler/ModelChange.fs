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
    static member private Qualify(objName : ObjectName<_>, temp : bool) =
        stateful {
            let! model = State.get
            let defaultSchema = if temp then model.TemporarySchema else model.DefaultSchema
            return
                {   Source = objName.Source
                    Value =
                        {   SchemaName = objName.SchemaName |? defaultSchema
                            ObjectName = objName.ObjectName
                        }
                }
        }
    static member private Qualify(objName) = ModelChange.Qualify(objName, false)
    static member private AddColumnDef(tableName, column : ColumnDef<_, _> WithSource) =
        stateful {
            let ty = ColumnType.OfTypeName(column.Value.Type, nullable = false)
            let columnName = { Source = column.Source; Value = column.Value.Name }
            do! ModelOps.addTableColumn tableName columnName ty
            for constr in column.Value.Constraints do
                let! constraintType = ModelChange.ColumnConstraintType(constr)
                // more specific source info here?
                let constraintName = { Source = column.Source; Value = constr.Name }
                let cols = Set.singleton column.Value.Name
                do! ModelOps.addConstraint tableName constraintName constraintType cols
        }
    member private this.CreateTable(create : InfCreateTableStmt) =
        stateful {
            let! tableName = ModelChange.Qualify(create.Name, create.Temporary)
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
                    do! ModelChange.AddColumnDef(tableName, column)
                for constr in def.Constraints do
                    let! constraintType, cols = ModelChange.TableConstraint(constr.Value)
                    let constraintName = constr.Map(fun c -> c.Name)
                    do! ModelOps.addConstraint tableName constraintName constraintType cols
        } |> State.runForOutputState model |> Some
    member this.AlterTable(alter : InfAlterTableStmt) =
        stateful {
            let! tableName = ModelChange.Qualify(alter.Table)
            match alter.Alteration with
            | RenameTo newName ->
                let newName = 
                    {   Source = tableName.Source
                        Value = { SchemaName = tableName.Value.SchemaName; ObjectName = newName }
                    }
                return! ModelOps.renameTable tableName newName
            | AddColumn column ->
                return! ModelChange.AddColumnDef(tableName, column)
        } |> State.runForOutputState model |> Some
    member this.CreateView(create : InfCreateViewStmt) =
        stateful {
            let stripper = ASTMapping.Stripper()
            let stripped = stripper.CreateView(create)
            let! viewName = ModelChange.Qualify(create.ViewName)
            return! ModelOps.createView viewName stripped
        } |> State.runForOutputState model |> Some
    member this.DropObject(drop : InfDropObjectStmt) =
        stateful {
            let! objName = ModelChange.Qualify(drop.ObjectName)
            match drop.Drop with
            | DropIndex ->
                return! ModelOps.dropIndex objName
            | DropView ->
                return! ModelOps.dropView objName
            | DropTable ->
                return! ModelOps.dropTable objName
        } |> State.runForOutputState model |> Some
    member this.CreateIndex(create : InfCreateIndexStmt) =
        stateful {
            let! tableName = ModelChange.Qualify(create.TableName)
            let! indexName = ModelChange.Qualify(create.IndexName)
            let cols = create.IndexedColumns |> Seq.map (fun w -> fst w.Value) |> Set.ofSeq
            return! ModelOps.createIndex tableName indexName cols
        } |> State.runForOutputState model |> Some         
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

