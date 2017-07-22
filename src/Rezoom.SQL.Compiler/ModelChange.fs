namespace Rezoom.SQL.Compiler
open System
open System.Collections.Generic
open Rezoom.SQL.Compiler.InferredTypes

type private ModelChange(model : Model, inference : ITypeInferenceContext) =
    member private this.CreateTable(create : InfCreateTableStmt) =
        stateful {
            let! tableName = ComplexModelOps.qualifyTemp create.Temporary create.Name
            match create.As with
            | CreateAsSelect select ->
                let query = select.Value.Info.Query
                let concreteQuery = query.Map(inference.Concrete)
                return! ComplexModelOps.createTableByQuery tableName concreteQuery
            | CreateAsDefinition def ->
                return! ComplexModelOps.createTableByDefinition tableName def
        } |> State.runForOutputState model |> Some
    member this.AlterTable(alter : InfAlterTableStmt) =
        stateful {
            let! tableName = ComplexModelOps.qualify alter.Table
            match alter.Alteration with
            | RenameTo newName ->
                let newName = 
                    {   Source = tableName.Source
                        Value = { SchemaName = tableName.Value.SchemaName; ObjectName = newName }
                    }
                return! ModelOps.renameTable tableName newName
            | AddColumn column ->
                return! ComplexModelOps.addColumnDef tableName column
        } |> State.runForOutputState model |> Some
    member this.CreateView(create : InfCreateViewStmt) =
        stateful {
            let stripper = ASTMapping.Stripper()
            let stripped = stripper.CreateView(create)
            let! viewName = ComplexModelOps.qualify create.ViewName
            return! ModelOps.createView viewName stripped
        } |> State.runForOutputState model |> Some
    member this.DropObject(drop : InfDropObjectStmt) =
        stateful {
            let! objName = ComplexModelOps.qualify drop.ObjectName
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
            let! tableName = ComplexModelOps.qualify create.TableName
            let! indexName = ComplexModelOps.qualify create.IndexName
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

