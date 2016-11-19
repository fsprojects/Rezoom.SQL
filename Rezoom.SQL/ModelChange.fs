namespace Rezoom.SQL
open System
open System.Collections.Generic
open Rezoom.SQL.InferredTypes

type private ModelChange(model : Model, inference : ITypeInferenceContext) = 
    member private this.CreateTableColumns(model, schemaName : Name, tableName : Name, asSelect : InfSelectStmt) =
        let query = asSelect.Value.Info.Query
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
        let schema = create.Name.SchemaName |? defaultSchema
        match model.Schemas.TryFind(schema) with
        | None -> failAt create.Name.Source <| sprintf "No such schema: ``%O``" schema
        | Some schema ->
            let tableName = create.Name.ObjectName
            match schema.Objects |> Map.tryFind tableName with
            | Some (SchemaTable _) ->
                failAt create.Name.Source <| sprintf "Table ``%O`` already exists" create.Name
            | Some _ -> failAt create.Name.Source <| sprintf "Object ``%O`` already exists" create.Name
            | None ->
                let table =
                    match create.As with
                    | CreateAsSelect select ->
                        {   SchemaName = schema.SchemaName
                            TableName = create.Name.ObjectName
                            Columns =
                                this.CreateTableColumns(model, schema.SchemaName, tableName, select)
                                |> mapBy (fun c -> c.ColumnName)
                            Indexes = Map.empty
                            Constraints = Map.empty
                        }
                    | CreateAsDefinition def ->
                        SchemaTable.OfCreateDefinition(schema.SchemaName, tableName, def)
                let schema =
                    { schema with Objects = schema.Objects |> Map.add table.TableName (SchemaTable table) }
                Some { model with Schemas = model.Schemas |> Map.add schema.SchemaName schema }
    member this.AlterTable(alter : InfAlterTableStmt) =
        match model.Schema(alter.Table.SchemaName) with
        | None -> failAt alter.Table.Source <| sprintf "No such schema: ``%O``" alter.Table
        | Some schema ->
            let tblName = alter.Table.ObjectName
            match schema.Objects |> Map.tryFind tblName with
            | None -> failAt alter.Table.Source <| sprintf "No such table: ``%O``" alter.Table
            | Some (SchemaTable tbl) ->
                match alter.Alteration with
                | RenameTo newName ->
                    match schema.Objects |> Map.tryFind newName with
                    | None ->
                        let objects =
                            schema.Objects |> Map.remove tblName |> Map.add newName (SchemaTable tbl)
                        let schema = { schema with Objects = objects }
                        Some { model with Schemas = model.Schemas |> Map.add schema.SchemaName schema }
                    | Some existing ->
                        failAt alter.Table.Source <| sprintf "Object ``%O`` already exists" existing
                | AddColumn col ->
                    let newTbl = tbl.WithAdditionalColumn(col) |> resultAt alter.Table.Source
                    let constraints =
                        col.Constraints
                        |> Seq.fold (fun state con ->
                            Map.add con.Name
                                {   SchemaName = tbl.SchemaName
                                    TableName = tbl.TableName
                                    ConstraintName = con.Name
                                    Columns = Set.singleton col.Name
                                } state) newTbl.Constraints
                    let newTbl = { newTbl with Constraints = constraints }
                    let schema = { schema with Objects = schema.Objects |> Map.add tbl.TableName (SchemaTable newTbl) }
                    Some { model with Schemas = model.Schemas |> Map.add schema.SchemaName schema }
            | Some _ -> failAt alter.Table.Source <| sprintf "Not a table: ``%O``" alter.Table
    member this.CreateView(create : InfCreateViewStmt) =
        let viewName = create.ViewName.ObjectName
        match model.Schema(create.ViewName.SchemaName) with
        | None -> failAt create.ViewName.Source <| sprintf "No such schema: ``%O``" create.ViewName
        | Some schema ->
            match schema.Objects |> Map.tryFind viewName with
            | Some (SchemaView _) ->
                failAt create.ViewName.Source <| sprintf "View already exists: ``%O``" create.ViewName
            | Some _ ->
                failAt create.ViewName.Source <| sprintf "Object already exists: ``%O``" create.ViewName
            | None ->
                let view =
                    {   SchemaName = schema.SchemaName
                        ViewName = viewName
                        // Must make concrete for the schema.
                        Definition = (concreteMapping inference).Select(create.AsSelect)
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
        | None -> failAt drop.ObjectName.Source <| sprintf "No such schema: ``%O``" objName
        | Some schema ->
            let dropped() =
                let droppedSchema = { schema with Objects = schema.Objects |> Map.remove objName }
                Some { model with Schemas = model.Schemas |> Map.add schema.SchemaName droppedSchema }
            match schema.Objects |> Map.tryFind objName with
            | None ->
                failAt drop.ObjectName.Source <| sprintf "No such %s: ``%O``" typeName objName
            | Some o ->
                match drop.Drop, o with
                | DropTable, SchemaTable _
                | DropView, SchemaView _ ->
                    dropped()
                | _ ->
                    failAt drop.ObjectName.Source <| sprintf "Not a %s: ``%O``" typeName objName
    member this.CreateIndex(create : InfCreateIndexStmt) =
        match model.Schema(create.IndexName.SchemaName), model.Schema(create.TableName.SchemaName) with
        | Some schema, Some tableSchema when schema.SchemaName = tableSchema.SchemaName ->
            let table =
                match schema.Objects |> Map.tryFind create.TableName.ObjectName with
                | None -> failAt create.TableName.Source <| sprintf "No such table: ``%O``" create.TableName
                | Some (SchemaTable table) -> table
                | Some _ -> failAt create.TableName.Source <| sprintf "Not a table: ``%O``" create.TableName
            if schema.ContainsObject(create.IndexName.ObjectName) then
                failAt create.IndexName.Source <| sprintf "Object already exists: ``%O``" create.IndexName
            let index =
                {   SchemaName = schema.SchemaName
                    TableName = table.TableName
                    IndexName = create.IndexName.ObjectName
                    Columns = create.IndexedColumns |> Seq.map fst |> Set.ofSeq
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
            failAt create.IndexName.Source <| sprintf "Can't create index in a different schema from its table"
        | None, Some _ ->
            failAt create.IndexName.Source <| sprintf "No such schema for index: ``%O``" create.IndexName
        | _ ->
            failAt create.TableName.Source <| sprintf "No such schema for table: ``%O``" create.TableName
            
    member this.Stmt(stmt : InfStmt) =
        match stmt with
        | AlterTableStmt alter -> this.AlterTable(alter)
        | CreateTableStmt create -> this.CreateTable(create)
        | CreateViewStmt create -> this.CreateView(create)
        | CreateIndexStmt create -> this.CreateIndex(create)
        | DropObjectStmt drop -> this.DropObject(drop)
        | BeginStmt
        | CommitStmt
        | DeleteStmt _
        | InsertStmt _
        | RollbackStmt
        | SelectStmt _
        | UpdateStmt _ -> None

