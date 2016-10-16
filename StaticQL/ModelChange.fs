namespace StaticQL
open System
open System.Collections.Generic
open StaticQL.InferredTypes

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
                if create.IfNotExists then None
                else failAt create.Name.Source <| sprintf "Table ``%O`` already exists" create.Name
            | Some _ -> failAt create.Name.Source <| sprintf "Object ``%O`` already exists" create.Name
            | None ->
                let table =
                    match create.As with
                    | CreateAsSelect select ->
                        {   SchemaName = schema.SchemaName
                            TableName = create.Name.ObjectName
                            Columns =
                                this.CreateTableColumns(model, schema.SchemaName, tableName, select) |> Set.ofSeq
                        }
                    | CreateAsDefinition def ->
                        SchemaTable.OfCreateDefinition(schema.SchemaName, tableName, def)
                let schema =
                    { schema with Objects = schema.Objects |> Map.add table.TableName (SchemaTable table) }
                Some { model with Schemas = model.Schemas |> Map.add schema.SchemaName schema }
    member this.AlterTable(alter : InfAlterTableStmt) =
        let schemaName = alter.Table.SchemaName |? model.DefaultSchema
        match model.Schemas |> Map.tryFind schemaName with
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
                    let newTbl = tbl.WithAdditionalColumn(col) |> resultAt alter.Table.Source |> SchemaTable
                    let schema = { schema with Objects = schema.Objects |> Map.add tbl.TableName newTbl }
                    Some { model with Schemas = model.Schemas |> Map.add schema.SchemaName schema }
            | Some _ -> failAt alter.Table.Source <| sprintf "Not a table: ``%O``" alter.Table
    member this.CreateView(create : InfCreateViewStmt) =
        let schemaName = create.ViewName.SchemaName |? model.DefaultSchema
        let viewName = create.ViewName.ObjectName
        match model.Schemas |> Map.tryFind schemaName with
        | None -> failAt create.ViewName.Source <| sprintf "No such schema: ``%O``" create.ViewName
        | Some schema ->
            match schema.Objects |> Map.tryFind viewName with
            | Some (SchemaView _) ->
                if create.IfNotExists then None else
                failAt create.ViewName.Source <| sprintf "View already exists: ``%O``" create.ViewName
            | Some _ ->
                failAt create.ViewName.Source <| sprintf "Object already exists: ``%O``" create.ViewName
            | None ->
                let view =
                    {   SchemaName = schema.SchemaName
                        ViewName = viewName
                        Columns =
                            seq {
                                for column in create.AsSelect.Value.Info.Columns ->
                                    {   SchemaName = schemaName
                                        TableName = viewName
                                        ColumnName = column.ColumnName
                                        PrimaryKey = column.Expr.Info.PrimaryKey
                                        ColumnType = inference.Concrete(column.Expr.Info.Type)
                                    }
                            } |> Set.ofSeq
                        ReferencedTables = Set.empty // TODO
                    } |> SchemaView
                let schema = { schema with Objects = schema.Objects |> Map.add create.ViewName.ObjectName view }
                Some { model with Schemas = model.Schemas |> Map.add schema.SchemaName schema }
    member this.DropObject(drop : InfDropObjectStmt) =
        let schemaName = drop.ObjectName.SchemaName |? model.DefaultSchema
        let objName = drop.ObjectName.ObjectName
        let typeName =
            match drop.Drop with
            | DropTable -> "table"
            | DropView -> "view"
            | DropIndex -> "index"
        match model.Schemas |> Map.tryFind schemaName with
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
                | DropView, SchemaView _
                | DropIndex, SchemaIndex _ ->
                    dropped()
                | _ ->
                    failAt drop.ObjectName.Source <| sprintf "Not a %s: ``%O``" typeName objName
            
    member this.Statement(stmt : InfStmt) =
        match stmt with
        | AlterTableStmt alter -> this.AlterTable(alter)
        | CreateTableStmt create -> this.CreateTable(create)
        | CreateViewStmt create -> this.CreateView(create)
        | CreateIndexStmt create -> failwith "not implemented"
        | DropObjectStmt drop -> this.DropObject(drop)
        | BeginStmt
        | CommitStmt
        | DeleteStmt _
        | InsertStmt _
        | RollbackStmt
        | SelectStmt _
        | UpdateStmt _ -> None

