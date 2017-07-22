module Rezoom.SQL.Compiler.ModelOps

let getSchema (name : Name) =
    stateful {
        let! model = State.get
        return model.Schemas |> Map.tryFind name
    }

let getObject (name : QualifiedObjectName) =
    stateful {
        let! schema = getSchema name.SchemaName
        match schema with
        | None -> return None
        | Some schema ->
            return schema.Objects |> Map.tryFind name.ObjectName
    }

let getRequiredObject objectTypeName (name : QualifiedObjectName WithSource) =
    stateful {
        let! schema = getSchema name.Value.SchemaName
        match schema with
        | None -> return failAt name.Source <| Error.noSuchSchema name.Value.SchemaName
        | Some schema ->
            return
                match schema.Objects |> Map.tryFind name.Value.ObjectName with
                | None -> failAt name.Source <| Error.noSuchObject objectTypeName name.Value.ObjectName
                | Some obj -> obj
    }

let getRequiredTable name =
    getRequiredObject "table" name
    |> State.map (function
        | SchemaTable t -> t
        | _ -> failAt name.Source <| Error.objectIsNotA "table" name.Value)

let getRequiredColumn tableName (columnName : Name WithSource) =
    getRequiredTable tableName
    |> State.map (fun tbl ->
        match tbl.Columns |> Map.tryFind columnName.Value with
        | None -> failAt columnName.Source <| Error.noSuchColumn columnName.Value
        | Some col -> col)

/// Create or update a schema within the model.
let putSchema (schema : Schema) =
    stateful {
        let! model = State.get
        let newModel = { model with Schemas = model.Schemas |> Map.add schema.SchemaName schema }
        return! State.put newModel
    }

/// Create or update an object within an existing schema in the model.
let putObject (name : QualifiedObjectName WithSource) (obj : SchemaObject) =
    stateful {
        let! schema = getSchema name.Value.SchemaName
        match schema with
        // shouldn't have called this with a bogus schema
        | None ->
            failAt name.Source <| Error.noSuchSchema name.Value.SchemaName
        | Some schema ->
            let newSchema = { schema with Objects = schema.Objects |> Map.add name.Value.ObjectName obj }
            return! putSchema newSchema
    }

/// Remove an existing object from the model.
let removeObject (name : QualifiedObjectName WithSource) =
    stateful {
        let! schema = getSchema name.Value.SchemaName
        match schema with
        // shouldn't have called this with a bogus schema
        | None ->
            failAt name.Source <| Error.noSuchSchema name.Value.SchemaName
        | Some schema ->
            let newSchema = { schema with Objects = schema.Objects |> Map.remove name.Value.ObjectName }
            return! putSchema newSchema
    }

/// Create a new table with a given name.
let createEmptyTable (tableName : QualifiedObjectName WithSource) =
    stateful {
        let! existing = getObject tableName.Value
        match existing with
        | Some _ ->
            failAt tableName.Source <| Error.objectAlreadyExists tableName.Value
        | None ->
            let table =
                {   SchemaName = tableName.Value.SchemaName
                    TableName = tableName.Value.ObjectName
                    Columns = Map.empty
                    Indexes = Map.empty
                    Constraints = Map.empty
                    ReverseForeignKeys = Set.empty
                }
            return! putObject tableName (SchemaTable table)
    }

/// Add a column to an existing table.
let addTableColumn (tableName : QualifiedObjectName WithSource) (columnName : Name WithSource) columnType =
    stateful {
        let! table = getRequiredTable tableName
        match table.Columns |> Map.tryFind columnName.Value with
        | None ->
            let column =
                {   SchemaName = tableName.Value.SchemaName
                    TableName = tableName.Value.ObjectName
                    ColumnName = columnName.Value
                    ColumnType = { Type = columnType; Nullable = false }
                    PrimaryKey = false
                }
            let table =
                { table with Columns = table.Columns |> Map.add columnName.Value column }
            return! putObject tableName (SchemaTable table)
        | Some _ ->
            failAt columnName.Source <| Error.columnAlreadyExists columnName.Value
    }

let private mapValues f map =
    Map.map (fun _ v -> f v) map

let private replaceMany xs key map =
    xs |> Seq.fold (fun m x -> Map.add (key x) x m) map

/// Add a column-scoped constraint
let addConstraint (tableName : QualifiedObjectName WithSource) (constraintName : Name WithSource) constraintType cols =
    stateful {
        let! table = getRequiredTable tableName
        match table.Constraints |> Map.tryFind constraintName.Value with
        | None ->
            let constr =
                {   SchemaName = tableName.Value.SchemaName
                    TableName = tableName.Value.ObjectName
                    ConstraintName = constraintName.Value
                    ConstraintType = constraintType
                    Columns = cols
                }
            let table =
                { table with Constraints = table.Constraints |> Map.add constraintName.Value constr }
            let table =
                match constraintType with
                | PrimaryKeyConstraintType _ ->
                    let columns = cols |> Seq.map (fun c -> { Map.find c table.Columns with PrimaryKey = true })
                    { table with Columns = table.Columns |> replaceMany columns (fun c -> c.ColumnName) }
                | ForeignKeyConstraintType _
                | DefaultConstraintType
                | NullableConstraintType
                | OtherConstraintType -> table
            do! putObject tableName (SchemaTable table)
            match constraintType with
            | ForeignKeyConstraintType fk ->
                let targetName = artificialSource fk.ToTable
                let! target = getRequiredTable targetName
                let reverse =
                    {   FromTable = tableName.Value
                        FromConstraint = constraintName.Value
                        OnDelete = fk.OnDelete
                    }
                let target =
                    { target with ReverseForeignKeys = target.ReverseForeignKeys |> Set.add reverse }
                do! putObject targetName (SchemaTable target)
            | _ -> ()
        | Some _ -> failAt constraintName.Source <| Error.constraintAlreadyExists constraintName.Value
    }

/// Create an index to a table. There must not be an existing index with the same name.
let createIndex (tableName : QualifiedObjectName WithSource) (indexName : Name WithSource) cols =
    stateful {
        let! table = getRequiredTable tableName
        match table.Indexes |> Map.tryFind indexName.Value with
        | None ->
            let index =
                {   SchemaName = tableName.Value.SchemaName
                    TableName = tableName.Value.ObjectName
                    IndexName = indexName.Value
                    Columns = cols
                }
            let table = { table with Indexes = table.Indexes |> Map.add indexName.Value index }
            return! putObject tableName (SchemaTable table)
        | Some _ -> failAt indexName.Source <| Error.indexAlreadyExists indexName.Value
    }

/// Create a view.
let createView (viewName : QualifiedObjectName WithSource) (createDefinition : CreateViewStmt) =
    stateful {
        let! existing = getObject viewName.Value
        match existing with
        | Some _ ->
            failAt viewName.Source <| Error.objectAlreadyExists viewName.Value
        | None ->
            let view =
                {   SchemaName = viewName.Value.SchemaName
                    ViewName = viewName.Value.ObjectName
                    CreateDefinition = createDefinition
                }
            return! putObject viewName (SchemaView view)
    }

/// Rename an existing table *and* update other references in the schema that point to it (child objects of the table
/// and foreign keys in other tables). Does not update source code of views, however.
let renameTable (oldName : QualifiedObjectName WithSource) (newName : QualifiedObjectName WithSource) =
    stateful {
        let! oldTable = getRequiredTable oldName
        let! existing = getObject newName.Value
        match existing with
        | Some _ ->
            failAt newName.Source <| Error.objectAlreadyExists newName.Value
        | None ->
            let tn = newName.Value.ObjectName
            let newTable =
                {   SchemaName = oldTable.SchemaName
                    TableName = tn
                    Columns = oldTable.Columns |> mapValues (fun c -> { c with TableName = tn })
                    Indexes = oldTable.Indexes |> mapValues (fun i -> { i with TableName = tn })
                    Constraints = oldTable.Constraints |> mapValues (fun c -> { c with TableName = tn })
                    ReverseForeignKeys = oldTable.ReverseForeignKeys
                }
            do! removeObject oldName
            do! putObject newName (SchemaTable newTable)
            for reverseFk in newTable.ReverseForeignKeys do
                let fromTableName = artificialSource reverseFk.FromTable
                let! fromTable = getRequiredTable fromTableName
                let fromTable =
                    let updateConstraint (constr : SchemaConstraint) =
                        match constr.ConstraintType with
                        | ForeignKeyConstraintType fk ->
                            { constr with
                                ConstraintType = ForeignKeyConstraintType { fk with ToTable = newName.Value } }
                        | _ -> constr
                    { fromTable with
                        Constraints = fromTable.Constraints |> mapValues updateConstraint
                    }
                do! putObject fromTableName (SchemaTable fromTable)

    }