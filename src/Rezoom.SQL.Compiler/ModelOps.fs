/// Fundamental operations on the model.
/// The model should only be modified (it's immutable, so by modified I mean creation of altered copies)
/// via these primitives. This will ensure invariants like that every foreign key constraint has a reverse foreign
/// key tracking it in the referenced table.
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

let getRequiredView name =
    getRequiredObject "view" name
    |> State.map (function
        | SchemaView v -> v
        | _ -> failAt name.Source <| Error.objectIsNotA "view" name.Value)

let getRequiredIndex name =
    getRequiredObject "index" name
    |> State.map (function
        | SchemaIndex i -> i
        | _ -> failAt name.Source <| Error.objectIsNotA "index" name.Value)

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
                {   Name = tableName.Value
                    Columns = Map.empty
                    Indexes = Map.empty
                    Constraints = Map.empty
                    ReverseForeignKeys = Set.empty
                }
            return! putObject tableName (SchemaTable table)
    }

/// Add a column to an existing table.
let addTableColumn
    (tableName : QualifiedObjectName WithSource)
    (columnName : Name WithSource)
    (columnTypeName : TypeName)
    (columnNullable : bool) =
    stateful {
        let! table = getRequiredTable tableName
        match table.Columns |> Map.tryFind columnName.Value with
        | None ->
            let column =
                {   SchemaName = tableName.Value.SchemaName
                    TableName = tableName.Value.ObjectName
                    ColumnName = columnName.Value
                    ColumnType = ColumnType.OfTypeName(columnTypeName, columnNullable)
                    ColumnTypeName = columnTypeName
                    PrimaryKey = false
                    DefaultConstraintName = None
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

/// Add a table constraint.
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
                | DefaultConstraintType ->
                    let name = Some constraintName.Value
                    let columns =
                        cols
                        |> Seq.map (fun c -> { Map.find c table.Columns with DefaultConstraintName = name })
                    { table with Columns = table.Columns |> replaceMany columns (fun c -> c.ColumnName) }
                | ForeignKeyConstraintType _
                | CheckConstraintType
                | CollateConstraintType
                | UniqueConstraintType -> table
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
let createIndex (tableName : QualifiedObjectName WithSource) (indexName : QualifiedObjectName WithSource) cols =
    stateful {
        if indexName.Value.SchemaName <> tableName.Value.SchemaName then
            failAt indexName.Source <| Error.indexSchemasMismatch indexName.Value tableName.Value
        let! existing = getObject indexName.Value
        match existing with
        | Some _ ->
            failAt indexName.Source <| Error.objectAlreadyExists indexName.Value
        | None ->
            let! table = getRequiredTable tableName
            match table.Indexes |> Map.tryFind indexName.Value.ObjectName with
            | None ->
                let index =
                    {   SchemaName = tableName.Value.SchemaName
                        TableName = tableName.Value.ObjectName
                        IndexName = indexName.Value.ObjectName
                        Columns = cols
                    }
                let table = { table with Indexes = table.Indexes |> Map.add indexName.Value.ObjectName index }
                do! putObject indexName (SchemaIndex index)
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
                {   Name = { SchemaName = oldTable.SchemaName; ObjectName = tn }
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

let dropColumn tableName (column : Name) =
    stateful {
        let! table = getRequiredTable tableName
        match table.Columns |> Map.tryFind column with
        | None ->
            // IMPROVEMENT oughta have better source location
            failAt tableName.Source <| Error.noSuchColumn column
        | Some _ ->
            let coveredByConstraints =
                table.Constraints
                |> Seq.filter (function KeyValue(_, constr) -> constr.Columns |> Set.contains column)
                |> Seq.map (function KeyValue(_, constr) -> constr.ConstraintName)
                |> Seq.cache
            if Seq.isEmpty coveredByConstraints then
                for rfk in table.ReverseForeignKeys do
                    let! referencingTable = getRequiredTable (artificialSource rfk.FromTable)
                    let referencingConstr = referencingTable.Constraints |> Map.find rfk.FromConstraint
                    match referencingConstr.ConstraintType with
                    | ForeignKeyConstraintType fk ->
                        if fk.ToColumns |> Set.contains column then
                            let refName =
                                string referencingTable.Name + "." + string referencingConstr.ConstraintName
                            failAt tableName.Source <| Error.columnIsReferencedByConstraints column [refName]
                    | _ -> ()
                let table = { table with Columns = table.Columns |> Map.remove column }
                return! putObject tableName (SchemaTable table)
            else
                failAt tableName.Source <| Error.columnIsReferencedByConstraints column coveredByConstraints
    }

/// Remove an existing table from the model.
/// This handles checking for references to the table, and removing reverse references.
let dropTable (tableName : QualifiedObjectName WithSource) =
    stateful {
        let! tbl = getRequiredTable tableName
        let referencingTables = tbl.ReverseForeignKeys |> Set.map (fun fk -> fk.FromTable)
        if Set.isEmpty referencingTables then
            for constr in tbl.Constraints do
                match constr.Value.ConstraintType with
                | ForeignKeyConstraintType fk -> // remove reverse foreign keys from target table
                    let targetTableName = artificialSource fk.ToTable
                    let! targetTable = getRequiredTable targetTableName
                    let reverseKeys =
                        targetTable.ReverseForeignKeys
                        |> Set.filter (fun r -> r.FromTable <> tableName.Value)
                    do! putObject targetTableName (SchemaTable { targetTable with ReverseForeignKeys = reverseKeys })
                | _ -> ()
            return! removeObject tableName
        else
            failAt tableName.Source <| Error.tableIsReferencedByFKs tableName.Value referencingTables
    }

/// Remove an existing view from the model.
let dropView (viewName : QualifiedObjectName WithSource) =
    stateful {
        let! _ = getRequiredView viewName // ensure it exists
        return! removeObject viewName
    }

/// Remove an existing index from the model.
let dropIndex (indexName : QualifiedObjectName WithSource) =
    stateful {
        let! index = getRequiredIndex indexName
        let tableName = artificialSource { SchemaName = index.SchemaName; ObjectName = index.TableName }
        let! table = getRequiredTable tableName
        let table = { table with Indexes = table.Indexes |> Map.remove index.IndexName }
        do! putObject tableName (SchemaTable table)
        return! removeObject indexName
    }

/// Remove an existing table constraint from the mode.
let dropConstraint (tableName : QualifiedObjectName WithSource) (constraintName : Name WithSource) =
    stateful {
        let! table = getRequiredTable tableName
        match table.Constraints |> Map.tryFind constraintName.Value with
        | None -> failAt constraintName.Source <| Error.noSuchConstraint tableName.Value constraintName.Value
        | Some constr ->
            let table = { table with Constraints = table.Constraints |> Map.remove constraintName.Value }
            do! putObject tableName (SchemaTable table)
            match constr.ConstraintType with
            | ForeignKeyConstraintType fk ->
                // go remove reverse FK from targeted table
                let targetTableName = artificialSource fk.ToTable
                let! targetTable = getRequiredTable targetTableName
                let reverseForeignKeys =
                    targetTable.ReverseForeignKeys
                    |> Set.filter (fun r -> r.FromTable <> tableName.Value || r.FromConstraint <> constraintName.Value)
                let targetTable = { targetTable with ReverseForeignKeys = reverseForeignKeys }
                return! putObject targetTableName (SchemaTable targetTable)
            | PrimaryKeyConstraintType _ ->
                // remove PK attribute from columns
                let unPKed = constr.Columns |> Seq.map (fun c -> { Map.find c table.Columns with PrimaryKey = false })
                let table = { table with Columns = table.Columns |> replaceMany unPKed (fun c -> c.ColumnName) }
                return! putObject tableName (SchemaTable table)
            | DefaultConstraintType ->
                // make columns not have a default anymore
                let unDefaulted =
                    constr.Columns
                    |> Seq.map (fun c -> { Map.find c table.Columns with DefaultConstraintName = None })
                let table = { table with Columns = table.Columns |> replaceMany unDefaulted (fun c -> c.ColumnName) }
                return! putObject tableName (SchemaTable table)
            | _ -> ()
    }

/// Change a column's type.
let changeColumnType tableName (columnName : Name WithSource) newType =
    stateful {
        let! table = getRequiredTable tableName
        match table.Columns |> Map.tryFind columnName.Value with
        | None ->
            failAt columnName.Source <| Error.noSuchColumn columnName.Value
        | Some col ->
            if col.ColumnTypeName = newType then
                failAt columnName.Source <| Error.columnTypeIsAlready columnName.Value newType
            else
                // FUTURE validate that referencing FKs have compatible type? default value has compatible type?
                let newColumn =
                    { col with
                        ColumnType = ColumnType.OfTypeName(newType, col.ColumnType.Nullable)
                        ColumnTypeName = newType
                    }
                let table = { table with Columns = table.Columns |> Map.add columnName.Value newColumn }
                return! putObject tableName (SchemaTable table)
    }

let changeColumnNullability tableName (columnName : Name WithSource) newNullable =
    stateful {
        let! table = getRequiredTable tableName
        match table.Columns |> Map.tryFind columnName.Value with
        | None ->
            failAt columnName.Source <| Error.noSuchColumn columnName.Value
        | Some col ->
            if col.ColumnType.Nullable = newNullable then
                failAt columnName.Source <| Error.columnNullabilityIsAlready columnName.Value newNullable
            else
                let newColumn =
                    { col with ColumnType = { col.ColumnType with Nullable = newNullable } }
                let table = { table with Columns = table.Columns |> Map.add columnName.Value newColumn }
                return! putObject tableName (SchemaTable table)
    }