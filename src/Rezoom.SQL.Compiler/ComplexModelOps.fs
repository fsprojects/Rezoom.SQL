/// These model operations build on the primitives in ModelOps with awareness of higher level SQL statements.
module Rezoom.SQL.Compiler.ComplexModelOps

/// Converts an AST column constraint to a schema constraint type.
let columnConstraintType (colConstraint : ColumnConstraint<'t, 'e>) =
    stateful {
        let! model = State.get
        return
            match colConstraint.ColumnConstraintType with
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

/// Converts an AST column constraint to a schema constraint type and set of encompassed columns.
let tableConstraint (tblConstraint : TableConstraint<'t, 'e>) =
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

/// Qualifies an object name, depending on whether we are operating in a temporary context (CREATE TEMP) or not.
let qualifyTemp (temp : bool) (objName : ObjectName<'t>)=
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

/// Qualifies an object name, assuming we are not operating in a temporary context.
let qualify objName = qualifyTemp false objName

/// Adds a column def to a table.
let addColumnDef tableName (column : ColumnDef<'t, 'e> WithSource) =
    stateful {
        let columnName = { Source = column.Source; Value = column.Value.Name }
        do! ModelOps.addTableColumn tableName columnName column.Value.Type column.Value.Nullable
        for constr in column.Value.Constraints do
            let! constraintType = columnConstraintType constr
            // more specific source info here?
            let constraintName = { Source = column.Source; Value = constr.Name }
            let cols = Set.singleton column.Value.Name
            do! ModelOps.addConstraint tableName constraintName constraintType cols
    }

let createTableByDefinition tableName (def : CreateTableDefinition<'t, 'e>) =
    stateful {
        do! ModelOps.createEmptyTable tableName
        for column in def.Columns do
            do! addColumnDef tableName column
        for constr in def.Constraints do
            let! constraintType, cols = tableConstraint constr.Value
            let constraintName = constr.Map(fun c -> c.Name)
            do! ModelOps.addConstraint tableName constraintName constraintType cols
    }

let createTableByQuery tableName (query : ColumnType QueryExprInfo) =
    stateful {
        do! ModelOps.createEmptyTable tableName
        for column in query.Columns do
            let ty = column.Expr.Info.Type
            let typeName = ty.Type.ApproximateTypeName()
            let columnName = { Source = column.Expr.Source; Value = column.ColumnName }
            do! ModelOps.addTableColumn tableName columnName typeName ty.Nullable
    }

let dropColumn tableName (column : Name) =
    stateful {
        let! table = ModelOps.getRequiredTable tableName
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
                    let! referencingTable = ModelOps.getRequiredTable (artificialSource rfk.FromTable)
                    let referencingConstr = table.Constraints |> Map.find rfk.FromConstraint
                    match referencingConstr.ConstraintType with
                    | ForeignKeyConstraintType fk ->
                        if fk.ToColumns |> Set.contains column then
                            let refName =
                                string referencingTable.Name + "." + string referencingConstr.ConstraintName
                            failAt tableName.Source <| Error.columnIsReferencedByConstraints column [refName]
                    | _ -> ()
                let table = { table with Columns = table.Columns |> Map.remove column }
                return! ModelOps.putObject tableName (SchemaTable table)
            else
                failAt tableName.Source <| Error.columnIsReferencedByConstraints column coveredByConstraints
    }