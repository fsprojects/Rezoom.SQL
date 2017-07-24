/// These model operations build on the primitives in ModelOps with awareness of higher level SQL statements.
module Rezoom.SQL.Compiler.ComplexModelOps

/// Converts an AST column constraint to a schema constraint type.
let columnConstraintType (colConstraint : ColumnConstraint<'t, 'e>) =
    stateful {
        let! model = State.get
        return
            match colConstraint.ColumnConstraintType with
            | PrimaryKeyConstraint pk -> PrimaryKeyConstraintType pk.AutoIncrement
            | ForeignKeyConstraint fk ->
                let toSchema = fk.ReferencesTable.SchemaName |? model.DefaultSchema
                {   ToTable = { SchemaName = toSchema; ObjectName = fk.ReferencesTable.ObjectName }
                    ToColumns = fk.ReferencesColumns |> Seq.map (fun c -> c.Value) |> Set.ofSeq
                    OnDelete = fk.OnDelete
                } |> ForeignKeyConstraintType
            | UniqueConstraint -> UniqueConstraintType
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
                | Unique -> UniqueConstraintType, cols
            | TableForeignKeyConstraint (names, fk) ->
                let cols = names |> Seq.map (fun c -> c.Value) |> Set.ofSeq
                let toSchema = fk.ReferencesTable.SchemaName |? model.DefaultSchema
                {   ToTable = { SchemaName = toSchema; ObjectName = fk.ReferencesTable.ObjectName }
                    ToColumns = fk.ReferencesColumns |> Seq.map (fun c -> c.Value) |> Set.ofSeq
                    OnDelete = fk.OnDelete
                } |> ForeignKeyConstraintType, cols
            | TableCheckConstraint _ ->
                CheckConstraintType, Set.empty
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

let addTableConstraint tableName (constr : TableConstraint<'t, 'e> WithSource) =
    stateful {
        let! constraintType, cols = tableConstraint constr.Value
        let constraintName = constr.Map(fun c -> c.Name)
        do! ModelOps.addConstraint tableName constraintName constraintType cols
    }

/// Adds a column def to a table.
let addColumnDef tableName (column : ColumnDef<'t, 'e> WithSource) =
    stateful {
        let columnName = { Source = column.Source; Value = column.Value.Name }
        do! ModelOps.addTableColumn tableName columnName column.Value.Type column.Value.Nullable
        for constr in column.Value.Constraints do
            let! constraintType = columnConstraintType constr
            // more specific source info here?
            let constraintName = constr.Name |> nearSourceOf column
            let cols = Set.singleton column.Value.Name
            do! ModelOps.addConstraint tableName constraintName constraintType cols
    }

let createTableByDefinition tableName (def : CreateTableDefinition<'t, 'e>) =
    stateful {
        do! ModelOps.createEmptyTable tableName
        for column in def.Columns do
            do! addColumnDef tableName column
        for constr in def.Constraints do
            do! addTableConstraint tableName constr
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

let addColumnDefault (tableName : QualifiedObjectName WithSource) columnName =
    stateful {
        let cols = Set.singleton columnName
        let constraintName =
            ColumnConstraintType<unit, unit>.DefaultConstraintName(columnName) |> nearSourceOf tableName
        return! ModelOps.addConstraint tableName constraintName DefaultConstraintType cols
    }

let dropColumnDefault (tableName : QualifiedObjectName WithSource) columnName =
    stateful {
        let! column = ModelOps.getRequiredColumn tableName columnName
        match column.DefaultConstraintName with
        | None -> failAt columnName.Source <| Error.noDefaultConstraintToDrop tableName.Value columnName.Value
        | Some defaultConstraintName ->
            return! ModelOps.dropConstraint tableName (artificialSource defaultConstraintName)
    }