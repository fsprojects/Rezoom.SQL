namespace Rezoom.ORM.SQLProvider
open System
open System.Collections.Generic

type CoreColumnType =
    | AnyType
    | BooleanType
    | StringType
    | IntegerType
    | FloatType
    | BlobType

type ColumnType =
    {
        Type : CoreColumnType
        Nullable : bool
    }

type IModel =
    abstract member Schemas : IReadOnlyDictionary<string, ISchema>
    abstract member DefaultSchema : string

and ISchema =
    abstract member SchemaName : string
    abstract member Tables : IReadOnlyDictionary<string, ISchemaTable>
    abstract member Views : IReadOnlyDictionary<string, ISchemaView>

and ISchemaTable =
    abstract member Schema : ISchema
    abstract member TableName : string
    abstract member Columns : IReadOnlyList<ISchemaColumn>
    abstract member ColumnsByName : IReadOnlyDictionary<string, ISchemaColumn>
    /// Equivalent to TableQuery(this).
    abstract member Query : ISchemaQuery

and ISchemaColumn =
    abstract member Table : ISchemaTable
    /// True if this column is part of the table's primary key.
    abstract member PrimaryKey : bool
    abstract member ColumnName : string
    abstract member ColumnType : ColumnType

and ISchemaView =
    abstract member Schema : ISchema
    abstract member ViewName : string
    abstract member Query : ISchemaQuery

and ISchemaQuery =
    abstract member Columns : IReadOnlyList<ISchemaQueryColumn>
    abstract member ColumnsByName : IReadOnlyDictionary<string, ISchemaQueryColumn>
    abstract member ReferencedTables : ISchemaTable seq

and ISchemaQueryColumn =
    abstract member Query : ISchemaQuery
    abstract member ColumnName : string
    abstract member ColumnType : ColumnType
    /// If this column is pulled straight from a table, this is the referenced column.
    /// Useful for determining identity.
    abstract member SourceColumn : ISchemaColumn option

type TableQuery(table : ISchemaTable) as query =
    let columns =
        [| for col in table.Columns ->
            { new ISchemaQueryColumn with
                member __.Query = query :> ISchemaQuery
                member __.ColumnName = col.ColumnName
                member __.ColumnType = col.ColumnType
                member __.SourceColumn = Some col
            }
        |]
    let byName = columns |> ciDictBy (fun c -> c.ColumnName)
    interface ISchemaQuery with
        member __.Columns = upcast columns
        member __.ColumnsByName = byName
        member __.ReferencedTables = Seq.singleton table