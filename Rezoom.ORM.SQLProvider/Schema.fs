namespace Rezoom.ORM.SQLProvider
open SQLow
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

type Model =
    {
        Schemas : Map<Name, Schema>
        DefaultSchema : Name
        TemporarySchema : Name
    }

and Schema =
    {
        SchemaName : Name
        Tables : Map<Name, SchemaTable>
        Views : Map<Name, SchemaView>
    }

and SchemaTable =
    {
        SchemaName : Name
        TableName : Name
        Columns : IReadOnlyList<SchemaColumn>
    }


and SchemaColumn =
    {
        SchemaName : Name
        TableName : Name
        ColumnName : Name
        /// True if this column is part of the table's primary key.
        PrimaryKey : bool
        ColumnType : ColumnType
    }


and SchemaView =
    {
        SchemaName : Name
        ViewName : Name
        Query : SchemaQuery
    }

and SchemaQuery =
    {
        Columns : IReadOnlyList<SchemaQueryColumn>
        ReferencedTables : SchemaTable seq
    }

and SchemaQueryColumn =
    {
        ColumnName : Name
        ColumnType : ColumnType
    }