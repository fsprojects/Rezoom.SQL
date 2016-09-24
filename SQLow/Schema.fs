namespace SQLow
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

type ArgumentType =
    | ArgumentConcrete of ColumnType
    | ArgumentTypeVariable of Name

type FunctionType =
    {
        FixedArguments : ArgumentType IReadOnlyList
        VariableArgument : ArgumentType option
        Output : ArgumentType
        AllowWildcard : bool
        AllowDistinct : bool
        Aggregate : bool
    }

type DatabaseBuiltin =
    {
        Functions : Map<Name, FunctionType>
    }

type Model =
    {
        Schemas : Map<Name, Schema>
        DefaultSchema : Name
        TemporarySchema : Name
        Builtin : DatabaseBuiltin
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
        Columns : SchemaColumn IReadOnlyList
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
        Columns : SchemaColumn IReadOnlyList
        ReferencedTables : SchemaTable IReadOnlyList
    }