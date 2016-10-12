namespace StaticQL
open System
open System.Collections.Generic

type CoreColumnType =
    | AnyType
    | BooleanType
    | StringType
    | IntegerType of IntegerSize
    | FloatType of FloatSize
    | DecimalType
    | BinaryType
    | DateTimeType
    | DateTimeOffsetType
    static member OfTypeName(typeName : TypeName) =
        match typeName with
        | StringTypeName _ -> StringType
        | BinaryTypeName _ -> BinaryType
        | IntegerTypeName sz -> IntegerType sz
        | FloatTypeName sz -> FloatType sz
        | DecimalTypeName -> DecimalType
        | BooleanTypeName -> BooleanType
        | DateTimeTypeName -> DateTimeType
        | DateTimeOffsetTypeName -> DateTimeOffsetType

type ColumnType =
    {   Type : CoreColumnType
        Nullable : bool
    }
    static member OfTypeName(typeName : TypeName, nullable) =
        {   Type = CoreColumnType.OfTypeName(typeName)
            Nullable = nullable
        }
    member ty.CLRType =
        match ty.Type with
        | IntegerType Integer8 -> if ty.Nullable then typeof<Nullable<sbyte>> else typeof<sbyte>
        | IntegerType Integer16 -> if ty.Nullable then typeof<Nullable<int16>> else typeof<int16>
        | IntegerType Integer32 -> if ty.Nullable then typeof<Nullable<int32>> else typeof<int32>
        | IntegerType Integer64 -> if ty.Nullable then typeof<Nullable<int64>> else typeof<int64>
        | FloatType Float32 -> if ty.Nullable then typeof<Nullable<single>> else typeof<single>
        | FloatType Float64 -> if ty.Nullable then typeof<Nullable<double>> else typeof<double>
        | BooleanType -> if ty.Nullable then typeof<Nullable<bool>> else typeof<bool>
        | DecimalType -> if ty.Nullable then typeof<Nullable<decimal>> else typeof<decimal>
        | DateTimeType -> if ty.Nullable then typeof<Nullable<DateTime>> else typeof<DateTime>
        | DateTimeOffsetType -> if ty.Nullable then typeof<Nullable<DateTimeOffset>> else typeof<DateTimeOffset>
        | StringType -> typeof<string>
        | BinaryType -> typeof<byte array>
        | AnyType -> typeof<obj>

type ArgumentType =
    | ArgumentConcrete of ColumnType
    | ArgumentTypeVariable of Name

type FunctionType =
    {   FixedArguments : ArgumentType IReadOnlyList
        VariableArgument : ArgumentType option
        Output : ArgumentType
        AllowWildcard : bool
        AllowDistinct : bool
        Aggregate : bool
    }

type DatabaseBuiltin =
    {   Functions : Map<Name, FunctionType>
    }

type Model =
    {   Schemas : Map<Name, Schema>
        DefaultSchema : Name
        TemporarySchema : Name
        Builtin : DatabaseBuiltin
    }

and Schema =
    {   SchemaName : Name
        Tables : Map<Name, SchemaTable>
        Views : Map<Name, SchemaView>
    }
    member this.ContainsObject(name : Name) =
        this.Tables.ContainsKey(name)
        || this.Views.ContainsKey(name)

and SchemaTable =
    {   SchemaName : Name
        TableName : Name
        Columns : SchemaColumn Set
    }
    static member OfCreateDefinition(schemaName, tableName, def : CreateTableDefinition<_, _>) =
        let tablePkColumns =
            seq {
                for constr in def.Constraints do
                    match constr.TableConstraintType with
                    | TableIndexConstraint { Type = PrimaryKey; IndexedColumns = indexed } ->
                        for expr, _ in indexed do
                            match expr.Value with
                            | ColumnNameExpr name -> yield name.ColumnName
                            | _ -> ()
                    | _ -> ()
            } |> Set.ofSeq
        let tableColumns =
            seq {
                for column in def.Columns ->
                let hasNotNullConstraint =
                    column.Constraints
                    |> Seq.exists(function | { ColumnConstraintType = NotNullConstraint _ } -> true | _ -> false)
                let isPrimaryKey =
                    tablePkColumns.Contains(column.Name)
                    || column.Constraints |> Seq.exists(function
                        | { ColumnConstraintType = PrimaryKeyConstraint _ } -> true
                        | _ -> false)
                {   SchemaName = schemaName
                    TableName = tableName
                    PrimaryKey = isPrimaryKey
                    ColumnName = column.Name
                    ColumnType = ColumnType.OfTypeName(column.Type, not hasNotNullConstraint)
                }
            }
        {   SchemaName = schemaName
            TableName = tableName
            Columns = tableColumns |> Set.ofSeq
        }

and SchemaColumn =
    {   SchemaName : Name
        TableName : Name
        ColumnName : Name
        /// True if this column is part of the table's primary key.
        PrimaryKey : bool
        ColumnType : ColumnType
    }


and SchemaView =
    {   SchemaName : Name
        ViewName : Name
        Columns : SchemaColumn Set
        ReferencedTables : SchemaTable Set
    }