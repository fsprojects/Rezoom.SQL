namespace StaticQL
open System
open System.Data
open System.Collections.Generic
open StaticQL.Mapping
open StaticQL
open FSharp.Quotations

type IParameterIndexer =
    abstract member ParameterIndex : parameter : BindParameter -> int

type ParameterTransform =
    {   ParameterType : DbType
        ValueTransform : Quotations.Expr -> Quotations.Expr
    }
    static member Default(columnType : ColumnType) =
        let transform (expr : Quotations.Expr) =
            let asObj = Expr.Coerce(expr, typeof<obj>)
            <@@ if %%asObj = null then box DBNull.Value else %%asObj @@>
        let ty =
            match columnType.Type with
            | IntegerType Integer8 -> DbType.SByte
            | IntegerType Integer16 -> DbType.Int16
            | IntegerType Integer32 -> DbType.Int32
            | IntegerType Integer64 -> DbType.Int64
            | FloatType Float32 -> DbType.Single
            | FloatType Float64 -> DbType.Double
            | BooleanType -> DbType.Boolean
            | DecimalType -> DbType.Decimal
            | DateTimeType -> DbType.DateTime
            | DateTimeOffsetType -> DbType.DateTimeOffset
            | StringType -> DbType.String
            | BinaryType -> DbType.Binary
            | AnyType -> DbType.Object
        {   ParameterType = ty
            ValueTransform = transform
        }

type IBackend =
    abstract member InitialModel : Model
    abstract member ParameterTransform
        : columnType : ColumnType -> ParameterTransform
    abstract member ToCommandFragments
        : indexer : IParameterIndexer * stmts : TStmt IReadOnlyList -> CommandFragment IReadOnlyList
