namespace Rezoom.SQL.Compiler.Postgres
open System
open System.Collections.Generic
open System.Data
open System.Configuration
open System.Data.SqlClient
open System.Data.Common
open System.Globalization
open System.Text.RegularExpressions
open Rezoom.SQL.Compiler
open Rezoom.SQL.Compiler.BackendUtilities
open Rezoom.SQL.Compiler.Translators
open Rezoom.SQL.Mapping
open Rezoom.SQL.Migrations

module private PostgresFunctions =
    open Rezoom.SQL.Compiler.FunctionDeclarations
    let functions =
        [|  // aggregate functions
            aggregate "avg" [ numeric a' ] (nullable a')
            aggregateW "count" [ scalar ] int64
            aggregate "max" [ a' ] (nullable a')
            aggregate "min" [ a' ] (nullable a')
            aggregate "sum" [ numeric a' ] a'
        |] |> DefaultFunctions.extendedBy

type private PostgresLiteral() =
    inherit DefaultLiteralTranslator()
    override __.BlobLiteral(bytes) =
        let hexPairs = bytes |> Array.map (fun b -> b.ToString("X2", CultureInfo.InvariantCulture))
        @"E'\\x" + String.Concat(hexPairs) + "'" |> text
    override __.DateTimeLiteral(dt) =
        failwith "not implemented" // TODO
    override __.DateTimeOffsetLiteral(dt) =
        failwith "not implemented" // TODO
    override __.StringLiteral(str) =
        CommandText <| "'" + str.Replace("'", "''") + "'"

type private PostgresExpression(statement : StatementTranslator, indexer) =
    inherit DefaultExprTranslator(statement, indexer)
    let literal = PostgresLiteral()
    override __.Literal = upcast literal
    override __.Name(name) =
        "\"" + name.Value.ToLowerInvariant().Replace("\"", "\"\"") + "\""
        |> text
    override __.TypeName(name) =
        (Seq.singleton << text) <|
            match name with
            | BooleanTypeName -> "BOOLEAN"
            | GuidTypeName -> "UUID"
            | IntegerTypeName Integer8 // not really supported, comes out as a smallint
            | IntegerTypeName Integer16 -> "SMALLINT"
            | IntegerTypeName Integer32 -> "INT"
            | IntegerTypeName Integer64 -> "BIGINT"
            | FloatTypeName Float32 -> "FLOAT4"
            | FloatTypeName Float64 -> "FLOAT8"
            | StringTypeName(Some len) -> "VARCHAR(" + string len + ")"
            | StringTypeName(None) -> "TEXT"
            | BinaryTypeName(Some _)
            | BinaryTypeName(None) -> "BYTEA"
            | DecimalTypeName -> "NUMERIC(38, 19)"
            | DateTimeTypeName -> "TIMESTAMP"
            | DateTimeOffsetTypeName -> "TIMESTAMP WITH TIME ZONE"
    override this.ObjectName name =
        seq {
            match name.SchemaName with
            // can't schema-qualify temp tables since they are created in a special schema
            // with a name generated per-connection
            | Some schema when schema <> Name("temp") ->
                yield this.Name(schema)
                yield text "."
                yield this.Name(name.ObjectName) 
            | _ -> yield this.Name(name.ObjectName) 
        }
    override __.BinaryOperator(op) =
        CommandText <|
        match op with
        | Concatenate -> "||"
        | Multiply -> "*"
        | Divide -> "/"
        | Modulo -> "%"
        | Add -> "+"
        | Subtract -> "-"
        | BitAnd -> "&"
        | BitOr -> "|"
        | LessThan -> "<"
        | LessThanOrEqual -> "<="
        | GreaterThan -> ">"
        | GreaterThanOrEqual -> ">="
        | Equal -> "="
        | NotEqual -> "<>"
        | And -> "AND"
        | Or -> "OR"
        | Is -> "IS NOT DISTINCT FROM"
        | IsNot -> "IS DISTINCT FROM"
        | BitShiftLeft -> "<<"
        | BitShiftRight -> ">>"
    override __.SimilarityOperator op =
        CommandText <|
        match op with
        | Like -> "LIKE"
        | Match -> "SIMILAR TO"
        | Regexp -> "~"

type private PostgresStatement(indexer : IParameterIndexer) as this =
    inherit DefaultStatementTranslator(Name("POSTGRES"), indexer)
    let expr = PostgresExpression(this :> StatementTranslator, indexer)
    override __.Expr = upcast expr
    override __.ColumnsNullableByDefault = true
    // TODO: figure out PG quirks