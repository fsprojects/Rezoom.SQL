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
        [|  // math https://www.postgresql.org/docs/9.6/static/functions-math.html
            func "cbrt" [ infect num ] float64
            func "sqrt" [ infect num ] float64
            func "ceil" [ infect (numeric a') ] a'
            func "ceiling" [ infect (numeric a') ] a'
            func "degrees" [ infect float64 ] float64
            func "div" [ infect (numeric a'); infect a' ] a'
            func "exp" [ infect (numeric a') ] a'
            func "floor" [ infect (numeric a') ] a'
            func "ln" [ infect (numeric a') ] a'
            func "log" [ infect (numeric a') ] a'
            func "mod" [ infect (intish a'); infect a' ] a'
            func "pi" [] float64
            func "power" [ infect (fracish a'); infect a' ] a'
            func "radians" [ infect float64 ] float64
            func "round" [ infect (numeric a'); optional (infect int32) ] a'
            func "scale" [ infect decimal ] decimal
            func "sign" [ infect (numeric a') ] a'
            func "sqrt" [ infect (numeric a') ] a'
            func "trunc" [ infect (numeric a'); optional (infect int32) ] a'
            func "width_bucket" [ infect num; infect num; infect num; infect int32 ] int32

            proc "random" [] float64
            // proc "setseed" [ float64 ] void // can't model void results

            func "acos" [ infect float64 ] float64
            func "acosd" [ infect float64 ] float64
            func "asin" [ infect float64 ] float64
            func "asind" [ infect float64 ] float64
            func "atan" [ infect float64 ] float64
            func "atand" [ infect float64 ] float64
            func "atan2" [ infect float64; infect float64 ] float64
            func "atan2d" [ infect float64; infect float64 ] float64
            func "cos" [ infect float64 ] float64
            func "cosd" [ infect float64 ] float64
            func "cot" [ infect float64 ] float64
            func "cotd" [ infect float64 ] float64
            func "sin" [ infect float64 ] float64
            func "sind" [ infect float64 ] float64
            func "tan" [ infect float64 ] float64
            func "tand" [ infect float64 ] float64

            // string functions https://www.postgresql.org/docs/9.6/static/functions-string.html
            func "bit_length" [ infect (stringish a') ] int32
            func "char_length" [ infect string ] int32
            func "character_length" [ infect string ] int32
            func "lower" [ infect string ] string
            func "octet_length" [ infect (stringish a') ] int32 // this works on BYTEA too
            // func "overlay" ... // wacky syntax! would have to do more work for this like TSQL special funcs
            // func "position" ... // wacky syntax: position('needle' in 'haystack')
            // substring has wacky syntax in documentation, but works fine without it
            func "substring" [ infect (stringish a'); infect int32; optional (infect int32) ] a'
            // trim has wacky syntax in documentation, but works fine without it
            // (except we can't specify leading/trailing)
            func "trim" [ infect string; optional (infect string) ] string
            func "upper" [ infect string ] string
            func "ascii" [ infect string ] int32
            func "chr" [ infect int32] string
            func "concat" [ scalar; vararg scalar ] string
            func "convert" [ infect binary; infect string; infect string ] binary
            func "convert_from" [ infect binary; infect string ] string
            func "convert_to" [ infect string; infect string ] binary
            func "decode" [ infect string; infect string ] binary
            func "encode" [ infect binary; infect string ] string
            func "format" [ infect string; vararg scalar ] string
            func "initcap" [ infect string ] string
            func "left" [ infect string; infect int32 ] string
            func "length" [ infect (stringish a'); optional (infect string) ] int32
            func "lpad" [ infect string; infect int32; optional (infect string) ] string
            func "ltrim" [ infect string; optional (infect string) ] string
            func "md5" [ infect (stringish a') ] string
            proc "pg_client_encoding" [] string
            // questionably useful in static SQL
            func "quote_ident" [ infect string ] string
            func "quote_literal" [ infect scalar ] string
            func "quote_nullable" [ scalar ] string
            // func "regexp_matches" // cannot represent TVF
            func "regexp_replace" [ infect string; infect string; infect string; optional (infect string) ] string
            func "repeat" [ infect string; infect int32 ] string
            func "replace" [ infect string; infect string ] string
            func "reverse" [ infect string ] string
            func "right" [ infect string; infect int32 ] string
            func "rpad" [ infect string; infect int32; optional (infect string) ] string
            func "rtrim" [ infect string; optional (infect string) ] string
            func "split_part" [ infect string; infect string; infect int32 ] string
            func "strpos" [ infect string; infect string ] int32
            func "substr" [ infect string; infect int32; optional (infect int32) ] string
            func "to_ascii" [ infect string; optional (infect string) ] string
            func "to_hex" [ integral ] string
            func "translate" [ infect string; infect string; infect string ] string

            // binary string functions https://www.postgresql.org/docs/9.6/static/functions-binarystring.html
            func "btrim" [ infect binary; optional (infect binary) ] binary
            func "get_bit" [ infect binary; infect int32 ] int32
            func "get_byte" [ infect binary; infect int32 ] int32
            func "set_bit" [ infect binary; infect int32; infect int32 ] binary
            func "set_byte" [ infect binary; infect int32; infect int32 ] binary

            // aggregate functions
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
        @"BYTEA E'\\x" + String.Concat(hexPairs) + "'" |> text
    override __.DateTimeLiteral(dt) =
        "TIMESTAMPTZ " + dt.ToString("yyyy'-'MM'-'dd'T'HH':'mm':'ss'.'fffZ") |> text
    override __.DateTimeOffsetLiteral(dt) =
        // Can't really store a DateTimeOffset, but we should let people use it since it's the only .NET
        // type that unambiguously represents a moment in time.
        "TIMESTAMPTZ " + dt.ToString("yyyy'-'MM'-'dd'T'HH':'mm':'ss'.'fffzzz") |> text
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
            | DateTimeTypeName
            | DateTimeOffsetTypeName -> "TIMESTAMPTZ"
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
    override __.SimilarityOperator(invert, op) =
        CommandText <|
        match op with
        | Like -> if invert then "NOT LIKE" else "LIKE"
        | Match -> if invert then "NOT SIMILAR TO" else "SIMILAR TO"
        | Regexp -> if invert then "!~" else "~"

type private PostgresStatement(indexer : IParameterIndexer) as this =
    inherit DefaultStatementTranslator(Name("POSTGRES"), indexer)
    let expr = PostgresExpression(this :> StatementTranslator, indexer)
    override __.Expr = upcast expr
    override __.ColumnsNullableByDefault = true
    override this.AlterTable(alter) =
        let inline alterColumn (col : Name) =
            [|  text "ALTER COLUMN"
                ws
                this.Expr.Name(col)
                ws
            |]
        let inline changeType (col : Name) (ty : TypeName) (collation : Name option) =
            seq {
                yield! alterColumn col
                yield text "TYPE"
                yield ws
                yield! this.Expr.TypeName(ty)
                match collation with
                | None -> ()
                | Some collation ->
                    yield ws
                    yield text "COLLATE"
                    yield ws
                    yield this.Expr.Name(collation)
            }
        seq {
            yield text "ALTER TABLE"
            yield ws
            yield! this.Expr.ObjectName(alter.Table)
            yield ws
            match alter.Alteration with
            | RenameTo newName ->
                yield text "RENAME TO"
                yield ws
                yield this.Expr.Name(newName)
            | AddColumn columnDef ->
                yield text "ADD COLUMN"
                yield ws
                yield! this.ColumnDefinition(alter.Table, columnDef.Value)
            | AddConstraint constr ->
                yield text "ADD"
                yield ws
                yield! this.TableConstraint(alter.Table, constr.Value) // includes CONSTRAINT keyword
            | AddDefault (col, defaultValue) ->
                yield! alterColumn col
                yield text "SET DEFAULT"
                yield ws
                yield! this.FirstClassValue(defaultValue)
            | DropColumn name ->
                yield text "DROP COLUMN"
                yield ws
                yield this.Expr.Name(name)
                yield ws
                yield text "RESTRICT" // this is probably the default but just to be on the safe side
            | DropConstraint constr ->
                yield text "DROP CONSTRAINT"
                yield ws
                yield this.Expr.Name(this.ConstraintName(alter.Table, constr))
                yield ws
                yield text "RESTRICT"
            | DropDefault col ->
                yield! alterColumn col
                yield text "DROP DEFAULT"
            | ChangeNullability change ->
                yield! alterColumn change.Column
                yield text (if change.NewNullable then "DROP NOT NULL" else "SET NOT NULL")
            | ChangeType change ->
                let schemaColumn = change.ExistingInfo.Column |> Option.get
                yield! changeType change.Column change.NewType schemaColumn.Collation
            | ChangeCollation change ->
                let schemaColumn = change.ExistingInfo.Column |> Option.get
                yield! changeType change.Column schemaColumn.ColumnTypeName (Some change.NewCollation)
        }
    // TODO: figure out PG quirks

type PostgresBackend() =
    static let initialModel =
        let main, temp = Name("main"), Name("temp")
        {   Schemas =
                [   Schema.Empty(main)
                    Schema.Empty(temp)
                ] |> List.map (fun s -> s.SchemaName, s) |> Map.ofList
            DefaultSchema = main
            TemporarySchema = temp
            Builtin =
                {   Functions = PostgresFunctions.functions
                }
        }
    interface IBackend with
        member this.MigrationBackend = <@ fun conn -> new DefaultMigrationBackend(conn) :> IMigrationBackend @>
        member this.InitialModel = initialModel
        member this.ParameterTransform(columnType) = ParameterTransform.Default(columnType)
        member this.ToCommandFragments(indexer, stmts) =
            let translator = PostgresStatement(indexer)
            translator.TotalStatements(stmts)
            |> BackendUtilities.simplifyFragments
            |> ResizeArray
            :> _ IReadOnlyList