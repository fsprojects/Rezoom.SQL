namespace Rezoom.SQL.Compiler.SQLite
open System
open System.Data
open System.Collections.Generic
open System.Globalization
open Rezoom.SQL.Compiler
open Rezoom.SQL.Compiler.BackendUtilities
open Rezoom.SQL.Compiler.Translators
open Rezoom.SQL.Mapping
open Rezoom.SQL.Migrations

type private SQLiteLiteral() =
    inherit DefaultLiteralTranslator()
    override __.BooleanLiteral(t) = CommandText <| if t then "1" else "0"

type private SQLiteExpression(statement : StatementTranslator, indexer) =
    inherit DefaultExprTranslator(statement, indexer)
    let literal = SQLiteLiteral()
    override __.Literal = upcast literal
    override __.TypeName(name) =
        (Seq.singleton << text) <|
            match name with
            | BooleanTypeName
            | IntegerTypeName Integer8
            | IntegerTypeName Integer16
            | IntegerTypeName Integer32
            | IntegerTypeName Integer64 -> "INTEGER"
            | FloatTypeName Float32
            | FloatTypeName Float64 -> "FLOAT"
            | StringTypeName(_) -> "VARCHAR"
            | BinaryTypeName(_) -> "BLOB"
            | DecimalTypeName
            | DateTimeTypeName
            | DateTimeOffsetTypeName -> failwith <| sprintf "Unsupported type ``%A``" name

type private SQLiteStatement(indexer : IParameterIndexer) as this =
    inherit DefaultStatementTranslator(Name("SQLITE"), indexer)
    let expr = SQLiteExpression(this :> StatementTranslator, indexer)
    override __.Expr = upcast expr
    override __.ColumnsNullableByDefault = true

module private SQLiteFunctions =
    open Rezoom.SQL.Compiler.FunctionDeclarations
    let private minmax name =
        { new FunctionType(Name(name), [| infect a'; vararg (infect a') |], a', idem = true) with
            override __.Aggregate(arg) =
                match arg with
                | ArgumentWildcard -> None
                | ArgumentList (_, exprs) ->
                    if exprs.Length = 1 then
                        Some { AllowWildcard = false; AllowDistinct = false }
                    else
                        None
        }
    let functions =
        let numeric ty = ty |> constrained NumericTypeClass
        [|  // core functions from https://www.sqlite.org/lang_corefunc.html
            proc "changes" [] int64
            func "char" [ vararg string ] string
            func "glob" [ infect string; infect string ] boolean
            func "hex" [ binary ] string
            func "ifnull" [ nullable a'; infect a' ] a'
            func "instr" [ infect (stringish a'); infect a' ] int64
            proc "last_insert_rowid" [] int64
            func "length" [ infect (stringish scalar) ] int64
            func "like" [ infect string; infect string; optional (infect string) ] boolean
            func "likelihood" [ boolean; float64 ] boolean
            func "likely" [ boolean ] boolean
            // no load_extension
            func "lower" [ infect string ] string
            func "ltrim" [ infect string; optional (infect string) ] string
            minmax "max"
            minmax "min"
            func "nullif" [ a'; a' ] (nullable a')
            func "printf" [ infect string; vararg scalar ] string
            func "quote" [ scalar ] string
            proc "random" [] int64
            proc "randomblob" [] binary
            func "replace" [ infect string; infect string; infect string ] string
            func "round" [ infect float64; optional (infect integral) ] float64
            func "rtrim" [ infect string; optional (infect string) ] string
            func "soundex" [ infect string ] string
            func "sqlite_compileoption_get" [ integral ] string
            func "sqlite_compileoption_used" [ infect string ] boolean
            func "sqlite_source_id" [] string
            func "sqlite_version" [] string
            func "substr" [ infect string; infect integral; optional (infect integral) ] string
            proc "total_changes" [] int64
            func "trim" [ infect string; optional (infect integral) ] string
            func "typeof" [ scalar ] string
            func "unicode" [ infect string ] int64
            func "unlikely" [ boolean ] boolean
            func "upper" [ infect string ] string
            func "zeroblob" [ integral ] binary

            // aggregate functions from https://www.sqlite.org/lang_aggfunc.html
            aggregate "avg" [ numeric a' ] (nullable float64)
            aggregateW "count" [ scalar ] int64
            aggregate "group_concat" [ infect string; optional string ] string
            aggregate "sum" [ numeric a' ] a'
            aggregate "total" [ numeric a' ] a'

            // date and time functions from https://www.sqlite.org/lang_datefunc.html
            // for now we use strings to represent dates -- maybe should formalize this by using the datetime type
            // even though its underlying representation will be a string
            func "date" [ string; vararg string ] (nullable string)
            func "time" [ string; vararg string ] (nullable string)
            func "datetime" [ string; vararg string ] (nullable string)
            func "julianday" [ string; vararg string ] (nullable string)
            func "strftime" [ string; string; vararg string ] (nullable string)
        |] |> DefaultFunctions.extendedBy

type SQLiteBackend() =
    static let initialModel =
        let main, temp = Name("main"), Name("temp")
        {   Schemas =
                [   Schema.Empty(main)
                    Schema.Empty(temp)
                ] |> List.map (fun s -> s.SchemaName, s) |> Map.ofList
            DefaultSchema = main
            TemporarySchema = temp
            Builtin =
                {   Functions = SQLiteFunctions.functions
                }
        }
    interface IBackend with
        member this.MigrationBackend = <@ fun settings -> new DefaultMigrationBackend(settings) :> IMigrationBackend @>
        member this.InitialModel = initialModel
        member this.ParameterTransform(columnType) = ParameterTransform.Default(columnType)
        member this.ToCommandFragments(indexer, stmts) =
            let translator = SQLiteStatement(indexer)
            translator.TotalStatements(stmts)
            |> BackendUtilities.simplifyFragments
            |> ResizeArray
            :> _ IReadOnlyList
       