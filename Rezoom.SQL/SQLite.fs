namespace Rezoom.SQL.SQLite
open System
open System.Data
open System.Collections.Generic
open System.Globalization
open Rezoom.SQL
open Rezoom.SQL.Mapping
open Rezoom.SQL.BackendUtilities
open Rezoom.SQL.Translators

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
            | IntegerTypeName Integer64 -> "INT"
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

module SQLiteFunctions =
    open Rezoom.SQL.FunctionDeclarations
    let functions =
        let int = int64
        [|  // core functions from https://www.sqlite.org/lang_corefunc.html
            func "abs" [ a' ] a'
            proc "changes" [] int
            func "char" [] string |> withVarArg int
            func "coalesce" [ a'; a' ] a' |> withVarArg a'
            func "glob" [ string; string ] boolean
            func "hex" [ binary ] string
            func "ifnull" [ a'; a' ] a'
            func "instr" [ a' |> constrained [ string; binary ]; a' ] int
            proc "last_insert_rowid" [] int
            func "length" [ a' |> constrained [ string; binary ] ] int
            func "like" [ string; string ] boolean // TODO: allow ESCAPE clause?
            func "likelihood" [ boolean; float64 ] boolean
            func "likely" [ boolean ] boolean
            // no load_extension
            func "lower" [ string ] string
            func "ltrim" [ string; string ] string // TODO: optional chars argument
            func "max" [ a' ] a' |> withVarArg a' // TODO: aggregate with one argument, regular func otherwise?
            func "min" [ a' ] a' |> withVarArg a' // TODO: aggregate with one argument, regular func otherwise?
            func "nullif" [ a'; a' ] a'
            func "printf" [ string ] string |> withVarArg any
            func "quote" [ any ] string
            proc "random" [] int
            proc "randomblob" [] binary
            func "replace" [ string; string; string ] string
            func "round" [ float64; int ] float64 // TODO: make precision argument optional
            func "rtrim" [ string; string ] string // TODO: optional chars argument
            func "soundex" [ string ] string
            func "sqlite_compileoption_get" [ int ] string
            func "sqlite_compileoption_used" [ string ] boolean
            func "sqlite_source_id" [] string
            func "sqlite_version" [] string
            func "substr" [ string; int; int ] string // TODO: make length argument optional
            proc "total_changes" [] int
            func "trim" [ string; string ] string // TODO: optional chars argument
            func "typeof" [ any ] string
            func "unicode" [ string ] int
            func "unlikely" [ boolean ] boolean
            func "upper" [ string ] string
            func "zeroblob" [ int ] binary

            // aggregate functions from https://www.sqlite.org/lang_aggfunc.html
            aggregate "avg" [ a' |> constrained [ int; float64 ] ] float64 |> withDistinct
            aggregate "count" [ any ] int |> withWildcard |> withDistinct
            aggregate "group_concat" [ string; string ] string |> withDistinct // TODO: make separator optional
            aggregate "sum" [ a' ] a' |> withDistinct
            aggregate "total" [ a' ] a' |> withDistinct
            // min/max excluded due to mention above in core functions

            // date and time functions from https://www.sqlite.org/lang_datefunc.html
            // for now we use strings to represent dates -- maybe should formalize this by using the datetime type
            // even though its underlying representation will be a string
            func "date" [ string ] string |> withVarArg string
            func "time" [ string ] string |> withVarArg string
            func "datetime" [ string ] string |> withVarArg string
            func "julianday" [ string ] string |> withVarArg string
            func "strftime" [ string; string ] string |> withVarArg string
        |] |> mapBy (fun f -> f.FunctionName)

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
        member this.MigrationBackend = <@ fun conn -> DefaultMigrationBackend(conn) :> Migrations.IMigrationBackend @>
        member this.InitialModel = initialModel
        member this.ParameterTransform(columnType) = ParameterTransform.Default(columnType)
        member this.ToCommandFragments(indexer, stmts) =
            let translator = SQLiteStatement(indexer)
            translator.TotalStatements(stmts)
            |> BackendUtilities.simplifyFragments
            |> ResizeArray
            :> _ IReadOnlyList
       