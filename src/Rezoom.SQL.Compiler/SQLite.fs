namespace Rezoom.SQL.Compiler.SQLite
open System
open System.Collections.Generic
open Rezoom.SQL.Mapping
open System.Data
open System.Data.Common
open System.IO
open FSharp.Quotations
open Rezoom.SQL.Compiler
open Rezoom.SQL.Compiler.BackendUtilities
open Rezoom.SQL.Compiler.Translators
open Rezoom.SQL.Migrations

/// For the SQLite backend we convert DateTimes and GUIDs to supported underlying types
/// clob and blob respectively. The conversion methods live in this module so our quotations
/// can call them rather than inlining conversion code.
type SQLiteParamConversions() =
    static member DateTimeToString(dt : DateTime) : string =
        let utc =
            if dt.Kind = DateTimeKind.Unspecified then
                DateTime.SpecifyKind(dt, DateTimeKind.Utc)
            else
                dt.ToUniversalTime()
        utc.ToString("yyyy'-'MM'-'dd'T'HH':'mm':'ss'.'fffZ")

    static member GuidToBytes(g : Guid) : byte[] = g.ToByteArray()

type private SQLiteLiteral() =
    inherit DefaultLiteralTranslator()
    override __.BooleanLiteral(t) =
        CommandText <| if t then "1" else "0"
    override __.DateTimeLiteral(dt) =
        CommandText <| "'" + SQLiteParamConversions.DateTimeToString(dt) + "'"

type private SQLiteExpression(statement : StatementTranslator, indexer) =
    inherit DefaultExprTranslator(statement, indexer)
    let literal = SQLiteLiteral()
    override __.Literal = upcast literal
    override __.TypeName(name, autoIncrement) =
        let rec tyName name =
            match name with
            | BooleanTypeName
            | IntegerTypeName Integer16
            | IntegerTypeName Integer32
            | IntegerTypeName Integer64 -> if autoIncrement then "INTEGER" else "INT"
            | FloatTypeName Float32
            | FloatTypeName Float64 -> "FLOAT"
            | DateTimeTypeName // store datetimes as UTC ISO8601 strings -- yyyy-MM-ddTHH:mm:ssZ
            | StringTypeName(_) -> "VARCHAR"
            | GuidTypeName
            | BinaryTypeName(_) -> "BLOB"
            | DecimalTypeName
            | DateTimeOffsetTypeName -> fail <| sprintf "Unsupported type ``%A``" name
            | UnresolvedTypeName t -> bug <| sprintf "Unresolved UserType %s beyond resolution layer" t
            | ResolvedUserType r -> r.RawBackendSQLType |> Option.defaultWith (fun () -> tyName r.UnderlyingSQLTypeName)
        tyName name |> text |> Seq.singleton

type private SQLiteStatement(indexer : IParameterIndexer) as this =
    inherit DefaultStatementTranslator(Name("SQLITE"), indexer)
    let expr = SQLiteExpression(this :> StatementTranslator, indexer)
    override __.Expr = upcast expr
    override __.ColumnsNullableByDefault = true
    override __.AlterTable(alter) =
        match alter.Alteration with
        | RenameTo _
        | AddColumn _ ->
            base.AlterTable(alter)
        | _ ->
            fail <|
            Error.backendDoesNotSupportFeature
                "SQLite" "ALTER TABLE statements other than RENAME TO/ADD COLUMN"

type SQLiteMigrationBackend(info : ConnectionInfo) =
    inherit DefaultMigrationBackend(info)
    override this.Initialize() =
        let builder = DbConnectionStringBuilder(ConnectionString = info.ConnectionString)
        let dataSource = "Data Source"
        if builder.ContainsKey(dataSource) then
            match builder.[dataSource] with
            | :? string as dataSource ->
                if not <| File.Exists(dataSource) then
                    File.WriteAllBytes(dataSource, [||])
            | _ -> ()
        base.Initialize()

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
            BackendCharacteristics =
                {   CanDropColumnWithDefaultValue = true
                }
        }
    interface IBackend with
        member this.MigrationBackend = <@ fun settings -> new SQLiteMigrationBackend(settings) :> IMigrationBackend @>
        member this.InitialModel = initialModel
        member this.ParameterTransform(columnType) =
            ParameterTransform.Default(columnType, fun columnType ->
                match columnType.Type with
                | DateTimeType ->
                    {   ParameterType = StdDbType DbType.String
                        ValueTransform = fun expr ->
                            Expr.Call(typeof<SQLiteParamConversions>.GetMethod(nameof SQLiteParamConversions.DateTimeToString), [ expr ])
                    }
                | GuidType ->
                    {   ParameterType = StdDbType DbType.Binary
                        ValueTransform = fun expr ->
                            Expr.Call(typeof<SQLiteParamConversions>.GetMethod(nameof SQLiteParamConversions.GuidToBytes), [ expr ])
                    }
                | _ -> { ParameterType = columnType.XDbType; ValueTransform = fun e -> e }
            )
            
        member this.ToCommandFragments(indexer, stmts) =
            let translator = SQLiteStatement(indexer)
            translator.TotalStatements(stmts)
            |> BackendUtilities.simplifyFragments
            |> ResizeArray
            :> _ IReadOnlyList
       