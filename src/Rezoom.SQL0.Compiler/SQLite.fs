namespace Rezoom.SQL.Compiler.SQLite
open System
open System.Collections.Generic
open System.Configuration
open System.Data
open System.Data.Common
open System.IO
open FSharp.Quotations
open Rezoom.SQL.Compiler
open Rezoom.SQL.Compiler.BackendUtilities
open Rezoom.SQL.Compiler.Translators
open Rezoom.SQL.Mapping
open Rezoom.SQL.Migrations

type private SQLiteLiteral() =
    inherit DefaultLiteralTranslator()
    override __.BooleanLiteral(t) =
        CommandText <| if t then "1" else "0"
    override __.DateTimeLiteral(dt) =
        CommandText <| "'" + dt.ToString("yyyy'-'MM'-'dd'T'HH':'mm':'ss'.'fffZ") + "'"

type private SQLiteExpression(statement : StatementTranslator, indexer) =
    inherit DefaultExprTranslator(statement, indexer)
    let literal = SQLiteLiteral()
    override __.Literal = upcast literal
    override __.TypeName(name, autoIncrement) =
        (Seq.singleton << text) <|
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

type SQLiteMigrationBackend(settings : ConnectionStringSettings) =
    inherit DefaultMigrationBackend(settings)
    override this.Initialize() =
        let builder = DbConnectionStringBuilder(ConnectionString = settings.ConnectionString)
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
            match columnType.Type with
            | DateTimeType ->
                let transform (expr : Quotations.Expr) =
                    let xform (dtExpr : Quotations.Expr<DateTime>) =
                        <@  let utcDt =
                                let dtExpr = %dtExpr
                                if dtExpr.Kind = DateTimeKind.Unspecified
                                then DateTime.SpecifyKind(dtExpr, DateTimeKind.Utc)
                                else dtExpr.ToUniversalTime()
                            utcDt.ToString("yyyy'-'MM'-'dd'T'HH':'mm':'ss'.'fffZ") |> box
                        @>
                    let xform (dtExpr : Quotations.Expr) =
                        (xform (Expr.Cast(Expr.Coerce(dtExpr, typeof<DateTime>)))).Raw
                    let ty = expr.Type
                    let asObj = Expr.Coerce(expr, typeof<obj>)
                    if ty.IsConstructedGenericType && ty.GetGenericTypeDefinition() = typedefof<_ option> then
                        let invokeValue = Expr.Coerce(Expr.PropertyGet(expr, ty.GetProperty("Value")), typeof<obj>)
                        <@@ if isNull %%asObj then box DBNull.Value else %%xform invokeValue @@>
                    else
                        <@@ if isNull %%asObj then box DBNull.Value else %%xform asObj @@>
                {   ParameterType = DbType.String
                    ValueTransform = transform
                }
            | GuidType ->
                let transform (expr : Quotations.Expr) =
                    let xform (gExpr : Quotations.Expr<Guid>) =
                        <@  let guid = %gExpr
                            let bytes = guid.ToByteArray()
                            box bytes
                        @>
                    let xform (gExpr : Quotations.Expr) =
                        (xform (Expr.Cast(Expr.Coerce(gExpr, typeof<Guid>)))).Raw
                    let ty = expr.Type
                    let asObj = Expr.Coerce(expr, typeof<obj>)
                    if ty.IsConstructedGenericType && ty.GetGenericTypeDefinition() = typedefof<_ option> then
                        let invokeValue = Expr.Coerce(Expr.PropertyGet(expr, ty.GetProperty("Value")), typeof<obj>)
                        <@@ if isNull %%asObj then box DBNull.Value else %%xform invokeValue @@>
                    else
                        <@@ if isNull %%asObj then box DBNull.Value else %%xform asObj @@>
                {   ParameterType = DbType.Binary
                    ValueTransform = transform
                }
            | _ -> ParameterTransform.Default(columnType)
        member this.ToCommandFragments(indexer, stmts) =
            let translator = SQLiteStatement(indexer)
            translator.TotalStatements(stmts)
            |> BackendUtilities.simplifyFragments
            |> ResizeArray
            :> _ IReadOnlyList
       