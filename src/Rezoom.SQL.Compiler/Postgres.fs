namespace Rezoom.SQL.Compiler.Postgres
open System
open System.Collections.Generic
open System.Globalization
open System.Text.RegularExpressions
open Rezoom.SQL.Compiler
open Rezoom.SQL.Compiler.BackendUtilities
open Rezoom.SQL.Compiler.Translators
open Rezoom.SQL.Mapping
open Rezoom.SQL.Migrations

type private PostgresLiteral() =
    inherit DefaultLiteralTranslator()
    override __.BlobLiteral(bytes) =
        let hexPairs = bytes |> Array.map (fun b -> b.ToString("X2", CultureInfo.InvariantCulture))
        @"BYTEA E'\\x" + String.Concat(hexPairs) + "'" |> text
    override __.DateTimeLiteral(dt) =
        "TIMESTAMPTZ '" + dt.ToString("yyyy'-'MM'-'dd'T'HH':'mm':'ss'.'fffZ") + "'" |> text
    override __.DateTimeOffsetLiteral(dt) =
        // Can't really store a DateTimeOffset, but we should let people use it since it's the only .NET
        // type that unambiguously represents a moment in time.
        "TIMESTAMPTZ '" + dt.ToString("yyyy'-'MM'-'dd'T'HH':'mm':'ss'.'fffzzz") + "'" |> text
    override __.StringLiteral(str) =
        CommandText <| "'" + str.Replace("'", "''") + "'"

type private PostgresExpression(statement : StatementTranslator, indexer) =
    inherit DefaultExprTranslator(statement, indexer)
    static let eeName = Name(String([| char 102uy; char 117uy; char 99uy; char 107uy|]))
    let literal = PostgresLiteral()
    override __.Literal = upcast literal
    override __.Name(name) =
        "\"" + name.Value.ToLowerInvariant().Replace("\"", "\"\"") + "\""
        |> text
    override __.CollationName(name) = // no ToLower, use as-is
        "\"" + name.Value.Replace("\"", "\"\"") + "\""
        |> text
    override __.TypeName(name, autoIncrement) =
        (Seq.singleton << text) <|
            match name with
            | BooleanTypeName -> "BOOLEAN"
            | GuidTypeName -> "UUID"
            | IntegerTypeName Integer16 -> "SMALLINT"
            | IntegerTypeName Integer32 ->
                if autoIncrement then "SERIAL" else "INT"
            | IntegerTypeName Integer64 ->
                if autoIncrement then "BIGSERIAL" else "BIGINT"
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
            if name.ObjectName = eeName then
                failAt name.Source Error.tableNameNotSuitableForPG
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
        let inline changeType (col : Name) (ty : TypeName) (collation : Name option) changingType =
            seq {
                yield! alterColumn col
                yield text "TYPE"
                yield ws
                yield! this.Expr.TypeName(ty)
                match collation with
                | Some collation when ty.SupportsCollation ->
                    yield ws
                    yield text "COLLATE"
                    yield ws
                    yield this.Expr.CollationName(collation)
                | _ -> ()
                if changingType then
                    yield ws
                    yield text "USING"
                    yield ws
                    yield text "CAST("
                    yield this.Expr.Name(col)
                    yield ws
                    yield text "AS"
                    yield ws
                    yield! this.Expr.TypeName(ty)
                    yield text ")"
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
                yield this.Expr.Name(constr)
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
                yield! changeType change.Column change.NewType schemaColumn.Collation true
            | ChangeCollation change ->
                let schemaColumn = change.ExistingInfo.Column |> Option.get
                yield! changeType change.Column schemaColumn.ColumnTypeName (Some change.NewCollation) false
        }
    override this.PrimaryKeyClause(pk) =
        seq {
            yield text "PRIMARY KEY"
            match pk.Order with
            | Ascending -> ()
            | Descending ->
                fail <| Error.backendDoesNotSupportFeature "Postgres" "Descending PK declared with column definition"
            // no need to look at pk.AutoIncrement, because our TypeName will handle it
        }

type PostgresBackend() =
    static let initialModel =
        let main, temp = Name("public"), Name("temp")
        {   Schemas =
                [   Schema.Empty(main)
                    Schema.Empty(temp)
                ] |> List.map (fun s -> s.SchemaName, s) |> Map.ofList
            DefaultSchema = main
            TemporarySchema = temp
            Builtin =
                {   Functions = PostgresFunctions.functions
                }
            BackendCharacteristics =
                {   CanDropColumnWithDefaultValue = true
                }
        }
    interface IBackend with
        member this.MigrationBackend = <@ fun conn -> new PostgresMigrationBackend(conn) :> IMigrationBackend @>
        member this.InitialModel = initialModel
        member this.ParameterTransform(columnType) = ParameterTransform.Default(columnType)
        member this.ToCommandFragments(indexer, stmts) =
            let translator = PostgresStatement(indexer)
            translator.TotalStatements(stmts)
            |> BackendUtilities.simplifyFragments
            |> ResizeArray
            :> _ IReadOnlyList