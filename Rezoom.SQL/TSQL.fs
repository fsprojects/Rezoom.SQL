namespace Rezoom.SQL.TSQL
open System
open System.Data
open System.Data.Common
open System.Collections.Generic
open System.Globalization
open Rezoom.SQL
open Rezoom.SQL.Mapping
open Rezoom.SQL.BackendUtilities
open Rezoom.SQL.Translators

type private TSQLLiteral() =
    inherit DefaultLiteralTranslator()
    override __.BooleanLiteral(t) = CommandText <| if t then "1" else "0"
    override __.BlobLiteral(bytes) =
        let hexPairs = bytes |> Array.map (fun b -> b.ToString("X2", CultureInfo.InvariantCulture))
        "0x" + String.Concat(hexPairs) |> text

type private TSQLExpression(statement : StatementTranslator, indexer) =
    inherit DefaultExprTranslator(statement, indexer)
    let literal = DefaultLiteralTranslator()
    override __.Literal = upcast literal
    override __.TypeName(name) =
        (Seq.singleton << text) <|
            match name with
            | BooleanTypeName -> "BIT"
            | IntegerTypeName Integer8 -> "TINYINT"
            | IntegerTypeName Integer16 -> "SMALLINT"
            | IntegerTypeName Integer32 -> "INT"
            | IntegerTypeName Integer64 -> "BIGINT"
            | FloatTypeName Float32 -> "FLOAT(24)"
            | FloatTypeName Float64 -> "FLOAT(53)"
            | StringTypeName(len) -> "NVARCHAR(" + string len + ")"
            | BinaryTypeName(len) -> "VARBINARY(" + string len + ")"
            | DecimalTypeName -> "NUMERIC(38, 19)"
            | DateTimeTypeName -> "DATETIME2"
            | DateTimeOffsetTypeName -> "DATETIMEOFFSET"
    override __.BinaryOperator(op) =
        CommandText <|
        match op with
        | Concatenate -> "+"
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
        | Is
        | IsNot -> bug "should have been handled for TSQL before we got here"
        | BitShiftLeft
        | BitShiftRight -> failwithf "Not supported by TSQL: %A" op
    override this.Binary(bin) =
        match bin.Operator with
        | Is
        | IsNot ->
            seq {
                if bin.Operator = IsNot then
                    yield text "NOT"
                    yield ws
                yield text "EXISTS(SELECT"
                yield ws
                yield! this.Expr(bin.Left, FirstClassValue)
                yield ws
                yield text "INTERSECT SELECT"
                yield ws
                yield! this.Expr(bin.Right, FirstClassValue)
                yield text ")"
            }
        | _ -> base.Binary(bin)
    override __.UnaryOperator(op) =
        CommandText <|
        match op with
        | Negative -> "-"
        | Not -> "NOT"
        | NotNull -> "IS NOT NULL"
        | IsNull -> "IS NULL"
        | BitNot -> "~"
    override __.SimilarityOperator(op) =
        CommandText <|
        match op with
        | Like -> "LIKE"
        | Glob
        | Match
        | Regexp -> failwithf "Not supported by TSQL: %A" op
    /// Identifies expressions that are set up to use as predicates in T-SQL.
    /// These expressions don't produce actual values.
    /// For example, you can't `SELECT 1=1`, but you can do `SELECT 1 WHERE 1=1`.
    /// Conversely, you can't `SELECT 1 WHERE tbl.BitColumn`, but you can do `SELECT tbl.BitColumn`.
    static member private IsPredicateBoolean(expr : TExpr) =
        expr.Info.Type.Type = BooleanType
        &&  match expr.Value with
            | SimilarityExpr _
            | BetweenExpr _
            | InExpr _
            | ExistsExpr _
            | BinaryExpr _
            | UnaryExpr _ -> true
            | _ -> false
    member private __.BaseExpr(expr, context) = base.Expr(expr, context)
    override this.Expr(expr, context) =
        match context with
        | FirstClassValue ->
            if TSQLExpression.IsPredicateBoolean(expr) then
                seq {
                    yield text "CAST((CASE WHEN"
                    yield ws
                    yield! this.BaseExpr(expr, Predicate)
                    yield ws
                    yield text "THEN 1 ELSE 0 END) AS BIT)"
                }
            else
                base.Expr(expr, context)
        | Predicate ->
            if TSQLExpression.IsPredicateBoolean(expr) then
                base.Expr(expr, context)
            else
                seq {
                    yield text "(("
                    yield! this.BaseExpr(expr, FirstClassValue)
                    yield text ")<>0)"
                }

type private TSQLStatement(indexer : IParameterIndexer) as this =
    inherit DefaultStatementTranslator(Name("TSQL"), indexer)
    let expr = TSQLExpression(this :> StatementTranslator, indexer)
    override __.Expr = upcast expr
    member this.SelectCoreWithTop(select : TSelectCore, top) =
        seq {
            yield text "SELECT"
            yield ws
            match top with
            | None -> ()
            | Some top ->
                yield text "TOP"
                yield ws
                yield! this.FirstClassValue(top)
                yield ws
            yield! this.ResultColumns(select.Columns)
            match select.From with
            | None -> ()
            | Some from ->
                yield ws
                yield text "FROM"
                yield ws
                yield! this.TableExpr(from)
            match select.Where with
            | None -> ()
            | Some where ->
                yield ws
                yield text "WHERE"
                yield ws
                yield! this.Predicate(where)
            match select.GroupBy with
            | None -> ()
            | Some groupBy ->
                yield ws
                yield text "GROUP BY"
                yield ws
                yield! groupBy.By |> Seq.map this.FirstClassValue |> join ","
                match groupBy.Having with
                | None -> ()
                | Some having ->
                    yield ws
                    yield text "HAVING"
                    yield ws
                    yield! this.Predicate(having)
        }
    override this.SelectCore(select) = this.SelectCoreWithTop(select, None)
    override this.Select(select) =
        match select.Value.Limit with
        | None -> base.Select(select)
        | Some limit ->
            // TSQL doesn't exactly support LIMIT so what shall we do?
            match limit.Offset, select.Value.Compound.Value with
            | None, CompoundTerm { Value = Select core } ->
                // We can use TOP here
                this.SelectCoreWithTop(core, Some limit.Limit)
            | _ ->
                this.Select(select) // Our override of LIMIT will turn this into an offset/fetch clause
    override this.Limit(limit) =
        seq {
            yield text "OFFSET"
            yield ws
            match limit.Offset with
            | Some offset ->
                yield! this.FirstClassValue(offset)
            | None ->
                yield text "0"
            yield ws
            yield text "ROWS FETCH NEXT"
            yield ws
            yield! this.FirstClassValue(limit.Limit)
            yield ws
            yield text "ROWS ONLY"
        }

type TSQLMigrationBackend(conn : DbConnection) =
    inherit DefaultMigrationBackend(conn)
    override __.Initialize() =
        use cmd = conn.CreateCommand()
        cmd.CommandText <-
            """
                IF NOT EXISTS (
                    SELECT * FROM sys.tables t
                    JOIN sys.schemas s ON t.schema_id = s.schema_id
                    WHERE s.name = 'dbo' and t.name = '__RZSQL_MIGRATIONS'
                )
                CREATE TABLE IF NOT EXISTS __RZSQL_MIGRATIONS
                    ( MajorVersion int
                    , Name varchar(256)
                    , UNIQUE (MajorVersion, Name)
                    );
            """
        ignore <| cmd.ExecuteNonQuery()

type TSQLBackend() =
    static let initialModel =
        let main, temp = Name("dbo"), Name("temp")
        {   Schemas =
                [   Schema.Empty(main)
                    Schema.Empty(temp)
                ] |> List.map (fun s -> s.SchemaName, s) |> Map.ofList
            DefaultSchema = main
            TemporarySchema = temp
            Builtin =
                {   Functions = Map.empty
                }
        }
    interface IBackend with
        member this.MigrationBackend = <@ fun conn -> TSQLMigrationBackend(conn) :> Migrations.IMigrationBackend @>
        member this.InitialModel = initialModel
        member this.ParameterTransform(columnType) = ParameterTransform.Default(columnType)
        member this.ToCommandFragments(indexer, stmts) =
            let translator = TSQLStatement(indexer)
            translator.TotalStatements(stmts)
            |> BackendUtilities.simplifyFragments
            |> ResizeArray
            :> _ IReadOnlyList
       