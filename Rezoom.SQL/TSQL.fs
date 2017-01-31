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
        match bin.Operator, bin.Right.Value with
        | Is, LiteralExpr NullLiteral
        | IsNot, LiteralExpr NullLiteral ->
            seq {
                yield! this.Expr(bin.Left, FirstClassValue)
                yield ws
                yield text "IS"
                yield ws
                if bin.Operator = IsNot then
                    yield text "NOT"
                    yield ws
                yield text "NULL"
            }
        | Is, _
        | IsNot, _ ->
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
    override __.ColumnsNullableByDefault = true
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
    override this.AutoIncrement = "IDENTITY(1,1)"

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
                CREATE TABLE __RZSQL_MIGRATIONS
                    ( MajorVersion int
                    , Name varchar(256)
                    , UNIQUE (MajorVersion, Name)
                    );
            """
        ignore <| cmd.ExecuteNonQuery()

module TSQLFunctions =
    open Rezoom.SQL.FunctionDeclarations
    let private noArgProc name ret = // TODO: translate to NAME
        proc name [] ret
    let private atAtProc name ret = // TODO: translate to @@name
        proc name [] ret
    let private datePartFunc name otherArgs ret = // TODO: translate to f(datepart, ...)
        func name (string :: otherArgs) ret
    let private i = integral
    let private ii = infect i
    let private date = datetime
    let functions =
        [|  atAtProc "datefirst" int8
            atAtProc "dbts" binary
            atAtProc "langid" int8
            atAtProc "language" string
            atAtProc "lock_timeout" int32
            atAtProc "max_connections" int32
            atAtProc "max_precision" int8
            atAtProc "nestlevel" int32
            atAtProc "options" int32
            atAtProc "remserver" string
            atAtProc "servername" string
            atAtProc "servicename" string
            atAtProc "spid" int8
            atAtProc "textsize" int32
            atAtProc "version" string
            atAtProc "cursor_rows" int32
            atAtProc "fetch_status" int32
            // date/time functions from https://msdn.microsoft.com/en-us/library/ms186724.aspx
            noArgProc "current_timestamp" datetime
            func "sysdatetime" [] datetime
            func "sysdatetimeoffset" [] datetimeoffset
            func "sysutcdatetime" [] datetime
            func "getdate" [] datetime
            func "getutcdate" [] datetime
            datePartFunc "datename" [ infect datetime ] string
            datePartFunc "dateadd" [ infect datetime ] string
            datePartFunc "datediff" [ infect datetime; infect datetime ] int32
            datePartFunc "datediff_big" [ infect datetime; infect datetime ] int64
            datePartFunc "dateadd" [ infect i; infect datetime ] datetime
            func "day" [ infect datetime ] i
            func "month" [ infect datetime ] i
            func "year" [ infect datetime ] i
            func "datefromparts" [ ii; ii; ii ] date
            func "datetime2fromparts" [ ii; ii; ii; ii; ii; ii; ii; ii ] datetime
            func "datetimefromparts" [ ii; ii; ii; ii; ii; ii; ii ] datetime
            func "datetimeoffsetfromparts" [ ii; ii; ii; ii; ii; ii; ii; ii; ii; ii ] datetimeoffset
            func "smalldatetimefromparts" [ ii; ii; ii; ii; ii ] datetime
            // math funcs from https://msdn.microsoft.com/en-us/library/ms177516.aspx
            func "acos" [ infect fractional ] float64
            func "asin" [ infect fractional ] float64
            func "atan" [ infect fractional ] float64
            func "atn2" [ infect fractional; infect fractional ] float64
            func "ceiling" [ infect (numeric a') ] a'
            func "cos" [ infect fractional] float64
            func "cot" [ infect fractional ] float64
            func "degrees" [ infect (numeric a') ] a'
            func "exp" [ infect fractional ] float64
            func "floor" [ infect (numeric a') ] a'
            func "log" [ infect num; infect (optional i) ] float64
            func "log10" [ infect num ] float64
            func "pi" [] float64
            func "power" [ infect (numeric a'); infect num ] a'
            func "radians" [ infect (numeric a') ] a'
            func "rand" [ infect (optional i) ] float64
            func "round" [ infect (numeric a'); infect i ] a'
            func "sign" [ infect (numeric a') ] a'
            func "sin" [ infect fractional ] float64
            func "sqrt" [ infect (numeric a') ] float64
            func "square" [ infect (numeric a') ] float64
            func "tan" [ infect fractional ] float64
            // JSON functions from https://msdn.microsoft.com/en-us/library/dn921900.aspx
            func "isjson" [ infect string ] boolean
            func "json_value" [ infect string; infect string ] string
            func "json_query" [ infect string; infect string ] string
            func "json_modify" [ infect string; infect string; infect string ] string
            // logical funcs from https://msdn.microsoft.com/en-us/library/hh213226.aspx
            func "choose" [ infect i; vararg (infect a') ] a'
            func "iif" [ boolean; infect a'; infect a' ] a'
            // skip over "metadata functions" (for now) from https://msdn.microsoft.com/en-us/library/ms187812.aspx
            // ...
            // also "security functions" (for now) from https://msdn.microsoft.com/en-us/library/ms186236.aspx
            // ...
            // so onto string functions from https://msdn.microsoft.com/en-us/library/ms181984.aspx
            func "ascii" [ infect string ] int32
            func "concat" [ string; string; vararg string ] string
            func "format" [ infect scalar; infect string; optional (infect string) ] string
            func "lower" [ infect string ] string
            func "upper" [ infect string ] string
            func "patindex" [ infect string; infect string ] integral
            func "replicate" [ infect string; infect integral ] string
            func "rtrim" [ infect string ] string
            func "ltrim" [ infect string ] string
            func "str" [ infect fractional; optional integral; optional integral ] string
            // func "string_split" [ infect string; infect string ] string_table // wait till we can do TVFs
            func "translate" [ infect string; infect string; infect string ] string
            func "char" [ infect integral ] string
            func "concat_ws" [ infect string; scalar; scalar; vararg scalar ] string
            func "left" [ infect string; infect integral ] string
            func "right" [ infect string; infect integral ] string
            func "quotename" [ infect string; optional (infect string) ] string
            func "reverse" [ infect string ] string
            func "soundex" [ infect string ] string
            // func "string_agg" // wtf, how do we support this? it has its own special clause type...
            func "stuff" [ infect (a' |> constrained StringishTypeClass); infect integral; infect integral; string ] a'
            func "trim" [ infect string ] string // come on TSQL, "characters from"? cut it out...
            func "charindex" [ infect string; infect string ; optional integral ] integral
            func "difference" [ infect string; infect string ] int32
            func "len" [ infect string ] integral
            func "nchar" [ infect integral ] string
            func "replace" [ infect string; infect string; infect string ] string
            func "space" [ infect integral ] string
            func "string_escape" [ infect string; infect string ] string // TODO: enforce literal on 2nd arg?
            func "substring" [ infect a' |> constrained StringishTypeClass; infect integral; infect integral ] a'
            func "unicode" [ infect string ] int32
        |] |> DefaultFunctions.extendedBy

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
                {   Functions = TSQLFunctions.functions
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
       