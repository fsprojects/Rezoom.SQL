namespace Rezoom.SQL.Compiler.TSQL
open System
open System.Collections.Generic
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

type private TSQLLiteral() =
    inherit DefaultLiteralTranslator()
    override __.BooleanLiteral(t) = CommandText <| if t then "1" else "0"
    override __.BlobLiteral(bytes) =
        let hexPairs = bytes |> Array.map (fun b -> b.ToString("X2", CultureInfo.InvariantCulture))
        "0x" + String.Concat(hexPairs) |> text
    override __.DateTimeLiteral(dt) =
        CommandText <| "'" + dt.ToString("yyyy'-'MM'-'dd'T'HH':'mm':'ss'.'fff") + "'"
    override __.DateTimeOffsetLiteral(dt) =
        CommandText <| "'" + dt.ToString("yyyy'-'MM'-'dd'T'HH':'mm':'ss'.'fffzzz") + "'"
    override __.StringLiteral(str) =
        CommandText <| "N'" + str.Replace("'", "''") + "'"

type private TSQLExpression(statement : StatementTranslator, indexer) =
    inherit DefaultExprTranslator(statement, indexer)
    let literal = TSQLLiteral()
    override __.Literal = upcast literal
    override __.Name(name) =
        "[" + name.Value.Replace("]", "]]") + "]"
        |> text
    override __.TypeName(name) =
        (Seq.singleton << text) <|
            match name with
            | BooleanTypeName -> "BIT"
            | GuidTypeName -> "UNIQUEIDENTIFIER"
            | IntegerTypeName Integer16 -> "SMALLINT"
            | IntegerTypeName Integer32 -> "INT"
            | IntegerTypeName Integer64 -> "BIGINT"
            | FloatTypeName Float32 -> "FLOAT(24)"
            | FloatTypeName Float64 -> "FLOAT(53)"
            | StringTypeName(Some len) -> "NVARCHAR(" + string len + ")"
            | StringTypeName(None) -> "NVARCHAR(max)"
            | BinaryTypeName(Some len) -> "VARBINARY(" + string len + ")"
            | BinaryTypeName(None) -> "VARBINARY(max)"
            | DecimalTypeName -> "NUMERIC(38, 19)"
            | DateTimeTypeName -> "DATETIME2"
            | DateTimeOffsetTypeName -> "DATETIMEOFFSET"
    override this.ObjectName name =
        seq {
            match name.SchemaName with
            | Some schema ->
                if schema = Name("temp") then
                    yield this.Name("#" + name.ObjectName)
                else
                    yield this.Name(schema)
                    yield text "."
                    yield this.Name(name.ObjectName) 
            | None -> yield this.Name(name.ObjectName) 
        }
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
        | BitNot -> "~"
    override __.SimilarityOperator(invert, op) =
        CommandText <|
        match op with
        | Like -> if invert then "NOT LIKE" else "LIKE"
        | Match
        | Regexp -> fail <| Error.backendDoesNotSupportFeature "TSQL" "MATCH/REGEXP operators"
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
    override this.Invoke(func) =
        match TSQLFunctions.getCustom func.FunctionName with
        | Some custom -> custom (this :> ExprTranslator) func
        | None -> base.Invoke(func)

type private TSQLStatement(indexer : IParameterIndexer) as this =
    inherit DefaultStatementTranslator(Name("TSQL"), indexer)
    let defaultConstraint (table : Name) (column : Name) = table + "_" + column + "_DEFAULT_CONSTRAINT"
    let expr = TSQLExpression(this :> StatementTranslator, indexer)
    override __.Expr = upcast expr
    override __.ColumnsNullableByDefault = true
    override __.CreateView(createView) =
        let createView = base.CreateView(createView)
        // http://msdn.microsoft.com/en-us/library/ms175502(v=sql.105).aspx
        // have to have create view statements get their own batch, because T-SQL has terrible design decisions
        seq {
            yield text TSQLMigrationBackend.BatchSeparator
            yield! createView
            yield text TSQLMigrationBackend.BatchSeparator
        }
    override __.DropObject(drop) =
        let drop = base.DropObject(drop)
        seq {
            yield text TSQLMigrationBackend.BatchSeparator
            yield! drop
            yield text TSQLMigrationBackend.BatchSeparator
        }
    member this.SelectCoreWithTop(select : TSelectCore, top) =
        seq {
            yield text "SELECT"
            match top with
            | None -> ()
            | Some top ->
                yield ws
                yield text "TOP"
                yield ws
                yield! this.FirstClassValue(top)
            yield linebreak
            yield! this.ResultColumns(select.Columns) |> indent
            match select.From with
            | None -> ()
            | Some from ->
                yield linebreak
                yield text "FROM"
                yield ws
                yield! this.TableExpr(from) |> indent
            match select.Where with
            | None -> ()
            | Some where ->
                yield linebreak
                yield text "WHERE"
                yield ws
                yield! this.Predicate(where) |> indent
            match select.GroupBy with
            | None -> ()
            | Some groupBy ->
                yield linebreak
                yield text "GROUP BY"
                yield ws
                yield! groupBy.By |> Seq.map this.FirstClassValue |> join ","
                match groupBy.Having with
                | None -> ()
                | Some having ->
                    yield linebreak
                    yield text "HAVING"
                    yield ws
                    yield! this.Predicate(having) |> indent
        }
    override this.Compound(expr) =
        match expr with
        | CompoundTerm _ -> base.Compound(expr)
        | _ ->
            // TSQL compound terms don't always evaluate left->right, INTERSECT has higher precedence
            // so just wrap in parens to be safe (this syntax is not legal on SQLite, which *does* eval left->right)
            let wrapped = base.Compound(expr)
            seq {
                yield text "("
                yield! wrapped
                yield text ")"
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
                seq {
                    match select.Value.With with
                    | None -> ()
                    | Some withClause ->
                        yield! this.With(withClause)
                        yield linebreak
                    yield! this.SelectCoreWithTop(core, Some limit.Limit)
                    match select.Value.OrderBy with
                    | None -> ()
                    | Some orderBy ->
                        yield linebreak
                        yield text "ORDER BY"
                        yield ws
                        yield! orderBy |> Seq.map this.OrderingTerm |> join ","
                }
            | _ ->
                base.Select(select) // Our override of LIMIT will turn this into an offset/fetch clause
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
    override this.ConstraintName(table, constr) =
        // constraint names must be unique across the db, so qualify w/ table name
        table.ObjectName + "_" + constr
    override this.PrimaryKeyClause(pk) =
        seq {
            yield text "PRIMARY KEY"
            if pk.AutoIncrement then
                yield ws
                yield text "IDENTITY(1,1)"
        }
    override this.ColumnDefinition(table, col) =
        seq {
            yield this.Expr.Name(col.Name)
            yield ws
            yield! this.Expr.TypeName(col.Type)
            if not col.Nullable then
                yield ws
                yield text "NOT NULL"
            match col.Collation with
            | None -> ()
            | Some collation ->
                yield ws
                yield text "COLLATE"
                yield ws
                yield text collation.Value // N.B. not wrapped in .Name -- TSQL doesn't like [collation name]
            match col.DefaultValue with
            | None -> ()
            | Some defaultValue ->
                yield ws
                yield text "CONSTRAINT"
                yield ws
                yield this.Expr.Name(defaultConstraint table.ObjectName col.Name)
                yield ws
                yield text "DEFAULT"
                yield ws
                yield! this.Expr.Expr(defaultValue, FirstClassValue)
            yield!
                col.Constraints
                |> Seq.collect (fun constr -> seq { yield linebreak; yield! this.ColumnConstraint(table, constr) })
                |> indent
        }
    override this.CreateTable(create) =
        seq {
            match create.As with
            | CreateAsSelect select ->
                yield text "SELECT * INTO"
                yield ws
                yield! this.Expr.ObjectName(create.Name)
                yield ws
                yield text "FROM ("
                yield! this.Select(select) |> indent
                yield text ") __rzsubquery"
            | CreateAsDefinition def ->
                yield text "CREATE TABLE"
                yield ws
                yield! this.Expr.ObjectName(create.Name)
                yield linebreak
                yield! this.CreateTableDefinition(create.Name, def)
        }
    override this.Update(update) =
        match update.Or with
        | None ->
            base.Update(update)
        | Some _ ->
            failAt update.UpdateTable.Source "UPDATE OR clause is not supported in TSQL"
    override this.Insert(insert) =
        match insert.Or with
        | None ->
            base.Insert(insert)
        | Some _ ->
            failAt insert.InsertInto.Source "INSERT OR clause is not supported in TSQL"
    override this.ForeignKeyOnDelete(handler) =
        seq {
            yield text "ON"
            yield ws
            yield text "DELETE"
            yield ws
            yield
                match handler with
                | SetNull -> text "SET NULL"
                | SetDefault -> text "SET DEFAULT"
                | Cascade -> text "CASCADE"
                | Restrict -> fail "RESTRICT is not supported in TSQL"
                | NoAction -> text "NO ACTION"
        }
    member private this.AlterColumn(columnName : Name, typeName : TypeName, nullable, collation : Name option) =
        seq {
            yield text "ALTER COLUMN"
            yield ws
            yield this.Expr.Name(columnName)
            yield ws
            yield! this.Expr.TypeName(typeName)
            yield ws
            if nullable then
                yield text "NULL"
            else
                yield text "NOT NULL"
            match collation with
            | None -> ()
            | Some collation ->
                yield ws
                yield text "COLLATE"
                yield ws
                yield text collation.Value // N.B. not wrapped in .Name -- TSQL doesn't like [collation name]
        }
    override this.AlterTable(alter) =
        seq {
            yield text "ALTER TABLE"
            yield ws
            yield! this.Expr.ObjectName(alter.Table)
            yield ws
            match alter.Alteration with
            | RenameTo _ ->
                fail <| Error.backendDoesNotSupportFeature "TSQL" "ALTER TABLE RENAME TO"
            | AddColumn columnDef ->
                yield text "ADD" // no COLUMN keyword
                yield ws
                yield! this.ColumnDefinition(alter.Table, columnDef.Value)
            | AddConstraint constr ->
                yield text "ADD"
                yield ws
                yield! this.TableConstraint(alter.Table, constr.Value) // includes CONSTRAINT keyword
            | AddDefault (name, defaultValue) ->
                yield text "ADD CONSTRAINT"
                yield ws
                yield this.Expr.Name(defaultConstraint alter.Table.ObjectName name)
                yield ws
                yield text "DEFAULT"
                yield ws
                yield! this.Expr.Expr(defaultValue, FirstClassValue)
                yield ws
                yield text "FOR"
                yield ws
                yield this.Expr.Name(name)
            | DropColumn name ->
                yield text "DROP COLUMN" // yes COLUMN keyword, yay for consistency
                yield ws
                yield this.Expr.Name(name)
            | DropConstraint constr ->
                yield text "DROP CONSTRAINT"
                yield ws
                yield this.Expr.Name(this.ConstraintName(alter.Table, constr))
            | DropDefault col ->
                yield text "DROP CONSTRAINT"
                yield ws
                yield this.Expr.Name(defaultConstraint alter.Table.ObjectName col)
            | ChangeType change ->
                let schemaColumn = change.ExistingInfo.Column |> Option.get
                yield!
                    this.AlterColumn
                        (change.Column, change.NewType, schemaColumn.ColumnType.Nullable, schemaColumn.Collation)
            | ChangeNullability change ->
                let schemaColumn = change.ExistingInfo.Column |> Option.get
                yield!
                    this.AlterColumn
                        (change.Column, schemaColumn.ColumnTypeName, change.NewNullable, schemaColumn.Collation)
            | ChangeCollation change ->
                let schemaColumn = change.ExistingInfo.Column |> Option.get
                yield!
                    this.AlterColumn
                        ( change.Column
                        , schemaColumn.ColumnTypeName
                        , schemaColumn.ColumnType.Nullable
                        , Some change.NewCollation
                        )
        }

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
        member this.MigrationBackend = <@ fun conn -> new TSQLMigrationBackend(conn) :> IMigrationBackend @>
        member this.InitialModel = initialModel
        member this.ParameterTransform(columnType) = ParameterTransform.Default(columnType)
        member this.ToCommandFragments(indexer, stmts) =
            let translator = TSQLStatement(indexer)
            translator.TotalStatements(stmts)
            |> BackendUtilities.simplifyFragments
            |> ResizeArray
            :> _ IReadOnlyList
       