namespace Rezoom.SQL.Compiler.TSQL
open Rezoom.SQL.Compiler
open Rezoom.SQL.Compiler.BackendUtilities
open Rezoom.SQL.Compiler.Translators

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
        seq {
            yield text TSQLMigrationBackend.BatchSeparator
            yield text "DROP"
            yield ws
            yield
                match drop.Drop with
                | DropIndex -> text "INDEX"
                | DropTable -> text "TABLE"
                | DropView -> text "VIEW"
            yield ws
            yield! this.Expr.ObjectName(drop.ObjectName)
            match drop.Drop with
            | DropIndex ->
                match drop.ObjectName.Info with
                | Index idx ->
                    yield ws
                    yield text "ON"
                    yield ws
                    if idx.TableName.SchemaName = Name("temp") then
                        yield text "#"
                        yield this.Expr.Name(idx.TableName.ObjectName)
                    else
                        yield this.Expr.Name(idx.TableName.SchemaName)
                        yield text "."
                        yield this.Expr.Name(idx.TableName.ObjectName)
                | _ -> bug "Typechecker should've validated this object name as an index"
            | _ -> ()
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
                yield text "("
                yield! this.FirstClassValue(top)
                yield text ")"
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
            match col.Collation with
            | None -> ()
            | Some collation -> // collation first on TSQL
                yield ws
                yield text "COLLATE"
                yield ws
                yield this.Expr.CollationName(collation)
            if not col.Nullable then
                yield ws
                yield text "NOT NULL"
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
            match collation with
            | Some collation when typeName.SupportsCollation ->
                yield ws
                yield text "COLLATE"
                yield ws
                yield this.Expr.CollationName(collation)
            | _ -> ()
            yield ws
            if nullable then
                yield text "NULL"
            else
                yield text "NOT NULL"
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
                yield this.Expr.Name(constr)
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