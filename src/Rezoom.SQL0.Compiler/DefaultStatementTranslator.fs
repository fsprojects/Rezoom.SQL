namespace Rezoom.SQL.Compiler.Translators
open Rezoom.SQL.Compiler
open Rezoom.SQL.Compiler.BackendUtilities
open Rezoom.SQL.Mapping

type DefaultStatementTranslator(expectedVendorName : Name, indexer : IParameterIndexer) =
    inherit StatementTranslator()
    override this.Expr = upcast DefaultExprTranslator(this, indexer)
    member this.Predicate(x) = this.Expr.Expr(x, Predicate)
    member this.FirstClassValue(x) = this.Expr.Expr(x, FirstClassValue)
    override __.OrderDirection(dir) =
        match dir with
        | Ascending -> text "ASC"
        | Descending -> text "DESC"
    override this.CTE(cte) =
        seq {
            yield this.Expr.Name(cte.Name)
            yield linebreak
            match cte.ColumnNames with
            | None -> ()
            | Some names ->
                yield text "("
                yield! names.Value |> Seq.map (srcValue >> this.Expr.Name) |> joinLines1 "," |> indent
                yield text ") "
            yield text "AS ("
            yield linebreak
            yield! this.Select(cte.AsSelect) |> indent
            yield linebreak
            yield text ")"
        }
    override this.With(withClause) =
        seq {
            yield text "WITH"
            yield ws
            if withClause.Recursive then
                yield text "RECURSIVE"
                yield ws
            yield! withClause.Tables |> Seq.map this.CTE |> join ","
        }
    override this.Values(vals) =
        seq {
            yield text "VALUES"
            yield linebreak
            yield!
                vals
                |> Seq.map (fun row ->
                    seq {
                        yield text "("
                        yield! row.Value |> Seq.map this.FirstClassValue |> join ","
                        yield text ")"
                    })
                |> joinLines ","
                |> indent
        }

    override this.ResultColumn(expr, alias) =
        seq {
            yield! this.FirstClassValue(expr)
            match alias with
            | None -> ()
            | Some alias ->
                yield ws
                yield text "AS"
                yield ws
                yield this.Expr.Name(alias)
        }
    override this.ResultColumns(cols) =
        seq {
            match cols.Distinct with
            | None -> ()
            | Some Distinct -> yield text "DISTINCT"; yield ws
            yield!
                seq {
                    for col in cols.Columns do
                        match col.Case with
                        | Column(expr, alias) ->
                            yield this.ResultColumn(expr, alias)
                        | ColumnNav _ ->
                            bug "Bug in typechecker: nav columns should've been expanded"
                        | _ ->
                            bug "Bug in typechecker: wildcards should've been expanded"
                } |> joinLines ","
        }
    override this.TableOrSubquery(tbl) =
        seq {
            match tbl.Table with
            | Table table ->
                yield! this.Expr.Table(table)
                match tbl.Alias with
                | None -> ()
                | Some alias ->
                    yield ws
                    yield text "AS"
                    yield ws
                    yield this.Expr.Name(alias)
            | Subquery select ->
                yield text "("
                yield! this.Select(select) |> indent
                yield text ")"
                match tbl.Alias with
                | None -> ()
                | Some alias ->
                    yield ws
                    yield text "AS"
                    yield ws
                    yield this.Expr.Name(alias)
        }
    override this.TableExpr(texpr) =
        match texpr.Value with
        | TableOrSubquery tbl -> this.TableOrSubquery(tbl)
        | Join join -> this.Join(join)
    override __.JoinType(join) =
        let rec joinText join =
            match join with
            | Inner -> "INNER JOIN"
            | LeftOuter -> "LEFT OUTER JOIN"
            | Cross -> "CROSS JOIN"
            | Natural ty -> "NATURAL " + joinText ty
        joinText join |> text
    override this.Join(join) =
        seq {
            yield! this.TableExpr(join.LeftTable)
            yield linebreak
            yield this.JoinType(join.JoinType)
            yield linebreak
            yield! this.TableExpr(join.RightTable)
            match join.Constraint with
            | JoinOn expr ->
                yield ws
                yield text "ON"
                yield ws
                yield! this.Predicate(expr)
            | JoinUnconstrained -> ()
        }
    override this.SelectCore(select) =
        seq {
            yield text "SELECT"
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
    override this.CompoundTerm(compound) =
        match compound with
        | Values vals -> this.Values(vals)
        | Select select -> this.SelectCore(select)
    override this.Compound(compound) =
        let op name (expr : TCompoundExpr) (term : TCompoundTerm) =
            seq {
                yield! this.Compound(expr.Value)
                yield linebreak
                yield text name
                yield linebreak
                yield! this.CompoundTerm(term.Value)
            }
        match compound with
        | CompoundTerm term -> this.CompoundTerm(term.Value)
        | Union (expr, term) -> op "UNION" expr term
        | UnionAll (expr, term) -> op "UNION ALL" expr term
        | Intersect (expr, term) -> op "INTERSECT" expr term
        | Except (expr, term) -> op "EXCEPT" expr term
    override this.Limit(limit) =
        seq {
            yield text "LIMIT"
            yield ws
            yield! this.FirstClassValue(limit.Limit)
            match limit.Offset with
            | None -> ()
            | Some offset ->
                yield ws
                yield text "OFFSET"
                yield ws
                yield! this.FirstClassValue(offset)
        }
    override this.OrderingTerm(term) =
        seq {
            yield! this.FirstClassValue(term.By)
            yield ws
            yield this.OrderDirection(term.Direction)
        }
    override this.Select(select) =
        let select = select.Value
        seq {
            match select.With with
            | None -> ()
            | Some withClause ->
                yield! this.With(withClause)
                yield linebreak
            yield! this.Compound(select.Compound.Value)
            match select.OrderBy with
            | None -> ()
            | Some orderBy ->
                yield linebreak
                yield text "ORDER BY"
                yield ws
                yield! orderBy |> Seq.map this.OrderingTerm |> join ","
            match select.Limit with
            | None -> ()
            | Some limit ->
                yield linebreak
                yield! this.Limit(limit)
        }
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
                | Restrict -> text "RESTRICT"
                | NoAction -> text "NO ACTION"
        }
    override this.ForeignKeyClause(clause) =
        seq {
            yield text "REFERENCES"
            yield ws
            yield! this.Expr.ObjectName(clause.ReferencesTable)
            yield ws
            yield text "("
            yield! clause.ReferencesColumns |> Seq.map (srcValue >> this.Expr.Name) |> join1 ","
            yield text ")"
            match clause.OnDelete with
            | None -> ()
            | Some onDelete ->
                yield ws
                yield! this.ForeignKeyOnDelete(onDelete)
        }
    abstract member IndexedColumn : name : Name * dir : OrderDirection -> Fragments
    default this.IndexedColumn(name, dir) =
        seq {
            yield this.Expr.Name(name)
            yield ws
            yield this.OrderDirection(dir)
        }
    abstract member PrimaryKeyClause : pk : PrimaryKeyClause -> Fragments
    default this.PrimaryKeyClause(pk) =
        seq {
            yield text "PRIMARY KEY"
            yield ws
            yield this.OrderDirection(pk.Order)
            if pk.AutoIncrement then
                yield ws
                yield text "AUTOINCREMENT"
        }
    override this.ColumnConstraint(_, constr) =
        seq {
            yield text "CONSTRAINT"
            yield ws
            yield this.Expr.Name(constr.Name)
            yield ws
            match constr.ColumnConstraintType with
            | PrimaryKeyConstraint pk ->
                yield! this.PrimaryKeyClause(pk)
            | UniqueConstraint ->
                yield text "UNIQUE"
            | ForeignKeyConstraint fk ->
                yield! this.ForeignKeyClause(fk)
        }
    override this.TableConstraint(_, constr) =
        seq {
            yield text "CONSTRAINT"
            yield ws
            yield this.Expr.Name(constr.Name)
            yield ws
            match constr.TableConstraintType with
            | TableIndexConstraint indexClause ->
                yield
                    text <|
                    match indexClause.Type with
                    | PrimaryKey -> "PRIMARY KEY"
                    | Unique -> "UNIQUE"
                yield text "("
                yield! indexClause.IndexedColumns |> Seq.map (fun v -> this.IndexedColumn(v.Value)) |> join ","
                yield text ")"
            | TableForeignKeyConstraint (names, references) ->
                yield text "FOREIGN KEY("
                yield! names |> Seq.map (fun n -> this.Expr.Name(n.Value)) |> join1 ","
                yield text ")"
                yield! this.ForeignKeyClause(references)
            | TableCheckConstraint ex ->
                yield text "CHECK("
                yield! this.Predicate(ex)
                yield text ")"
        }
    abstract member ColumnsNullableByDefault : bool
    default __.ColumnsNullableByDefault = false
    override this.ColumnDefinition(table, col) =
        seq {
            yield this.Expr.Name(col.Name)
            yield ws
            yield! this.Expr.TypeName(col.Type, col.IsAutoIncrementPrimaryKey)
            if this.ColumnsNullableByDefault && not col.Nullable then
                yield ws
                yield text "NOT NULL"
            elif not this.ColumnsNullableByDefault && col.Nullable then
                yield ws
                yield text "NULL"
            match col.Collation with
            | None -> ()
            | Some collation ->
                yield ws
                yield text "COLLATE"
                yield ws
                yield this.Expr.CollationName(collation)
            match col.DefaultValue with
            | None -> ()
            | Some defaultValue ->
                yield ws
                yield text "DEFAULT"
                yield ws
                yield text "("
                yield! this.Expr.Expr(defaultValue, FirstClassValue)
                yield text ")"
            yield!
                col.Constraints
                |> Seq.collect (fun constr -> seq { yield linebreak; yield! this.ColumnConstraint(table, constr) })
                |> indent
        }
    override this.CreateTableDefinition(table, create) =
        seq {
            let columns = create.Columns |> Seq.map (fun c -> this.ColumnDefinition(table, c.Value))
            let constraints = create.Constraints |> Seq.map (fun c -> this.TableConstraint(table, c.Value))
            yield! Seq.append columns constraints |> parencols
        }
    override this.CreateTable(create) =
        seq {
            yield text "CREATE"
            yield ws
            if create.Temporary then
                yield text "TEMP"
                yield ws
            yield text "TABLE"
            yield ws
            yield! this.Expr.ObjectName(create.Name)
            yield ws
            match create.As with
            | CreateAsSelect select ->
                yield text "AS"
                yield linebreak
                yield! this.Select(select)
            | CreateAsDefinition def ->
                yield linebreak
                yield! this.CreateTableDefinition(create.Name, def) |> indent
        }
    override this.AlterTable(alter) =
        seq {
            yield text "ALTER TABLE"
            yield ws
            yield! this.Expr.ObjectName(alter.Table)
            yield linebreak
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
            | AddDefault (name, defaultValue) ->
                yield text "ADD DEFAULT FOR"
                yield ws
                yield this.Expr.Name(name)
                yield ws
                yield text "("
                yield! this.Expr.Expr(defaultValue, FirstClassValue)
                yield text ")"
            | DropColumn name ->
                yield text "DROP COLUMN"
                yield ws
                yield this.Expr.Name(name)
            | DropConstraint name ->
                yield text "DROP CONSTRAINT"
                yield ws
                yield this.Expr.Name(name)
            | DropDefault columnName ->
                yield text "DROP DEFAULT FOR"
                yield ws
                yield this.Expr.Name(columnName)
            | ChangeType change ->
                yield text "ALTER COLUMN"
                yield ws
                yield this.Expr.Name(change.Column)
                yield ws
                yield! this.Expr.TypeName(change.NewType)
            | ChangeNullability change ->
                yield text "ALTER COLUMN"
                yield ws
                yield this.Expr.Name(change.Column)
                yield ws
                yield text (if change.NewNullable then "NULL" else "NOT NULL")
            | ChangeCollation change ->
                yield text "ALTER COLUMN"
                yield ws
                yield this.Expr.Name(change.Column)
                yield ws
                yield text "COLLATE"
                yield ws
                yield this.Expr.CollationName(change.NewCollation)
        }
    override this.CreateView(create) =
        seq {
            yield text "CREATE"
            yield ws
            if create.Temporary then
                yield text "TEMP"
                yield ws
            yield text "VIEW"
            yield ws
            yield! this.Expr.ObjectName(create.ViewName)
            yield linebreak
            match create.ColumnNames with
            | None -> ()
            | Some names ->
                yield! names |> Seq.map (srcValue >> this.Expr.Name) |> parencols1
                yield linebreak
            yield text "AS"
            yield linebreak
            yield! this.Select(create.AsSelect) |> indent
        }
    override this.CreateIndex(create) =
        seq {
            yield text "CREATE"
            yield ws
            if create.Unique then
                yield text "UNIQUE"
                yield ws
            yield text "INDEX"
            yield ws
            yield! this.Expr.ObjectName(create.IndexName)
            yield ws
            yield text "ON"
            yield ws
            yield! this.Expr.ObjectName(create.TableName)
            yield linebreak
            yield! create.IndexedColumns |> Seq.map (fun w -> this.IndexedColumn(w.Value)) |> parencols
            match create.Where with
            | None -> ()
            | Some where ->
                yield linebreak
                yield text "WHERE"
                yield ws
                yield! this.Predicate(where)
        }
    override this.DropObject(drop) =
        seq {
            yield text "DROP"
            yield ws
            yield
                match drop.Drop with
                | DropIndex -> text "INDEX"
                | DropTable -> text "TABLE"
                | DropView -> text "VIEW"
            yield ws
            yield! this.Expr.ObjectName(drop.ObjectName)
        }
    override this.Insert(insert) =
        seq {
            match insert.With with
            | None -> ()
            | Some withClause ->
                yield! this.With(withClause)
                yield ws
            yield text "INSERT"
            match insert.Or with
            | None -> ()
            | Some insertOr ->
                yield ws
                yield
                    match insertOr with
                    | InsertOrRollback -> text "OR ROLLBACK"
                    | InsertOrAbort -> text "OR ABORT"
                    | InsertOrReplace -> text "OR REPLACE"
                    | InsertOrFail -> text "OR FAIL"
                    | InsertOrIgnore -> text "OR IGNORE"
            yield ws
            yield text "INTO"
            yield ws
            yield! this.Expr.ObjectName(insert.InsertInto)
            yield linebreak
            yield text "("
            yield! insert.Columns |> Seq.map (srcValue >> this.Expr.Name) |> join1 "," |> indent
            yield text ")"
            yield linebreak
            yield! this.Select(insert.Data)
        }
    override this.Update(update) =
        seq {
            match update.With with
            | None -> ()
            | Some withClause ->
                yield! this.With(withClause)
                yield linebreak
            yield text "UPDATE"
            match update.Or with
            | None -> ()
            | Some updateOr ->
                yield ws
                yield
                    match updateOr with
                    | UpdateOrRollback -> text "OR ROLLBACK"
                    | UpdateOrAbort -> text "OR ABORT"
                    | UpdateOrReplace -> text "OR REPLACE"
                    | UpdateOrFail -> text "OR FAIL"
                    | UpdateOrIgnore -> text "OR IGNORE"
            yield ws
            yield! this.Expr.ObjectName(update.UpdateTable)
            yield ws
            yield text "SET"
            yield linebreak
            yield!
                seq {
                    for name, value in update.Set ->
                        seq {
                            yield this.Expr.Name(name.Value)
                            yield ws
                            yield text "="
                            yield ws
                            yield! this.FirstClassValue(value)
                        }
                } |> join "," |> indent
            match update.Where with
            | None -> ()
            | Some where ->
                yield linebreak
                yield text "WHERE"
                yield ws
                yield! this.Predicate(where) |> indent
            match update.OrderBy with
            | None -> ()
            | Some orderBy ->
                yield linebreak
                yield text "ORDER BY"
                yield ws
                yield! orderBy |> Seq.map this.OrderingTerm |> join ","
            match update.Limit with
            | None -> ()
            | Some limit ->
                yield linebreak
                yield! this.Limit(limit)
        }
    override this.Delete(delete) =
        seq {
            match delete.With with
            | None -> ()
            | Some withClause ->
                yield! this.With(withClause)
                yield linebreak
            yield text "DELETE FROM"
            yield ws
            yield! this.Expr.ObjectName(delete.DeleteFrom)
            match delete.Where with
            | None -> ()
            | Some where ->
                yield linebreak
                yield text "WHERE"
                yield ws
                yield! this.Predicate(where) |> indent
            match delete.OrderBy with
            | None -> ()
            | Some orderBy ->
                yield linebreak
                yield text "ORDER BY"
                yield ws
                yield! orderBy |> Seq.map this.OrderingTerm |> join ","
            match delete.Limit with
            | None -> ()
            | Some limit ->
                yield linebreak
                yield! this.Limit(limit)
        }
    override this.Begin = Seq.singleton (text "BEGIN")
    override this.Commit = Seq.singleton (text "COMMIT")
    override this.Rollback = Seq.singleton (text "ROLLBACK")
    override this.Statement(stmt) =
        match stmt with
        | AlterTableStmt alter -> this.AlterTable(alter)
        | CreateTableStmt create -> this.CreateTable(create)
        | CreateViewStmt create -> this.CreateView(create)
        | CreateIndexStmt create -> this.CreateIndex(create)
        | DropObjectStmt drop -> this.DropObject(drop)

        | SelectStmt select -> this.Select(select)
        | InsertStmt insert -> this.Insert(insert)
        | UpdateStmt update -> this.Update(update)
        | DeleteStmt delete -> this.Delete(delete)

    override this.Statements(stmts) =
        seq {
            for stmt in stmts do
                yield! this.Statement(stmt)
                yield text ";"
                yield linebreak
        }
    override this.Vendor(vendor) =
        if expectedVendorName <> vendor.VendorName.Value then
            failAt vendor.VendorName.Source <|
                Error.vendorMismatch vendor.VendorName.Value expectedVendorName
        seq {
            for fragment in vendor.Fragments do
                match fragment with
                | VendorRaw raw -> yield text raw
                | VendorEmbeddedExpr expr -> yield! this.FirstClassValue(expr)
        }
    override this.TotalStatement(stmt) =
        match stmt with
        | CoreStmt stmt -> this.Statement(stmt)
        | VendorStmt vendor -> this.Vendor(vendor)
    override this.TotalStatements(stmts) =
        seq {
            for stmt in stmts do
                yield! this.TotalStatement(stmt)
                yield text ";"
                yield linebreak
        }

