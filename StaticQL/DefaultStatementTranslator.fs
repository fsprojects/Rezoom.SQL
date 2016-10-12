namespace StaticQL.Translators
open StaticQL
open StaticQL.Mapping
open StaticQL.BackendUtilities

type DefaultStatementTranslator(indexer : IParameterIndexer) =
    inherit StatementTranslator()
    override this.Expr = upcast DefaultExprTranslator(this, indexer)
    override __.OrderDirection(dir) =
        match dir with
        | Ascending -> text "ASC"
        | Descending -> text "DESC"
    override this.IndexHint(indexHint) =
        seq {
            match indexHint with
            | NotIndexed ->
                yield text "NOT INDEXED"
            | (IndexedBy name) ->
                yield text "INDEXED BY"
                yield ws
                yield this.Expr.Name(name)
        }
    member this.QualifiedTableName(qualified : TQualifiedTableName) =
        seq {
            yield! this.Expr.ObjectName(qualified.TableName)
            match qualified.IndexHint with
            | None -> ()
            | Some indexHint ->
                yield ws
                yield! this.IndexHint(indexHint)
        }
    override this.CTE(cte) =
        seq {
            yield this.Expr.Name(cte.Name)
            yield ws
            match cte.ColumnNames with
            | None -> ()
            | Some names ->
                yield text "("
                yield! names.Value |> Seq.map this.Expr.Name |> join1 ", "
                yield text ") "
            yield text "AS ("
            yield! this.Select(cte.AsSelect)
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
            yield!
                vals |> Seq.map (fun row ->
                    seq {
                        yield text "("
                        yield! row.Value |> Seq.map this.Expr.Expr |> join ","
                        yield text ")"
                    }) |> join ","
        }

    override this.ResultColumn(col) =
        match col with
        | ColumnsWildcard -> text "*" |> Seq.singleton
        | TableColumnsWildcard name ->
            seq {
                yield this.Expr.Name(name)
                yield text ".*"
            }
        | Column (expr, alias) ->
            seq {
                yield! this.Expr.Expr(expr)
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
            | None
            | Some AllColumns -> ()
            | Some DistinctColumns -> yield text "DISTINCT"; yield ws
            yield! cols.Columns |> Seq.map (fun c -> this.ResultColumn(c.Value)) |> join ","
        }
    override this.TableOrSubquery(tbl) =
        seq {
            match tbl.Table with
            | Table (table, indexHint) ->
                yield! this.Expr.Table(table)
                match tbl.Alias with
                | None -> ()
                | Some alias ->
                    yield ws
                    yield text "AS"
                    yield ws
                    yield this.Expr.Name(alias)
                match indexHint with
                | None -> ()
                | Some indexHint ->
                    yield ws
                    yield! this.IndexHint(indexHint)
            | Subquery select ->
                yield text "("
                yield! this.Select(select)
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
            yield ws
            yield this.JoinType(join.JoinType)
            yield ws
            yield! this.TableExpr(join.RightTable)
            match join.Constraint with
            | JoinOn expr ->
                yield ws
                yield text "ON"
                yield ws
                yield! this.Expr.Expr(expr)
            | JoinUsing names ->
                yield ws 
                yield text "USING"
                yield ws
                yield text "("
                yield! names |> Seq.map this.Expr.Name |> join1 ","
                yield text ")"
            | JoinUnconstrained -> ()
        }
    override this.SelectCore(select) =
        seq {
            yield text "SELECT"
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
                yield! this.Expr.Expr(where)
            match select.GroupBy with
            | None -> ()
            | Some groupBy ->
                yield ws
                yield text "GROUP BY"
                yield ws
                yield! groupBy.By |> Seq.map this.Expr.Expr |> join ","
                match groupBy.Having with
                | None -> ()
                | Some having ->
                    yield ws
                    yield text "HAVING"
                    yield ws
                    yield! this.Expr.Expr(having)
        }
    override this.CompoundTerm(compound) =
        match compound with
        | Values vals -> this.Values(vals)
        | Select select -> this.SelectCore(select)
    override this.Compound(compound) =
        let op name (expr : TCompoundExpr) (term : TCompoundTerm) =
            seq {
                yield! this.Compound(expr.Value)
                yield ws
                yield text name
                yield ws
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
            yield! this.Expr.Expr(limit.Limit)
            match limit.Offset with
            | None -> ()
            | Some offset ->
                yield ws
                yield text "OFFSET"
                yield ws
                yield! this.Expr.Expr(offset)
        }
    override this.OrderingTerm(term) =
        seq {
            yield! this.Expr.Expr(term.By)
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
                yield ws
            yield! this.Compound(select.Compound.Value)
            match select.OrderBy with
            | None -> ()
            | Some orderBy ->
                yield ws
                yield text "ORDER BY"
                yield ws
                yield! orderBy |> Seq.map this.OrderingTerm |> join ","
            match select.Limit with
            | None -> ()
            | Some limit ->
                yield ws
                yield! this.Limit(limit)
        }
    override this.ConflictClause(clause) =
        seq {
            yield text "ON CONFLICT"
            yield ws
            yield
                match clause with
                | Rollback -> text "ROLLBACK"
                | Abort -> text "ABORT"
                | Fail -> text "FAIL"
                | Ignore -> text "IGNORE"
                | Replace -> text "REPLACE"
        }
    member this.ConflictClause(clause : ConflictClause option) =
        seq {
            match clause with
            | None -> ()
            | Some clause ->
                yield ws
                yield! this.ConflictClause(clause)
        }
    override this.ForeignKeyRule(rule) =
        seq {
            match rule with
            | MatchRule name ->
                yield text "MATCH"
                yield ws
                yield this.Expr.Name(name)
            | EventRule (evt, handler) ->
                yield text "ON"
                yield ws
                yield
                    match evt with
                    | OnDelete -> text "DELETE"
                    | OnUpdate -> text "UPDATE"
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
            match clause.ReferencesColumns with
            | None -> ()
            | Some columns ->
                yield ws
                yield text "("
                yield! columns |> Seq.map this.Expr.Name |> join1 ","
                yield text ")"
            for rule in clause.Rules do
                yield ws
                yield! this.ForeignKeyRule(rule)
            match clause.Defer with
            | None -> ()
            | Some defer ->
                if not defer.Deferrable then
                    yield text "NOT"
                    yield ws
                yield text "DEFERRABLE"
                match defer.InitiallyDeferred with
                | None -> ()
                | Some initially ->
                    yield ws
                    yield text "INITIALLY"
                    yield ws
                    yield text (if initially then "DEFERRED" else "IMMEDIATE")
        }
    override this.ColumnConstraint(constr) =
        seq {
            match constr.Name with
            | None -> ()
            | Some name ->
                yield text "CONSTRAINT"
                yield ws
                yield this.Expr.Name(name)
                yield ws
            match constr.ColumnConstraintType with
            | NullableConstraint -> ()
            | PrimaryKeyConstraint pk ->
                yield text "PRIMARY KEY"
                yield ws
                yield this.OrderDirection(pk.Order)
                yield! this.ConflictClause(pk.ConflictClause)
                if pk.AutoIncrement then
                    yield ws
                    yield text "AUTOINCREMENT"
            | NotNullConstraint conflict ->
                yield text "NOT NULL"
                yield! this.ConflictClause(conflict)
            | UniqueConstraint conflict ->
                yield text "UNIQUE"
                yield! this.ConflictClause(conflict)
            | CheckConstraint expr ->
                yield text "CHECK("
                yield! this.Expr.Expr(expr)
                yield text ")"
            | DefaultConstraint expr ->
                yield text "DEFAULT("
                yield! this.Expr.Expr(expr)
                yield text ")"
            | CollateConstraint name ->
                yield text "COLLATE"
                yield ws
                yield this.Expr.Name(name)
            | ForeignKeyConstraint fk ->
                yield! this.ForeignKeyClause(fk)
        }
    override this.ColumnDefinition(col) =
        seq {
            yield this.Expr.Name(col.Name)
            yield ws
            yield! this.Expr.TypeName(col.Type)
            for constr in col.Constraints do
                yield ws
                yield! this.ColumnConstraint(constr)
        }
    override this.CreateTableDefinition(create) =
        seq {
            yield text "("
            yield! create.Columns |> Seq.map this.ColumnDefinition |> join ","
            yield text ")"
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
            if create.IfNotExists then
                yield text "IF NOT EXISTS"
                yield ws
            yield! this.Expr.ObjectName(create.Name)
            yield ws
            match create.As with
            | CreateAsSelect select ->
                yield text "AS"
                yield ws
                yield! this.Select(select)
            | CreateAsDefinition def ->
                yield! this.CreateTableDefinition(def)
        }
    override this.AlterTable(alter) =
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
                yield! this.ColumnDefinition(columnDef)
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
            if create.IfNotExists then
                yield text "IF NOT EXISTS"
                yield ws
            yield! this.Expr.ObjectName(create.ViewName)
            yield ws
            match create.ColumnNames with
            | None -> ()
            | Some names ->
                yield text "("
                yield! names |> Seq.map this.Expr.Name |> join1 ","
                yield text ")"
                yield ws
            yield text "AS"
            yield ws
            yield! this.Select(create.AsSelect)
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
            if create.IfNotExists then
                yield text "IF NOT EXISTS"
                yield ws
            yield! this.Expr.ObjectName(create.IndexName)
            yield ws
            yield text "ON"
            yield ws
            yield! this.Expr.ObjectName(create.TableName)
            yield text "("
            yield!
                seq {
                    for expr, dir in create.IndexedColumns ->
                        seq {
                            yield! this.Expr.Expr(expr)
                            yield ws
                            yield this.OrderDirection(dir)
                        }
                } |> join ","
            yield text ")"
            match create.Where with
            | None -> ()
            | Some where ->
                yield ws
                yield text "WHERE"
                yield ws
                yield! this.Expr.Expr(where)
        }
    override this.DropObject(drop) =
        seq {
            yield text "DROP"
            yield ws
            yield
                match drop.Drop with
                | DropIndex -> text "INDEX"
                | DropTable -> text "TABLE"
                | DropTrigger -> text "TRIGGER"
                | DropView -> text "VIEW"
            yield ws
            if drop.IfExists then
                yield text "IF EXISTS"
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
            match insert.Columns with
            | None -> ()
            | Some columns ->
                yield text "("
                yield! columns |> Seq.map this.Expr.Name |> join1 ","
                yield text ")"
            yield ws
            match insert.Data with
            | None -> yield text "DEFAULT VALUES"
            | Some data ->
                yield! this.Select(data)
        }
    override this.Update(update) =
        seq {
            match update.With with
            | None -> ()
            | Some withClause ->
                yield! this.With(withClause)
                yield ws
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
            yield! this.QualifiedTableName(update.UpdateTable)
            yield ws
            yield text "SET"
            yield ws
            yield!
                seq {
                    for name, value in update.Set ->
                        seq {
                            yield this.Expr.Name(name)
                            yield ws
                            yield text "="
                            yield ws
                            yield! this.Expr.Expr(value)
                        }
                } |> join ","
            match update.Where with
            | None -> ()
            | Some where ->
                yield ws
                yield text "WHERE"
                yield ws
                yield! this.Expr.Expr(where)
            match update.OrderBy with
            | None -> ()
            | Some orderBy ->
                yield ws
                yield text "ORDER BY"
                yield ws
                yield! orderBy |> Seq.map this.OrderingTerm |> join ","
            match update.Limit with
            | None -> ()
            | Some limit ->
                yield ws
                yield! this.Limit(limit)
        }
    override this.Delete(delete) =
        seq {
            match delete.With with
            | None -> ()
            | Some withClause ->
                yield! this.With(withClause)
                yield ws
            yield text "DELETE FROM"
            yield ws
            yield! this.QualifiedTableName(delete.DeleteFrom)
            match delete.Where with
            | None -> ()
            | Some where ->
                yield ws
                yield text "WHERE"
                yield ws
                yield! this.Expr.Expr(where)
            match delete.OrderBy with
            | None -> ()
            | Some orderBy ->
                yield ws
                yield text "ORDER BY"
                yield ws
                yield! orderBy |> Seq.map this.OrderingTerm |> join ","
            match delete.Limit with
            | None -> ()
            | Some limit ->
                yield ws
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
        | CreateTriggerStmt create -> failwith "not implemented: CREATE TRIGGER statements" // TODO
        | DropObjectStmt drop -> this.DropObject(drop)

        | SelectStmt select -> this.Select(select)
        | InsertStmt insert -> this.Insert(insert)
        | UpdateStmt update -> this.Update(update)
        | DeleteStmt delete -> this.Delete(delete)

        | BeginStmt -> this.Begin
        | CommitStmt -> this.Commit
        | RollbackStmt -> this.Rollback
    override this.Statements(stmts) =
        seq {
            for stmt in stmts do
                yield! this.Statement(stmt)
                yield text ";"
        }

