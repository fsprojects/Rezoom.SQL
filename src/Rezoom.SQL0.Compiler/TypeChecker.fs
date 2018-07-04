namespace Rezoom.SQL.Compiler
open System
open System.Collections.Generic
open Rezoom.SQL.Compiler.InferredTypes

type private InferredQueryShape = InferredType QueryExprInfo

[<NoComparison>]
[<NoEquality>]
type private SelfQueryShape =
    // this thing is for when we know ahead of time what the column names of a select statement are supposed to be
    // so we don't want to require that they all be aliased manually.
    {   CTEName : Name option
        KnownShape : InferredQueryShape option
    }
    static member Known(known) = { CTEName = None; KnownShape = known }
    static member Known(known) = SelfQueryShape.Known(Some known)
    static member Unknown = { CTEName = None; KnownShape = None }

type private TypeChecker(cxt : ITypeInferenceContext, scope : InferredSelectScope) as this =
    let exprChecker = ExprTypeChecker(cxt, scope, this)
    member this.ObjectName(name) = exprChecker.ObjectName(name)
    member this.ObjectName(name, allowNotFound) = exprChecker.ObjectName(name, allowNotFound)
    member this.SchemaTableName(name) =
        let name = exprChecker.ObjectName(name)
        match name.Info with
        | TableLike { Table = TableReference _ } -> name
        | _ -> failAt name.Source <| Error.objectNotATable name
    member this.Expr(expr, knownType) = exprChecker.Expr(expr, knownType)
    member this.Expr(expr) = exprChecker.Expr(expr)
    member this.BooleanExpr(expr) = this.Expr(expr, BooleanType)
    member this.Scope = scope
    member this.WithScope(scope) = TypeChecker(cxt, scope)

    member private this.TableOrSubqueryScope(tsub : TableOrSubquery) =
        match tsub.Table with
        | Table tinvoc ->
            tsub.Alias |? tinvoc.Table.ObjectName, this.ObjectName(tinvoc.Table).Info
        | Subquery select ->
            match tsub.Alias with
            | None -> failAt select.Source Error.subqueryRequiresAnAlias
            | Some alias -> alias, this.Select(select, SelfQueryShape.Unknown).Value.Info

    member private this.TableExprScope
        (dict : Dictionary<Name, InferredType ObjectInfo>, texpr : TableExpr, outerDepth) =
        let add name objectInfo =
            if dict.ContainsKey(name) then
                failAt texpr.Source <| Error.tableNameAlreadyInScope name
            else
                dict.Add(name, objectInfo)
        match texpr.Value with
        | TableOrSubquery tsub ->
            let alias, objectInfo = this.TableOrSubqueryScope(tsub)
            let objectInfo =
                if outerDepth > 0 then
                    let nullable = NullableDueToJoin |> Seq.replicate outerDepth |> Seq.reduce (>>)
                    objectInfo.Map(fun t -> { t with InferredNullable = nullable t.InferredNullable })
                else objectInfo
            add alias objectInfo
            outerDepth
        | Join join ->
            let leftDepth = this.TableExprScope(dict, join.LeftTable, outerDepth)
            let depthIncrement = if join.JoinType.IsOuter then 1 else 0
            this.TableExprScope(dict, join.RightTable, leftDepth + depthIncrement)

    member private this.TableExprScope(texpr : TableExpr) =
        let dict = Dictionary()
        ignore <| this.TableExprScope(dict, texpr, outerDepth = 0)
        { FromVariables = dict }

    member private this.TableOrSubquery(tsub : TableOrSubquery) =
        let tbl, info =
            match tsub.Table with
            | Table tinvoc ->
                let invoke = exprChecker.TableInvocation(tinvoc)
                Table invoke, invoke.Table.Info
            | Subquery select ->
                let select = this.Select(select, SelfQueryShape.Unknown)
                Subquery select, select.Value.Info
        {   Table = tbl
            Alias = tsub.Alias
            Info = info
        }

    member private this.TableExpr(constraintChecker : TypeChecker, texpr : TableExpr) =
        {   TableExpr.Source = texpr.Source
            Value =
                match texpr.Value with
                | TableOrSubquery tsub -> TableOrSubquery <| this.TableOrSubquery(tsub)
                | Join join ->
                    {   JoinType = join.JoinType
                        LeftTable = this.TableExpr(constraintChecker, join.LeftTable)
                        RightTable = this.TableExpr(constraintChecker, join.RightTable)
                        Constraint =
                            match join.Constraint with
                            | JoinOn e -> constraintChecker.Expr(e, BooleanType) |> JoinOn
                            | JoinUnconstrained -> JoinUnconstrained
                    } |> Join
        }

    member this.TableExpr(texpr : TableExpr) =
        let checker = TypeChecker(cxt, { scope with FromClause = Some <| this.TableExprScope(texpr) })
        checker, this.TableExpr(checker, texpr)

    member this.ResultColumn(aliasPrefix : Name option, resultColumn : ResultColumn) =
        let qualify (tableAlias : Name) fromTable (col : _ ColumnExprInfo) =
            {   Expr.Source = resultColumn.Source
                Value =
                    {   ColumnName = col.ColumnName
                        Table =
                            {   Source = resultColumn.Source
                                ObjectName = tableAlias
                                SchemaName = None
                                Info = fromTable
                            } |> Some
                    } |> ColumnNameExpr
                Info = col.Expr.Info
            },
                match aliasPrefix with
                | None -> None
                | Some prefix -> Some (prefix + col.ColumnName)
        match resultColumn.Case with
        | ColumnsWildcard ->
            match scope.FromClause with
            | None -> failAt resultColumn.Source Error.wildcardWithoutFromClause
            | Some from ->
                seq {
                    for KeyValue(tableAlias, fromTable) in from.FromVariables do
                    for col in fromTable.Table.Query.Columns do
                        yield qualify tableAlias fromTable col
                }
        | TableColumnsWildcard tbl ->
            match scope.FromClause with
            | None -> failAt resultColumn.Source <| Error.tableWildcardWithoutFromClause tbl
            | Some from ->
                let succ, fromTable = from.FromVariables.TryGetValue(tbl)
                if not succ then failAt resultColumn.Source <| Error.noSuchTableInFrom tbl
                fromTable.Table.Query.Columns |> Seq.map (qualify tbl fromTable)
        | Column (expr, alias) ->
            match aliasPrefix with
            | None -> (this.Expr(expr), alias) |> Seq.singleton
            | Some prefix -> 
                let expr = this.Expr(expr)
                match implicitAlias (expr.Value, alias) with
                | None -> (expr, None) |> Seq.singleton
                | Some a -> (expr, Some (prefix + a)) |> Seq.singleton
        | ColumnNav nav ->
            this.ColumnNav(aliasPrefix, resultColumn, nav)

    member this.ColumnNav(aliasPrefix : Name option, resultColumn : ResultColumn, nav : ResultColumnNav<unit, unit>) =
        let subAliasPrefix =
            let prev =
                match aliasPrefix with
                | Some prefix -> prefix.Value
                | None -> ""
            Some <| Name(prev + nav.Name.Value + nav.Cardinality.Separator)
        let columns =
            seq {
                for column in nav.Columns do
                    let producedColumns = this.ResultColumn(subAliasPrefix, column)
                    yield column, producedColumns |> ResizeArray
            } |> ResizeArray
        let keyExprs =
            seq {
                for source, producedColumns in columns do
                    match source.Case with
                    | ColumnNav _ -> () // ignore sub-nav props
                    | _ ->
                        for expr, _ in producedColumns do
                            if expr.Info.PrimaryKey then yield expr    
            } |> ResizeArray
        if keyExprs.Count <= 0 then
            failAt resultColumn.Source <| Error.navPropertyMissingKeys nav.Name
        else
            let minDepthOfImmediateKey =
                keyExprs
                |> Seq.map (fun e -> e.Info.Type.InferredNullable.JoinInducedNullabilityDepth())
                |> Seq.min
            columns
            |> Seq.collect snd
            |> Seq.map (fun (expr, alias) -> // remove nullability introduced by outer joins
                { expr with
                    Info = { expr.Info with Type = expr.Info.Type.StripNullDueToJoin(minDepthOfImmediateKey) }
                }, alias)
                  
    member this.ResultColumns(resultColumns : ResultColumns, knownShape : InferredQueryShape option) =
        let columns =
            resultColumns.Columns
            |> Seq.collect
                (fun rc ->
                    this.ResultColumn(None, rc)
                    |> Seq.map (fun (expr, alias) -> { Source = rc.Source; Case = Column (expr, alias); }))
            |> Seq.toArray
        match knownShape with
        | Some shape ->
            if columns.Length <> shape.Columns.Count then
                if columns.Length <= 0 then fail "BUG: impossible, parser shouldn't have accepted this"
                let source = columns.[columns.Length - 1].Source
                failAt source <| Error.expectedKnownColumnCount columns.Length shape.Columns.Count
            for i = 0 to columns.Length - 1 do
                let selected, alias = columns.[i].Case.AssumeColumn()
                let shape = shape.Columns.[i]
                cxt.UnifyLeftKnown(selected.Source, shape.Expr.Info.Type, selected.Info.Type)
                match implicitAlias (selected.Value, alias) with
                | Some a when a = shape.ColumnName -> ()
                | _ ->
                    columns.[i] <- { columns.[i] with Case = Column(selected, Some shape.ColumnName) }
        | None ->
            for column in columns do
                let selected, _ = column.Case.AssumeColumn()
                ignore <| cxt.Unify(selected.Source, selected.Info.Type.InferredType, TypeKnown ScalarTypeClass)
        {   Distinct = resultColumns.Distinct
            Columns = columns
        }

    member this.GroupBy(groupBy : GroupBy) =
        {   By = groupBy.By |> rmap this.Expr
            Having = groupBy.Having |> Option.map this.BooleanExpr
        }

    member this.SelectCore(select : SelectCore, knownShape : InferredQueryShape option) =
        let checker, from, staticCount =
            match select.From with
            | None -> this, None, (if Option.isNone select.Where then Some 1 else None)
            | Some from ->
                let checker, texpr = this.TableExpr(from)
                checker, Some texpr, None
        let columns = checker.ResultColumns(select.Columns, knownShape)
        let infoColumns =
            let used = HashSet()
            seq {
                for column in columns.Columns do
                    match column.Case with
                    | Column (expr, alias) ->
                        yield
                            {   Expr = expr
                                FromAlias = None
                                ColumnName =
                                    match implicitAlias (expr.Value, alias) with
                                    | None -> failAt column.Source Error.expressionRequiresAlias
                                    | Some alias ->
                                        if used.Add(alias) then alias
                                        else failAt column.Source (Error.ambiguousColumn alias)
                            }
                     // typechecker should've eliminated alternatives
                     | _ -> bug "All wildcards must be expanded -- this is a typechecker bug"
            } |> toReadOnlyList
        let where, whereIdempotent =
            match select.Where with
            | None -> None, true
            | Some where ->
                let where = checker.BooleanExpr(where)
                Some where, where.Info.Idempotent
        let groupBy, groupByIdempotent =
            match select.GroupBy with
            | None -> None, true
            | Some groupBy ->
                let groupBy = checker.GroupBy(groupBy)
                let byIdempotent = groupBy.By |> Array.forall (fun e -> e.Info.Idempotent)
                let havingIdempotent = groupBy.Having |> Option.forall (fun e -> e.Info.Idempotent)
                Some groupBy, byIdempotent && havingIdempotent
        checker,
            {   Columns = columns
                From = from
                Where = where
                GroupBy = groupBy
                Info =
                    {   Table = SelectResults
                        Query =
                            {   Columns = infoColumns
                                StaticRowCount = staticCount
                                ClausesIdempotent = whereIdempotent && groupByIdempotent
                            }
                    } |> TableLike
            } |> AggregateChecker.check

    member this.CTE(cte : CommonTableExpression) =
        let knownShape = cte.ColumnNames |> Option.map (fun n -> cxt.AnonymousQueryInfo(n.Value))
        let select = this.Select(cte.AsSelect, { KnownShape = knownShape; CTEName = Some cte.Name })
        {   Name = cte.Name
            ColumnNames = cte.ColumnNames
            AsSelect = select
            Info = select.Value.Info
        }

    member this.WithClause(withClause : WithClause) =
        let mutable scope = scope
        let clause =
            {   Recursive = withClause.Recursive
                Tables =
                    [|  for cte in withClause.Tables ->
                            let cte = TypeChecker(cxt, scope).CTE(cte)
                            scope <-
                                { scope with
                                    CTEVariables = scope.CTEVariables |> Map.add cte.Name cte.Info.Table.Query
                                }
                            cte
                    |]
            }
        TypeChecker(cxt, scope), clause

    member this.OrderingTerm(orderingTerm : OrderingTerm) =
        {   By = this.Expr(orderingTerm.By)
            Direction = orderingTerm.Direction
        }

    member this.Limit(limit : Limit) =
        {   Limit = this.Expr(limit.Limit, IntegerType Integer64)
            Offset = limit.Offset |> Option.map (fun e -> this.Expr(e, IntegerType Integer64))
        }

    member this.CompoundTerm(term : CompoundTerm, knownShape : InferredQueryShape option)
        : TypeChecker * InfCompoundTerm =
        let info, fromChecker, value =
            match term.Value, knownShape with
            | Values vals, Some shape ->
                let vals = vals |> rmap (fun w -> { WithSource.Value = rmap this.Expr w.Value; Source = w.Source })
                let columns =
                    seq {
                        for rowIndex, row in vals |> Seq.indexed do
                            if row.Value.Length <> shape.Columns.Count then
                                failAt row.Source <| Error.expectedKnownColumnCount row.Value.Length shape.Columns.Count  
                            for colVal, colShape in Seq.zip row.Value shape.Columns do
                                cxt.UnifyLeftKnown(row.Source, colShape.Expr.Info.Type, colVal.Info.Type)
                                if rowIndex > 0 then () else
                                yield
                                    {   Expr = colVal
                                        FromAlias = None
                                        ColumnName = colShape.ColumnName
                                    }
                    } |> toReadOnlyList
                let idempotent = vals |> Array.forall (fun r -> r.Value |> Array.forall (fun v -> v.Info.Idempotent))
                TableLike
                    {   Table = CompoundTermResults
                        Query = { Columns = columns; StaticRowCount = Some vals.Length; ClausesIdempotent = idempotent }
                    }, this, Values vals
            | Values _, None ->
                failAt term.Source Error.valuesRequiresKnownShape
            | Select select, knownShape ->
                let checker, select = this.SelectCore(select, knownShape)
                select.Info, checker, Select select
        fromChecker, // pass up the typechecker for the "from" clause so "order by" can use it
            {   Source = term.Source
                Value = value
                Info = info
            }

    member this.Compound(compound : CompoundExpr, knownShape : InferredQueryShape option)
        : TypeChecker * InfCompoundExpr =
        let nested f leftCompound rightTerm =
            match knownShape with
            | Some _ ->
                let fromChecker, left = this.Compound(leftCompound, knownShape)
                let _, right = this.CompoundTerm(rightTerm, knownShape)
                fromChecker, f(left, right)
            | None ->
                let fromChecker, left = this.Compound(leftCompound, None)
                let _, right = this.CompoundTerm(rightTerm, Some left.Value.LeftmostInfo.Query)
                fromChecker, f(left, right)
        let fromChecker, value =
            match compound.Value with
            | CompoundTerm term ->
                let checker, term = this.CompoundTerm(term, knownShape)
                checker, CompoundTerm term
            | Union (expr, term) -> nested Union expr term
            | UnionAll (expr, term) -> nested UnionAll expr term
            | Intersect (expr, term) -> nested Intersect expr term
            | Except (expr, term) -> nested Except expr term
        fromChecker,
            {   CompoundExpr.Source = compound.Source
                Value = value
            }

    member this.CompoundTop(compound : CompoundExpr, selfShape : SelfQueryShape)
        : TypeChecker * InfCompoundExpr =
        match selfShape.CTEName with
        | None -> this.Compound(compound, selfShape.KnownShape)
        | Some cteName -> // handle recursive references to own CTE in rightmost term
            let nested f leftCompound recursiveFinalTerm =
                let fromChecker, leftCompound = this.Compound(leftCompound, selfShape.KnownShape)
                let leftQuery = leftCompound.Value.LeftmostInfo.Query
                let rightChecker = 
                    { scope with
                        CTEVariables = scope.CTEVariables |> Map.add cteName leftQuery
                    } |> this.WithScope
                let _, right = rightChecker.CompoundTerm(recursiveFinalTerm, Some leftQuery)
                fromChecker, f(leftCompound, right)
            let fromChecker, value =
                match compound.Value with
                | CompoundTerm term ->
                    let checker, term = this.CompoundTerm(term, selfShape.KnownShape)
                    checker, CompoundTerm term
                | Union (expr, term) -> nested Union expr term
                | UnionAll (expr, term) -> nested UnionAll expr term
                | Intersect (expr, term) -> nested Intersect expr term
                | Except (expr, term) -> nested Except expr term
            fromChecker,
                {   CompoundExpr.Source = compound.Source
                    Value = value
                }

    member this.Select(select : SelectStmt, selfShape : SelfQueryShape) : InfSelectStmt =
        {   Source = select.Source
            Value =
                let select = select.Value
                let checker, withClause =
                    match select.With with
                    | None -> this, None
                    | Some withClause ->
                        let checker, withClause = this.WithClause(withClause)
                        checker, Some withClause
                let fromChecker, compound = checker.CompoundTop(select.Compound, selfShape)
                let limit = Option.map checker.Limit select.Limit
                let info =
                    let eitherNull (t1 : InferredType) (t2 : InferredType) =
                        { t1 with InferredNullable = InferredNullable.Either(t1.InferredNullable, t2.InferredNullable) }
                    let merge attemptAdd (leftInfo : InferredType ObjectInfo) (rightInfo : InferredType ObjectInfo) =
                        match attemptAdd, leftInfo, rightInfo with
                        | true,
                            TableLike({ Query = { StaticRowCount = Some left } as lq } as lt),
                            TableLike { Query = { StaticRowCount = Some right } as rq } ->
                                let q = lq.MergeInfo(rq, eitherNull)
                                { lt with
                                    Query = { q with StaticRowCount = Some (left + right) }
                                } |> TableLike
                        | _, TableLike ({ Query = q } as lt), right ->
                            let q = q.MergeInfo(right.Query, eitherNull)
                            TableLike { lt with Query = { q with StaticRowCount = None } }
                        | _ -> bug "Compound expr info must always be table-like!"
                    match limit, compound.Value.MergeInfo(merge true, merge false) with
                    | Some _, TableLike ({ Query = { StaticRowCount = Some _ } as query } as table) ->
                        // if we have any limit expr, drop the static row count
                        // technically we could figure it out if we're dealing w/ constants, but it's not worth it
                        TableLike { table with Query = { query with StaticRowCount = None } }
                    | _, other -> other
                let orderBy = Option.map (rmap fromChecker.OrderingTerm) select.OrderBy
                let info =
                    if not info.Query.ClausesIdempotent then info else
                    match info with
                    | TableLike t ->
                        let limitIdem =
                            limit
                            |> Option.forall (fun l ->
                                l.Limit.Info.Idempotent
                                && (l.Offset |> Option.forall (fun o -> o.Info.Idempotent)))
                        let orderByIdem =
                            orderBy
                            |> Option.forall (fun o ->
                                o |> Array.forall (fun e -> e.By.Info.Idempotent))
                        let idem = limitIdem && orderByIdem
                        TableLike { t with Query = { t.Query with ClausesIdempotent = idem } }
                    | other -> other
                {   With = withClause
                    Compound = compound
                    OrderBy = orderBy
                    Limit = limit
                    Info = info
                }
        }

    member this.ForeignKey(foreignKey, creating : CreateTableStmt option) =
        let referencesTable, columnNames =
            match creating with
            | Some tbl when tbl.Name = foreignKey.ReferencesTable -> // self-referencing
                this.ObjectName(foreignKey.ReferencesTable, allowNotFound = true),
                    match tbl.As with
                    | CreateAsDefinition cdef -> cdef.Columns |> Seq.map (fun c -> c.Value.Name)
                    | CreateAsSelect _ -> bug "Self-referencing constraints can't exist in a CREATE AS SELECT"
            | _ ->
                let name = this.ObjectName(foreignKey.ReferencesTable)
                name, name.Info.Query.Columns |> Seq.map (fun c -> c.ColumnName)
        for { Source = source; Value = referenceName } in foreignKey.ReferencesColumns do
            if not (Seq.contains referenceName columnNames) then
                failAt source <| Error.noSuchColumn referenceName
        {   ReferencesTable = referencesTable
            ReferencesColumns = foreignKey.ReferencesColumns
            OnDelete = foreignKey.OnDelete
        }

    member this.ColumnConstraint(constr : ColumnConstraint, creating : CreateTableStmt option) =
        {   Name = constr.Name
            ColumnConstraintType =
                match constr.ColumnConstraintType with
                | PrimaryKeyConstraint clause -> PrimaryKeyConstraint clause
                | UniqueConstraint -> UniqueConstraint
                | ForeignKeyConstraint foreignKey -> ForeignKeyConstraint <| this.ForeignKey(foreignKey, creating)
        }

    member this.ColumnDef(cdef : ColumnDef WithSource, creating : CreateTableStmt option) =
        {   Source = cdef.Source
            Value =
                let cdef = cdef.Value
                {   Name = cdef.Name
                    Type = cdef.Type
                    Nullable = cdef.Nullable
                    Collation = cdef.Collation
                    DefaultValue = Option.map this.Expr cdef.DefaultValue
                    Constraints = cdef.Constraints |> rmap (fun con -> this.ColumnConstraint(con, creating))
                }
        }

    member this.Alteration(tableName : InfObjectName, alteration : AlterTableAlteration) =
        let inline resolveColumn name =
            stateful {
                let! qualified = ComplexModelOps.qualify tableName
                // IMPROVEMENT source column name?
                let! column = ModelOps.getRequiredColumn qualified { Source = tableName.Source; Value = name }
                return column
            } |> State.runForOuputValue scope.Model
        match alteration with
        | RenameTo name -> RenameTo name
        | AddColumn cdef ->
            let hypothetical =
                stateful {
                    let! qualified = ComplexModelOps.qualify tableName
                    do! ComplexModelOps.addColumnDef qualified cdef
                    return! ModelOps.getRequiredTable qualified
                } |> State.runForOuputValue scope.Model
            let from =
                InferredFromClause.FromSingleObject
                    ({ tableName with
                        Info =
                            {   Table = TableReference hypothetical
                                Query = inferredOfTable(hypothetical)
                            } |> TableLike })
            let this = this.WithScope({ scope with FromClause = Some from })
            AddColumn <| this.ColumnDef(cdef, None)
        | AddConstraint constr ->
            let this =
                match constr.Value.TableConstraintType with
                | TableCheckConstraint _ ->
                    // TODO clean up this code -- but need FROM scope for check expr typechecking!
                    let hypothetical =
                        stateful {
                            let! qualified = ComplexModelOps.qualify tableName
                            return! ModelOps.getRequiredTable qualified
                        } |> State.runForOuputValue scope.Model
                    let from =
                        InferredFromClause.FromSingleObject
                            ({ tableName with
                                Info =
                                    {   Table = TableReference hypothetical
                                        Query = inferredOfTable(hypothetical)
                                    } |> TableLike })
                    this.WithScope({ scope with FromClause = Some from })
                | _ -> this
            AddConstraint <| this.TableConstraint(constr, None)
        | AddDefault (name, expr) -> AddDefault (name, this.Expr(expr))
        | DropColumn name -> DropColumn name
        | DropConstraint name -> DropConstraint name
        | DropDefault name -> DropDefault name
        | ChangeType change ->
            let schemaColumn = resolveColumn change.Column
            {   ExistingInfo = exprInfoOfColumn schemaColumn
                Column = change.Column
                NewType = change.NewType
            } |> ChangeType
        | ChangeNullability change ->
            let schemaColumn = resolveColumn change.Column
            {   ExistingInfo = exprInfoOfColumn schemaColumn
                Column = change.Column
                NewNullable = change.NewNullable
            } |> ChangeNullability
        | ChangeCollation change ->
            let schemaColumn = resolveColumn change.Column
            {   ExistingInfo = exprInfoOfColumn schemaColumn
                Column = change.Column
                NewCollation = change.NewCollation
            } |> ChangeCollation

    member this.CreateIndex(createIndex : CreateIndexStmt) =
        let tableName = this.SchemaTableName(createIndex.TableName)
        let checker =
            this.WithScope({ scope with FromClause = Some <| InferredFromClause.FromSingleObject(tableName) })
        let query = tableName.Info.Query
        for { Source = source; Value = (col, _) } in createIndex.IndexedColumns do
            match query.ColumnByName(col) with
            | Found _ -> ()
            | NotFound _ -> failAt source <| Error.noSuchColumn col
            | Ambiguous _ -> failAt source <| Error.ambiguousColumn col
        {   Unique = createIndex.Unique
            IndexName = this.ObjectName(createIndex.IndexName, allowNotFound = true)
            TableName = tableName
            IndexedColumns = createIndex.IndexedColumns
            Where = createIndex.Where |> Option.map checker.BooleanExpr
        }

    member this.TableIndexConstraint(constr : TableIndexConstraintClause, creating : CreateTableStmt option) =
        match creating with
        | Some { As = CreateAsDefinition def } ->
            let columnNames = def.Columns |> Seq.map (fun c -> c.Value.Name) |> Set.ofSeq
            for { Source = source; Value = (name, _) } in constr.IndexedColumns do
                if columnNames |> (not << Set.contains name) then
                    failAt source <| Error.noSuchColumn name
        | _ -> ()
        {   Type = constr.Type
            IndexedColumns = constr.IndexedColumns
        }

    member this.TableConstraint(constr : TableConstraint WithSource, creating : CreateTableStmt option) =
        {   Source = constr.Source
            Value =
                let constr = constr.Value
                {   Name = constr.Name
                    TableConstraintType =
                        match constr.TableConstraintType with
                        | TableIndexConstraint clause ->
                            TableIndexConstraint <| this.TableIndexConstraint(clause, creating)
                        | TableForeignKeyConstraint (names, foreignKey) ->
                            TableForeignKeyConstraint (names, this.ForeignKey(foreignKey, creating))
                        | TableCheckConstraint expr -> TableCheckConstraint <| this.Expr(expr)
                }
        }

    member this.CreateTableDefinition
        (tableName : InfObjectName, createTable : CreateTableDefinition, creating : CreateTableStmt) =
        let hypothetical =
            stateful {
                let! qualified = ComplexModelOps.qualifyTemp creating.Temporary tableName
                do! ComplexModelOps.createTableByDefinition qualified createTable
                return! ModelOps.getRequiredTable qualified
            } |> State.runForOuputValue scope.Model
        let from =
            InferredFromClause.FromSingleObject
                ({ tableName with
                    Info =
                        {   Table = TableReference hypothetical
                            Query = inferredOfTable hypothetical
                        } |> TableLike })
        let this = this.WithScope({ scope with FromClause = Some from })
        let creating = Some creating
        let columns = createTable.Columns |> rmap (fun col -> this.ColumnDef(col, creating))
        {   Columns = columns
            Constraints = createTable.Constraints |> rmap (fun con -> this.TableConstraint(con, creating))
        }

    member this.CreateTable(createTable : CreateTableStmt) =
        let name = this.ObjectName(createTable.Name, true)
        let name =
            match createTable.Temporary, name.SchemaName with
            | true, None ->
                { name with SchemaName = Some scope.Model.TemporarySchema }
            | _ -> name
        {   Temporary = createTable.Temporary
            Name = name
            As =
                match createTable.As with
                | CreateAsSelect select -> CreateAsSelect <| this.Select(select, SelfQueryShape.Unknown)
                | CreateAsDefinition def -> CreateAsDefinition <| this.CreateTableDefinition(name, def, createTable)
        }

    member this.CreateView(createView : CreateViewStmt) =
        let knownShape = createView.ColumnNames |> Option.map cxt.AnonymousQueryInfo
        {   Temporary = createView.Temporary
            ViewName = this.ObjectName(createView.ViewName, true)
            ColumnNames = createView.ColumnNames
            AsSelect = this.Select(createView.AsSelect, SelfQueryShape.Known(knownShape))
        }

    member this.Delete(delete : DeleteStmt) =
        let checker, withClause =
            match delete.With with
            | None -> this, None
            | Some withClause ->
                let checker, withClause = this.WithClause(withClause)
                checker, Some withClause
        let deleteFrom = checker.SchemaTableName(delete.DeleteFrom)
        let checker =
            checker.WithScope
                ({ checker.Scope with FromClause = InferredFromClause.FromSingleObject(deleteFrom) |> Some })
        {   With = withClause
            DeleteFrom = deleteFrom
            Where = Option.map checker.BooleanExpr delete.Where
            OrderBy = Option.map (rmap checker.OrderingTerm) delete.OrderBy
            Limit = Option.map checker.Limit delete.Limit
        }

    member this.DropObject(drop : DropObjectStmt) =
        {   Drop = drop.Drop
            ObjectName = this.ObjectName(drop.ObjectName)
        }

    member this.Insert(insert : InsertStmt) =
        let checker, withClause =
            match insert.With with
            | None -> this, None
            | Some withClause ->
                let checker, withClause = this.WithClause(withClause)
                checker, Some withClause
        let table = checker.ObjectName(insert.InsertInto)
        let knownShape = table.Info.Query.ColumnsWithNames(insert.Columns)
        let columns =
            knownShape.Columns
            |> Seq.map (fun c -> { WithSource.Source = c.Expr.Source; Value = c.ColumnName })
            |> Seq.toArray
        match table.Info with
        | TableLike { Table = TableReference tbl } ->
            let optionalColumns =
                let colsWithDefaults =
                    tbl.Columns
                    |> Seq.filter (fun c -> c.Value.ColumnType.Nullable || Option.isSome c.Value.DefaultValue)
                    |> Seq.map (fun c -> c.Value.ColumnName)
                    |> Set.ofSeq
                tbl.Constraints
                |> Seq.filter (fun c ->
                    match c.Value.ConstraintType with
                    | PrimaryKeyConstraintType true -> true
                    | _ -> false)
                |> Seq.map (fun c -> c.Value.Columns)
                |> Seq.fold Set.union colsWithDefaults
            let suppliedColumns =
                columns
                |> Seq.map (fun c -> c.Value)
                |> Set.ofSeq
            let missingColumns =
                tbl.Columns
                |> Seq.map (fun c -> c.Key)
                |> Seq.filter (fun c -> not (Set.contains c optionalColumns) && not (Set.contains c suppliedColumns))
                |> Seq.toArray
            if missingColumns.Length > 0 then
                failAt insert.Columns.[0].Source (Error.insertMissingColumns missingColumns)
        | _ ->
            failAt insert.InsertInto.Source Error.insertIntoNonTable
        match columns |> tryFindFirstDuplicateBy (fun c -> c.Value) with
        | None ->
            {   With = withClause
                Or = insert.Or
                InsertInto = table
                Columns = columns // we *must* specify these because our order might not match DB's
                Data = checker.Select(insert.Data, SelfQueryShape.Known(knownShape))
            }
        | Some duplicate ->
            failAt duplicate.Source (Error.insertDuplicateColumn duplicate.Value)

    member this.Update(update : UpdateStmt) =
        let checker, withClause =
            match update.With with
            | None -> this, None
            | Some withClause ->
                let checker, withClause = this.WithClause(withClause)
                checker, Some withClause
        let updateTable = checker.SchemaTableName(update.UpdateTable)
        let checker =
            checker.WithScope
                ({ checker.Scope with FromClause = InferredFromClause.FromSingleObject(updateTable) |> Some })
        let setColumns =
            [|  let cols = updateTable.Info.Query
                for name, expr in update.Set do
                    match cols.ColumnByName(name.Value) with
                    | Found col ->
                        let expr = checker.Expr(expr)
                        cxt.UnifyLeftKnown(name.Source, col.Expr.Info.Type, expr.Info.Type)
                        yield name, expr
                    | _ ->
                        failAt name.Source <| Error.noSuchColumnToSet updateTable name.Value
            |]
        match setColumns |> tryFindFirstDuplicateBy (fun (name, _) -> name.Value) with
        | None ->
            {   With = withClause
                UpdateTable = updateTable
                Or = update.Or
                Set = setColumns
                Where = Option.map checker.Expr update.Where
                OrderBy = Option.map (rmap checker.OrderingTerm) update.OrderBy
                Limit = Option.map checker.Limit update.Limit
            }
        | Some (name, _) ->
            failAt name.Source (Error.updateDuplicateColumn name.Value)

    member this.Stmt(stmt : Stmt) =
        match stmt with
        | AlterTableStmt alter ->
            AlterTableStmt <|
                let tbl = this.SchemaTableName(alter.Table)
                {   Table = tbl
                    Alteration = this.Alteration(tbl, alter.Alteration)
                }
        | CreateIndexStmt index -> CreateIndexStmt <| this.CreateIndex(index)
        | CreateTableStmt createTable -> CreateTableStmt <| this.CreateTable(createTable)
        | CreateViewStmt createView -> CreateViewStmt <| this.CreateView(createView)
        | DeleteStmt delete -> DeleteStmt <| this.Delete(delete)
        | DropObjectStmt drop -> DropObjectStmt <| this.DropObject(drop)
        | InsertStmt insert -> InsertStmt <| this.Insert(insert)
        | SelectStmt select -> SelectStmt <| this.Select(select, SelfQueryShape.Unknown)
        | UpdateStmt update -> UpdateStmt <| this.Update(update)

    interface IQueryTypeChecker with
        member this.Select(select) =
            TypeChecker(cxt, scope.Child()).Select(select, SelfQueryShape.Unknown)
        member this.CreateView(view) = this.CreateView(view)