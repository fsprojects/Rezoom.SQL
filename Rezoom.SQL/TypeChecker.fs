namespace Rezoom.SQL
open System
open System.Collections.Generic
open Rezoom.SQL.InferredTypes

type InferredQueryShape = InferredType QueryExprInfo
type SelfQueryShape =
    {   CTEName : Name option
        KnownShape : InferredQueryShape option
    }
    static member Known(known) = { CTEName = None; KnownShape = known }
    static member Known(known) = SelfQueryShape.Known(Some known)
    static member Unknown = { CTEName = None; KnownShape = None }

type TypeChecker(cxt : ITypeInferenceContext, scope : InferredSelectScope) as this =
    let exprChecker = ExprTypeChecker(cxt, scope, this)
    member this.ObjectName(name) = exprChecker.ObjectName(name)
    member this.ObjectName(name, allowNotFound) = exprChecker.ObjectName(name, allowNotFound)
    member this.Expr(expr, knownType) = exprChecker.Expr(expr, knownType)
    member this.Expr(expr) = exprChecker.Expr(expr)
    member this.Scope = scope
    member this.WithScope(scope) = TypeChecker(cxt, scope)

    member private this.TableOrSubqueryScope(tsub : TableOrSubquery) =
        match tsub.Table with
        | Table (tinvoc, index) ->
            tsub.Alias |? tinvoc.Table.ObjectName, this.ObjectName(tinvoc.Table).Info
        | Subquery select ->
            match tsub.Alias with
            | None -> failAt select.Source "Subquery requires an alias"
            | Some alias -> alias, this.Select(select, SelfQueryShape.Unknown).Value.Info

    member private this.TableExprScope(dict : Dictionary<Name, InferredType ObjectInfo>, texpr : TableExpr, outerJoin) =
        let add name objectInfo =
            if dict.ContainsKey(name) then
                failAt texpr.Source <| sprintf "Table name already in scope: ``%O``" name
            else
                dict.Add(name, objectInfo)
        match texpr.Value with
        | TableOrSubquery tsub ->
            let alias, objectInfo = this.TableOrSubqueryScope(tsub)
            let objectInfo =
                if outerJoin then
                    objectInfo.Map(fun t -> { t with InferredNullable = NullableDueToJoin t.InferredNullable })
                else objectInfo
            add alias objectInfo
        | Join join ->
            this.TableExprScope(dict, join.LeftTable, outerJoin = false)
            this.TableExprScope(dict, join.RightTable, outerJoin = join.JoinType.IsOuter)

    member private this.TableExprScope(texpr : TableExpr) =
        let dict = Dictionary()
        this.TableExprScope(dict, texpr, outerJoin = false)
        { FromVariables = dict }

    member private this.TableOrSubquery(tsub : TableOrSubquery) =
        let tbl, info =
            match tsub.Table with
            | Table (tinvoc, index) ->
                let invoke = exprChecker.TableInvocation(tinvoc)
                Table (invoke, index), invoke.Table.Info
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
            Column
                ({  Source = resultColumn.Source
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
                    | Some prefix -> Some (prefix + col.ColumnName))
        match resultColumn.Case with
        | ColumnsWildcard ->
            match scope.FromClause with
            | None -> failAt resultColumn.Source "Must have a FROM clause to use * wildcard"
            | Some from ->
                seq {
                    for KeyValue(tableAlias, fromTable) in from.FromVariables do
                    for col in fromTable.Table.Query.Columns do
                        yield qualify tableAlias fromTable col
                }
        | TableColumnsWildcard tbl ->
            match scope.FromClause with
            | None -> failAt resultColumn.Source <| sprintf "Must have a FROM clause to use ``%O``" tbl
            | Some from ->
                let succ, fromTable = from.FromVariables.TryGetValue(tbl)
                if not succ then failAt resultColumn.Source <| sprintf "No such table: ``%O``" tbl
                fromTable.Table.Query.Columns |> Seq.map (qualify tbl fromTable)
        | Column (expr, alias) ->
            match aliasPrefix with
            | None -> Column (this.Expr(expr), alias) |> Seq.singleton
            | Some prefix -> 
                let expr = this.Expr(expr)
                match implicitAlias (expr.Value, alias) with
                | None -> Column (expr, None) |> Seq.singleton
                | Some a -> Column (expr, Some (prefix + a)) |> Seq.singleton
        | ColumnNav nav ->
            let subAliasPrefix =
                let prev =
                    match aliasPrefix with
                    | Some prefix -> prefix.Value
                    | None -> ""
                Some <| Name(prev + nav.Name.Value + nav.Cardinality.Separator)
            nav.Columns |> Seq.collect (fun c -> this.ResultColumn(subAliasPrefix, c))
                                     
    member this.ResultColumns(resultColumns : ResultColumns, knownShape : InferredQueryShape option) =
        let columns =
            resultColumns.Columns
            |> Seq.collect
                (fun rc ->
                    this.ResultColumn(None, rc)
                    |> Seq.map (fun c -> { Source = rc.Source; Case = c; }))
            |> Seq.toArray
        match knownShape with
        | Some shape ->
            if columns.Length <> shape.Columns.Count then
                if columns.Length <= 0 then failwith "BUG: impossible, parser shouldn't have accepted this"
                let source = columns.[columns.Length - 1].Source
                failAt source <| sprintf "Expected %d columns but selected %d" shape.Columns.Count columns.Length
            for i = 0 to columns.Length - 1 do
                let selected, alias as selectedCol = columns.[i].Case.AssumeColumn()
                let shape = shape.Columns.[i]
                ignore <| cxt.Unify(selected.Source, selected.Info.Type, shape.Expr.Info.Type)
                match implicitAlias (selected.Value, alias) with
                | Some a when a = shape.ColumnName -> ()
                | _ ->
                    columns.[i] <- { columns.[i] with Case = Column(selected, Some shape.ColumnName) }
        | None -> ()
        {   Distinct = resultColumns.Distinct
            Columns = columns
        }

    member this.GroupBy(groupBy : GroupBy) =
        {   By = groupBy.By |> rmap this.Expr
            Having = groupBy.Having |> Option.map this.Expr
        }

    member this.SelectCore(select : SelectCore, knownShape : InferredQueryShape option) =
        let checker, from =
            match select.From with
            | None -> this, None
            | Some from ->
                let checker, texpr = this.TableExpr(from)
                checker, Some texpr
        let columns = checker.ResultColumns(select.Columns, knownShape)
        let infoColumns =
            seq {
                for column in columns.Columns do
                    match column.Case with
                    | Column (expr, alias) ->
                        yield
                            {   Expr = expr
                                FromAlias = None
                                ColumnName =
                                    match implicitAlias (expr.Value, alias) with
                                    | None -> failAt column.Source "Expression-valued column requires an alias"
                                    | Some alias -> alias
                            }
                     // typechecker should've eliminated alternatives
                     | _ -> failwith "All wildcards must be expanded -- this is a typechecker bug"
            } |> toReadOnlyList
        {   Columns = columns
            From = from
            Where = Option.map checker.Expr select.Where
            GroupBy = Option.map checker.GroupBy select.GroupBy
            Info =
                {   Table = SelectResults
                    Query = { Columns = infoColumns }
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

    member this.CompoundTerm(term : CompoundTerm, knownShape : InferredQueryShape option) : InfCompoundTerm =
        let info, value =
            match term.Value, knownShape with
            | Values vals, Some shape ->
                let vals = vals |> rmap (fun w -> { WithSource.Value = rmap this.Expr w.Value; Source = w.Source })
                let columns =
                    seq {
                        for rowIndex, row in vals |> Seq.indexed do
                            if row.Value.Length <> shape.Columns.Count then
                                failAt row.Source <|
                                    sprintf "Incorrect number of columns (expected %d, got %d)"
                                        shape.Columns.Count row.Value.Length
                            for colVal, colShape in Seq.zip row.Value shape.Columns do
                                ignore <| cxt.Unify(row.Source, colVal.Info.Type, colShape.Expr.Info.Type)
                                if rowIndex > 0 then () else
                                yield
                                    {   Expr = colVal
                                        FromAlias = None
                                        ColumnName = colShape.ColumnName
                                    }
                    } |> toReadOnlyList
                TableLike
                    {   Table = CompoundTermResults
                        Query = { Columns = columns }
                    }, Values vals
            | Values vals, None ->
                failAt term.Source "VALUES() clause can only be used when column names are implied by context"
            | Select select, knownShape ->
                let select = this.SelectCore(select, knownShape)
                select.Info, Select select
        {   Source = term.Source
            Value = value
            Info = info
        }

    member this.Compound(compound : CompoundExpr, knownShape : InferredQueryShape option) : InfCompoundExpr =
        let nested leftCompound rightTerm =
            match knownShape with
            | Some _ as shape ->
                this.Compound(leftCompound, knownShape), this.CompoundTerm(rightTerm, knownShape)
            | None ->
                let leftCompound = this.Compound(leftCompound, None)
                leftCompound, this.CompoundTerm(rightTerm, Some leftCompound.Value.Info.Query)
        {   CompoundExpr.Source = compound.Source
            Value =
                match compound.Value with
                | CompoundTerm term -> CompoundTerm <| this.CompoundTerm(term, knownShape)
                | Union (expr, term) -> Union <| nested expr term
                | UnionAll (expr, term) -> UnionAll <| nested expr term
                | Intersect (expr, term) -> Intersect <| nested expr term
                | Except (expr, term) -> Except <| nested expr term
        }

    member this.CompoundTop(compound : CompoundExpr, selfShape : SelfQueryShape) : InfCompoundExpr =
        match selfShape.CTEName with
        | None -> this.Compound(compound, selfShape.KnownShape)
        | Some cteName ->
            let nested leftCompound recursiveFinalTerm =
                let leftCompound = this.Compound(leftCompound, selfShape.KnownShape)
                let leftQuery = leftCompound.Value.Info.Query
                let rightChecker = 
                    { scope with
                        CTEVariables = scope.CTEVariables |> Map.add cteName leftQuery
                    } |> this.WithScope
                leftCompound, rightChecker.CompoundTerm(recursiveFinalTerm, Some leftQuery)
            {   CompoundExpr.Source = compound.Source
                Value =
                    match compound.Value with
                    | CompoundTerm term -> CompoundTerm <| this.CompoundTerm(term, selfShape.KnownShape)
                    | Union (expr, term) -> Union <| nested expr term
                    | UnionAll (expr, term) -> UnionAll <| nested expr term
                    | Intersect (expr, term) -> Intersect <| nested expr term
                    | Except (expr, term) -> Except <| nested expr term
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
                let compound = checker.CompoundTop(select.Compound, selfShape)
                {   With = withClause
                    Compound = compound
                    OrderBy = Option.map (rmap checker.OrderingTerm) select.OrderBy
                    Limit = Option.map checker.Limit select.Limit
                    Info = compound.Value.Info
                }
        }

    member this.ForeignKey(foreignKey) =
        {   ReferencesTable = this.ObjectName(foreignKey.ReferencesTable)
            ReferencesColumns = foreignKey.ReferencesColumns
            Rules = foreignKey.Rules
            Defer = foreignKey.Defer
        }

    member this.ColumnConstraint(constr : ColumnConstraint) =
        {   Name = constr.Name
            ColumnConstraintType =
                match constr.ColumnConstraintType with
                | NullableConstraint -> NullableConstraint
                | PrimaryKeyConstraint clause -> PrimaryKeyConstraint clause
                | NotNullConstraint -> NotNullConstraint
                | UniqueConstraint -> UniqueConstraint
                | DefaultConstraint def -> DefaultConstraint <| this.Expr(def)
                | CollateConstraint name -> CollateConstraint name
                | ForeignKeyConstraint foreignKey -> ForeignKeyConstraint <| this.ForeignKey(foreignKey)
        }

    member this.ColumnDef(cdef : ColumnDef) =
        {   Name = cdef.Name
            Type = cdef.Type
            Constraints = rmap this.ColumnConstraint cdef.Constraints
        }

    member this.Alteration(tableName : InfObjectName, alteration : AlterTableAlteration) =
        match alteration with
        | RenameTo name -> RenameTo name
        | AddColumn cdef ->
            let fake =
                resultAt tableName.Source <|
                match tableName.Info.Table.Table with
                | TableReference schemaTable -> schemaTable.WithAdditionalColumn(cdef)
                | _ -> Error <| sprintf "Not a table: %O" tableName
            let from =
                InferredFromClause.FromSingleObject
                    ({ tableName with
                        Info =
                            {   Table = TableReference fake
                                Query = inferredOfTable(fake)
                            } |> TableLike })
            let this = this.WithScope({ scope with FromClause = Some from })
            AddColumn <| this.ColumnDef(cdef)

    member this.CreateIndex(createIndex : CreateIndexStmt) =
        let tableName = this.ObjectName(createIndex.TableName)
        let checker =
            this.WithScope({ scope with FromClause = Some <| InferredFromClause.FromSingleObject(tableName) })
        {   Unique = createIndex.Unique
            IndexName = this.ObjectName(createIndex.IndexName)
            TableName = tableName
            IndexedColumns = createIndex.IndexedColumns
            Where = createIndex.Where |> Option.map checker.Expr
        }

    member this.TableIndexConstraint(constr : TableIndexConstraintClause) =
        {   Type = constr.Type
            IndexedColumns = constr.IndexedColumns
        }

    member this.TableConstraint(constr : TableConstraint) =
        {   Name = constr.Name
            TableConstraintType =
                match constr.TableConstraintType with
                | TableIndexConstraint clause ->
                    TableIndexConstraint <| this.TableIndexConstraint(clause)
                | TableForeignKeyConstraint (names, foreignKey) ->
                    TableForeignKeyConstraint (names, this.ForeignKey(foreignKey))
                | TableCheckConstraint expr -> TableCheckConstraint <| this.Expr(expr)
        }

    member this.CreateTableDefinition(tableName : InfObjectName, createTable : CreateTableDefinition) =
        let fake =
            SchemaTable.OfCreateDefinition
                ( tableName.SchemaName |? scope.Model.DefaultSchema
                , tableName.ObjectName
                , createTable
                )
        let from =
            InferredFromClause.FromSingleObject
                ({ tableName with
                    Info =
                        {   Table = TableReference fake
                            Query = inferredOfTable(fake)
                        } |> TableLike })
        let this = this.WithScope({ scope with FromClause = Some from })
        let columns = createTable.Columns |> rmap this.ColumnDef
        {   Columns = columns
            Constraints = createTable.Constraints |> rmap this.TableConstraint
            WithoutRowId = createTable.WithoutRowId
        }

    member this.CreateTable(createTable : CreateTableStmt) =
        let name = this.ObjectName(createTable.Name, true)
        {   Temporary = createTable.Temporary
            Name = name
            As =
                match createTable.As with
                | CreateAsSelect select -> CreateAsSelect <| this.Select(select, SelfQueryShape.Unknown)
                | CreateAsDefinition def -> CreateAsDefinition <| this.CreateTableDefinition(name, def)
        }

    member this.CreateView(createView : CreateViewStmt) =
        let knownShape = createView.ColumnNames |> Option.map (cxt.AnonymousQueryInfo)
        {   Temporary = createView.Temporary
            ViewName = this.ObjectName(createView.ViewName, true)
            ColumnNames = createView.ColumnNames
            AsSelect = this.Select(createView.AsSelect, SelfQueryShape.Known(knownShape))
        }

    member this.QualifiedTableName(qualified : QualifiedTableName) =
        {   TableName = this.ObjectName(qualified.TableName)
            IndexHint = qualified.IndexHint
        }

    member this.Delete(delete : DeleteStmt) =
        let checker, withClause =
            match delete.With with
            | None -> this, None
            | Some withClause ->
                let checker, withClause = this.WithClause(withClause)
                checker, Some withClause
        let deleteFrom = checker.QualifiedTableName(delete.DeleteFrom)
        let checker =
            checker.WithScope
                ({ checker.Scope with FromClause = InferredFromClause.FromSingleObject(deleteFrom.TableName) |> Some })
        {   With = withClause
            DeleteFrom = deleteFrom
            Where = Option.map checker.Expr delete.Where
            OrderBy = Option.map (rmap checker.OrderingTerm) delete.OrderBy
            Limit = Option.map checker.Limit delete.Limit
        }

    member this.DropObject(drop : DropObjectStmt) =
        {   Drop = drop.Drop
            ObjectName = this.ObjectName(drop.ObjectName)
        }

    member this.Insert(insert : InsertStmt) = // TODO: verify that we insert into all cols w/o default values
        let checker, withClause =
            match insert.With with
            | None -> this, None
            | Some withClause ->
                let checker, withClause = this.WithClause(withClause)
                checker, Some withClause
        let table = checker.ObjectName(insert.InsertInto)
        let knownShape =
            match insert.Columns with
            | None -> table.Info.Query
            | Some cols -> table.Info.Query.ColumnsWithNames(cols)
        let columns =
            knownShape.Columns
            |> Seq.map (fun c -> { WithSource.Source = c.Expr.Source; Value = c.ColumnName })
            |> Seq.toArray
        {   With = withClause
            Or = insert.Or
            InsertInto = table
            Columns = Some columns // we *must* specify these because our order might not match DB's
            Data = insert.Data |> Option.map (fun data -> checker.Select(data, SelfQueryShape.Known(knownShape)))
        }

    member this.Update(update : UpdateStmt) =
        let checker, withClause =
            match update.With with
            | None -> this, None
            | Some withClause ->
                let checker, withClause = this.WithClause(withClause)
                checker, Some withClause
        let updateTable = checker.QualifiedTableName(update.UpdateTable)
        let checker =
            checker.WithScope
                ({ checker.Scope with FromClause = InferredFromClause.FromSingleObject(updateTable.TableName) |> Some })
        let setColumns =
            [|  let cols = updateTable.TableName.Info.Query
                for name, expr in update.Set do
                    match cols.ColumnByName(name.Value) with
                    | Found col ->
                        let expr = checker.Expr(expr)
                        ignore <| cxt.Unify(name.Source, col.Expr.Info.Type, expr.Info.Type)
                        yield name, expr
                    | _ ->
                        failAt name.Source <| sprintf "No such column to set: ``%O``" name.Value
            |]
        {   With = withClause
            UpdateTable = updateTable
            Or = update.Or
            Set = setColumns
            Where = Option.map checker.Expr update.Where
            OrderBy = Option.map (rmap checker.OrderingTerm) update.OrderBy
            Limit = Option.map checker.Limit update.Limit
        }

    member this.Stmt(stmt : Stmt) =
        match stmt with
        | AlterTableStmt alter ->
            AlterTableStmt <|
                let tbl = this.ObjectName(alter.Table)
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
        | BeginStmt -> BeginStmt
        | CommitStmt -> CommitStmt
        | RollbackStmt -> RollbackStmt

    interface IQueryTypeChecker with
        member this.Select(select) = this.Select(select, SelfQueryShape.Unknown)