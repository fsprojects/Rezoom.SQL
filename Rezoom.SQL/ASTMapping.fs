namespace Rezoom.SQL
open System
open System.Collections.Generic

type ASTMapping<'t1, 'e1, 't2, 'e2>(mapT : 't1 -> 't2, mapE : 'e1 -> 'e2) =
    member this.Binary(binary : BinaryExpr<'t1, 'e1>) =
        {   Operator = binary.Operator
            Left = this.Expr(binary.Left)
            Right = this.Expr(binary.Right)
        }
    member this.Unary(unary : UnaryExpr<'t1, 'e1>) =
        {   Operator = unary.Operator
            Operand = this.Expr(unary.Operand)
        }
    member this.ObjectName(objectName : ObjectName<'t1>) =
        {   SchemaName = objectName.SchemaName
            ObjectName = objectName.ObjectName
            Source = objectName.Source
            Info = mapT objectName.Info
        }
    member this.ColumnName(columnName : ColumnName<'t1>) =
        {   Table = Option.map this.ObjectName columnName.Table
            ColumnName = columnName.ColumnName
        }
    member this.Cast(cast : CastExpr<'t1, 'e1>) =
        {   Expression = this.Expr(cast.Expression)
            AsType = cast.AsType
        }
    member this.Collation(collation : CollationExpr<'t1, 'e1>) =
        {   Input = this.Expr(collation.Input)
            Collation = collation.Collation
        }
    member this.FunctionInvocation(func : FunctionInvocationExpr<'t1, 'e1>) =
        {   FunctionName = func.FunctionName
            Arguments =
                match func.Arguments with
                | ArgumentWildcard -> ArgumentWildcard
                | ArgumentList (distinct, exprs) ->
                    ArgumentList (distinct, exprs |> rmap this.Expr)
        }
    member this.Similarity(sim : SimilarityExpr<'t1, 'e1>) =
        {   Invert = sim.Invert
            Operator = sim.Operator
            Input = this.Expr(sim.Input)
            Pattern = this.Expr(sim.Pattern)
            Escape = Option.map this.Expr sim.Escape
        }
    member this.Between(between : BetweenExpr<'t1, 'e1>) =
        {   Invert = between.Invert
            Input = this.Expr(between.Input)
            Low = this.Expr(between.Low)
            High = this.Expr(between.High)
        }
    member this.In(inex : InExpr<'t1, 'e1>) =
        {   Invert = inex.Invert
            Input = this.Expr(inex.Input)
            Set =
                {   Source = inex.Set.Source
                    Value =
                        match inex.Set.Value with
                        | InExpressions exprs -> exprs |> rmap this.Expr |> InExpressions
                        | InSelect select -> InSelect <| this.Select(select)
                        | InTable table -> InTable <| this.TableInvocation(table)
                }
        }
    member this.Case(case : CaseExpr<'t1, 'e1>) =
        {   Input = Option.map this.Expr case.Input
            Cases =
                seq {
                    for whenExpr, thenExpr in case.Cases ->
                        this.Expr(whenExpr), this.Expr(thenExpr)
                } |> ResizeArray
            Else =
                {   Source = case.Else.Source
                    Value = Option.map this.Expr case.Else.Value
                }
        }
    member this.ExprType(expr : ExprType<'t1, 'e1>) : ExprType<'t2, 'e2> =
        match expr with
        | LiteralExpr lit -> LiteralExpr lit
        | BindParameterExpr par -> BindParameterExpr par
        | ColumnNameExpr name -> ColumnNameExpr <| this.ColumnName(name)
        | CastExpr cast -> CastExpr <| this.Cast(cast)
        | CollateExpr collation -> CollateExpr <| this.Collation(collation)
        | FunctionInvocationExpr func -> FunctionInvocationExpr <| this.FunctionInvocation(func)
        | SimilarityExpr sim -> SimilarityExpr <| this.Similarity(sim)
        | BinaryExpr bin -> BinaryExpr <| this.Binary(bin)
        | UnaryExpr un -> UnaryExpr <| this.Unary(un)
        | BetweenExpr between -> BetweenExpr <| this.Between(between)
        | InExpr inex -> InExpr <| this.In(inex)
        | ExistsExpr select -> ExistsExpr <| this.Select(select)
        | CaseExpr case -> CaseExpr <| this.Case(case)
        | ScalarSubqueryExpr select -> ScalarSubqueryExpr <| this.Select(select)
        | RaiseExpr raise -> RaiseExpr raise
    member this.Expr(expr : Expr<'t1, 'e1>) =
        {   Value = this.ExprType(expr.Value)
            Source = expr.Source
            Info = mapE expr.Info
        }
    member this.TableInvocation(table : TableInvocation<'t1, 'e1>) =
        {   Table = this.ObjectName(table.Table)
            Arguments = table.Arguments |> Option.map (rmap this.Expr)
        }
    member this.CTE(cte : CommonTableExpression<'t1, 'e1>) =
        {   Name = cte.Name
            ColumnNames = cte.ColumnNames
            AsSelect = this.Select(cte.AsSelect)
            Info = mapT cte.Info
        }
    member this.WithClause(withClause : WithClause<'t1, 'e1>) =
        {   Recursive = withClause.Recursive
            Tables = rmap this.CTE withClause.Tables
        }
    member this.OrderingTerm(orderingTerm : OrderingTerm<'t1, 'e1>) =
        {   By = this.Expr(orderingTerm.By)
            Direction = orderingTerm.Direction
        }
    member this.Limit(limit : Limit<'t1, 'e1>) =
        {   Limit = this.Expr(limit.Limit)
            Offset = Option.map this.Expr limit.Offset
        }
    member this.ResultColumn(resultColumn : ResultColumn<'t1, 'e1>) =
        let case =
            match resultColumn.Case with
            | ColumnsWildcard -> ColumnsWildcard
            | TableColumnsWildcard tbl -> TableColumnsWildcard tbl
            | Column (expr, alias) -> Column (this.Expr(expr), alias)
        { Case = case; Source = resultColumn.Source; AliasPrefix = resultColumn.AliasPrefix }
    member this.ResultColumns(resultColumns : ResultColumns<'t1, 'e1>) =
        {   Distinct = resultColumns.Distinct
            Columns = resultColumns.Columns |> rmap this.ResultColumn
        }
    member this.TableOrSubquery(table : TableOrSubquery<'t1, 'e1>) =
        let tbl =
            match table.Table with
            | Table (tinvoc, index) ->
                Table (this.TableInvocation(tinvoc), index)
            | Subquery select ->
                Subquery (this.Select(select))
        {   Table = tbl
            Alias = table.Alias
            Info = mapT table.Info
        }
    member this.JoinConstraint(constr : JoinConstraint<'t1, 'e1>) =  
        match constr with
        | JoinOn expr -> JoinOn <| this.Expr(expr)
        | JoinUsing names -> JoinUsing names
        | JoinUnconstrained -> JoinUnconstrained
    member this.Join(join : Join<'t1, 'e1>) =
        {   JoinType = join.JoinType
            LeftTable = this.TableExpr(join.LeftTable)
            RightTable = this.TableExpr(join.RightTable)
            Constraint = this.JoinConstraint(join.Constraint)
        }
    member this.TableExpr(table : TableExpr<'t1, 'e1>) =
        {   Source = table.Source
            Value =
                match table.Value with
                | TableOrSubquery sub -> TableOrSubquery <| this.TableOrSubquery(sub)
                | Join join -> Join <| this.Join(join)
        }
    member this.GroupBy(groupBy : GroupBy<'t1, 'e1>) =
        {   By = groupBy.By |> rmap this.Expr
            Having = groupBy.Having |> Option.map this.Expr
        }
    member this.SelectCore(select : SelectCore<'t1, 'e1>) =
        {   Columns = this.ResultColumns(select.Columns)
            From = Option.map this.TableExpr select.From
            Where = Option.map this.Expr select.Where
            GroupBy = Option.map this.GroupBy select.GroupBy
            Info = mapT select.Info
        }
    member this.CompoundTerm(term : CompoundTerm<'t1, 'e1>) : CompoundTerm<'t2, 'e2> =
        {   Source = term.Source
            Value =
                match term.Value with
                | Values vals ->
                    Values (vals |> rmap (fun w -> { Value = rmap this.Expr w.Value; Source = w.Source }))
                | Select select ->
                    Select <| this.SelectCore(select)
            Info = mapT term.Info
        }
    member this.Compound(compound : CompoundExpr<'t1, 'e1>) =
        {   CompoundExpr.Source = compound.Source
            Value = 
                match compound.Value with
                | CompoundTerm term -> CompoundTerm <| this.CompoundTerm(term)
                | Union (expr, term) -> Union (this.Compound(expr), this.CompoundTerm(term))
                | UnionAll (expr, term) -> UnionAll (this.Compound(expr), this.CompoundTerm(term))
                | Intersect (expr, term) -> Intersect (this.Compound(expr), this.CompoundTerm(term))
                | Except (expr, term) -> Except (this.Compound(expr), this.CompoundTerm(term))
        }
    member this.Select(select : SelectStmt<'t1, 'e1>) : SelectStmt<'t2, 'e2> =
        {   Source = select.Source
            Value =
                let select = select.Value
                {   With = Option.map this.WithClause select.With
                    Compound = this.Compound(select.Compound)
                    OrderBy = Option.map (rmap this.OrderingTerm) select.OrderBy
                    Limit = Option.map this.Limit select.Limit
                    Info = mapT select.Info
                }
        }
    member this.ForeignKey(foreignKey) =
        {   ReferencesTable = this.ObjectName(foreignKey.ReferencesTable)
            ReferencesColumns = foreignKey.ReferencesColumns
            Rules = foreignKey.Rules
            Defer = foreignKey.Defer
        }
    member this.ColumnConstraint(constr : ColumnConstraint<'t1, 'e1>) =
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
    member this.ColumnDef(cdef : ColumnDef<'t1, 'e1>) =
        {   Name = cdef.Name
            Type = cdef.Type
            Constraints = rmap this.ColumnConstraint cdef.Constraints
        }
    member this.Alteration(alteration : AlterTableAlteration<'t1, 'e1>) =
        match alteration with
        | RenameTo name -> RenameTo name
        | AddColumn cdef -> AddColumn <| this.ColumnDef(cdef)
    member this.CreateIndex(createIndex : CreateIndexStmt<'t1, 'e1>) =
        {   Unique = createIndex.Unique
            IfNotExists = createIndex.IfNotExists
            IndexName = this.ObjectName(createIndex.IndexName)
            TableName = this.ObjectName(createIndex.TableName)
            IndexedColumns = createIndex.IndexedColumns |> rmap (fun (e, d) -> this.Expr(e), d)
            Where = createIndex.Where |> Option.map this.Expr
        }
    member this.TableIndexConstraint(constr : TableIndexConstraintClause<'t1, 'e1>) =
        {   Type = constr.Type
            IndexedColumns = constr.IndexedColumns |> rmap (fun (e, d) -> this.Expr(e), d)
        }
    member this.TableConstraint(constr : TableConstraint<'t1, 'e1>) =
        {   Name = constr.Name
            TableConstraintType =
                match constr.TableConstraintType with
                | TableIndexConstraint clause ->
                    TableIndexConstraint <| this.TableIndexConstraint(clause)
                | TableForeignKeyConstraint (names, foreignKey) ->
                    TableForeignKeyConstraint (names, this.ForeignKey(foreignKey))
                | TableCheckConstraint expr -> TableCheckConstraint <| this.Expr(expr)
        }
    member this.CreateTableDefinition(createTable : CreateTableDefinition<'t1, 'e1>) =
        {   Columns = createTable.Columns |> rmap this.ColumnDef
            Constraints = createTable.Constraints |> rmap this.TableConstraint
            WithoutRowId = createTable.WithoutRowId
        }
    member this.CreateTable(createTable : CreateTableStmt<'t1, 'e1>) =
        {   Temporary = createTable.Temporary
            IfNotExists = createTable.IfNotExists
            Name = this.ObjectName(createTable.Name)
            As =
                match createTable.As with
                | CreateAsSelect select -> CreateAsSelect <| this.Select(select)
                | CreateAsDefinition def -> CreateAsDefinition <| this.CreateTableDefinition(def)
        }
    member this.CreateView(createView : CreateViewStmt<'t1, 'e1>) =
        {   Temporary = createView.Temporary
            IfNotExists = createView.IfNotExists
            ViewName = this.ObjectName(createView.ViewName)
            ColumnNames = createView.ColumnNames
            AsSelect = this.Select(createView.AsSelect)
        }
    member this.QualifiedTableName(qualified : QualifiedTableName<'t1>) =
        {   TableName = this.ObjectName(qualified.TableName)
            IndexHint = qualified.IndexHint
        }
    member this.Delete(delete : DeleteStmt<'t1, 'e1>) =
        {   With = Option.map this.WithClause delete.With
            DeleteFrom = this.QualifiedTableName(delete.DeleteFrom)
            Where = Option.map this.Expr delete.Where
            OrderBy = Option.map (rmap this.OrderingTerm) delete.OrderBy
            Limit = Option.map this.Limit delete.Limit
        }
    member this.DropObject(drop : DropObjectStmt<'t1>) =
        {   Drop = drop.Drop
            IfExists = drop.IfExists
            ObjectName = this.ObjectName(drop.ObjectName)
        }
    member this.Insert(insert : InsertStmt<'t1, 'e1>) =
        {   With = Option.map this.WithClause insert.With
            Or = insert.Or
            InsertInto = this.ObjectName(insert.InsertInto)
            Columns = insert.Columns
            Data = Option.map this.Select insert.Data
        }
    member this.Update(update : UpdateStmt<'t1, 'e1>) =
        {   With = Option.map this.WithClause update.With
            UpdateTable = this.QualifiedTableName(update.UpdateTable)
            Or = update.Or
            Set = update.Set |> rmap (fun (name, expr) -> name, this.Expr(expr))
            Where = Option.map this.Expr update.Where
            OrderBy = Option.map (rmap this.OrderingTerm) update.OrderBy
            Limit = Option.map this.Limit update.Limit
        }
    member this.Stmt(stmt : Stmt<'t1, 'e1>) =
        match stmt with
        | AlterTableStmt alter ->
            AlterTableStmt <|
                {   Table = this.ObjectName(alter.Table)
                    Alteration = this.Alteration(alter.Alteration)
                }
        | CreateIndexStmt index -> CreateIndexStmt <| this.CreateIndex(index)
        | CreateTableStmt createTable -> CreateTableStmt <| this.CreateTable(createTable)
        | CreateViewStmt createView -> CreateViewStmt <| this.CreateView(createView)
        | DeleteStmt delete -> DeleteStmt <| this.Delete(delete)
        | DropObjectStmt drop -> DropObjectStmt <| this.DropObject(drop)
        | InsertStmt insert -> InsertStmt <| this.Insert(insert)
        | SelectStmt select -> SelectStmt <| this.Select(select)
        | UpdateStmt update -> UpdateStmt <| this.Update(update)
        | BeginStmt -> BeginStmt
        | CommitStmt -> CommitStmt
        | RollbackStmt -> RollbackStmt