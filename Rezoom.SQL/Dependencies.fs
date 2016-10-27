module Rezoom.SQL.Dependencies
open System
open System.Collections.Generic
open Rezoom.SQL

type DependencyType =
    | ReadReference
    | WriteReference

type DependencyFinder() =
    member __.ReferenceObject(dependency : DependencyType, name : TObjectName) =
        ()
    member this.ReferenceColumn(dependency : DependencyType, name : TColumnName) =
        // what about columns/tables local to the query?
        match name.Table with
        | None -> ()
        | Some tbl ->
            this.ReferenceObject(dependency, tbl)
    member this.Binary(binary : TBinaryExpr) =
        this.Expr(binary.Left)
        this.Expr(binary.Right)
    member this.Unary(unary : TUnaryExpr) = this.Expr(unary.Operand)
    member this.Cast(cast : TCastExpr) = this.Expr(cast.Expression)
    member this.Collation(collation : TCollationExpr) = this.Expr(collation.Input)
    member this.FunctionInvocation(func : TFunctionInvocationExpr) =
        match func.Arguments with
        | ArgumentList (_, exprs) ->
            for expr in exprs do this.Expr(expr)
        | _ -> ()
    member this.Similarity(sim : TSimilarityExpr) =
        this.Expr(sim.Input)
        this.Expr(sim.Pattern)
        Option.iter this.Expr sim.Escape
    member this.Between(between : TBetweenExpr) =
        this.Expr(between.Input)
        this.Expr(between.Low)
        this.Expr(between.High)
    member this.In(inex : TInExpr) =
        this.Expr(inex.Input)
        match inex.Set.Value with
        | InExpressions exprs -> for expr in exprs do this.Expr(expr)
        | InSelect select -> this.Select(select)
        | InTable table -> this.TableInvocation(table)
    member this.Case(case : TCaseExpr) =
        Option.iter this.Expr case.Input
        for whenExpr, thenExpr in case.Cases do
            this.Expr(whenExpr)
            this.Expr(thenExpr)
        Option.iter this.Expr case.Else.Value
    member this.ExprType(expr : TExprType) : unit =
        match expr with
        | ColumnNameExpr name -> this.ReferenceColumn(ReadReference, name)
        | CastExpr cast -> this.Cast(cast)
        | CollateExpr collation -> this.Collation(collation)
        | FunctionInvocationExpr func -> this.FunctionInvocation(func)
        | SimilarityExpr sim -> this.Similarity(sim)
        | BinaryExpr bin -> this.Binary(bin)
        | UnaryExpr un -> this.Unary(un)
        | BetweenExpr between -> this.Between(between)
        | InExpr inex -> this.In(inex)
        | ExistsExpr select -> this.Select(select)
        | CaseExpr case -> this.Case(case)
        | ScalarSubqueryExpr select -> this.Select(select)
        | RaiseExpr _
        | LiteralExpr _
        | BindParameterExpr _ -> ()
    member this.Expr(expr : TExpr) = this.ExprType(expr.Value)
    member this.TableInvocation(table : TTableInvocation) =
        this.ReferenceObject(ReadReference, table.Table)
        match table.Arguments with
        | Some args -> for arg in args do this.Expr(arg)
        | None -> ()
    member this.CTE(cte : TCommonTableExpression) = this.Select(cte.AsSelect)
    member this.WithClause(withClause : TWithClause) = for table in withClause.Tables do this.CTE(table)
    member this.OrderingTerm(orderingTerm : TOrderingTerm) = this.Expr(orderingTerm.By)
    member this.Limit(limit : TLimit) =
        this.Expr(limit.Limit)
        Option.iter this.Expr limit.Offset
    member this.ResultColumn(resultColumn : TResultColumn) =
        match resultColumn.Case with
        | Column (expr, _) -> this.Expr(expr)
        | _ -> failwith "BUG: result column wildcards should've been expanded by now"      
    member this.ResultColumns(resultColumns : TResultColumns) =
        for col in resultColumns.Columns do this.ResultColumn(col)
    member this.TableOrSubquery(table : TTableOrSubquery) =
        match table.Table with
        | Table (tinvoc, _) -> this.TableInvocation(tinvoc)
        | Subquery select -> this.Select(select)
    member this.JoinConstraint(constr : TJoinConstraint) =  
        match constr with
        | JoinOn expr -> this.Expr(expr)
        | JoinUnconstrained -> ()
    member this.Join(join : TJoin) =
        this.TableExpr(join.LeftTable)
        this.TableExpr(join.RightTable)
        this.JoinConstraint(join.Constraint)
    member this.TableExpr(table : TTableExpr) =
        match table.Value with
        | TableOrSubquery sub -> this.TableOrSubquery(sub)
        | Join join -> this.Join(join)
    member this.GroupBy(groupBy : TGroupBy) =
        for by in groupBy.By do this.Expr(by)
        Option.iter this.Expr groupBy.Having
    member this.SelectCore(select : TSelectCore) =
        this.ResultColumns(select.Columns)
        Option.iter this.TableExpr select.From
        Option.iter this.Expr select.Where
        Option.iter this.GroupBy select.GroupBy
    member this.CompoundTerm(term : TCompoundTerm) =
        match term.Value with
        | Values rows ->
            for row in rows do
            for col in row.Value do
                this.Expr(col)
        | Select select -> this.SelectCore(select)
    member this.Compound(compound : TCompoundExpr) =
            match compound.Value with
            | CompoundTerm term -> this.CompoundTerm(term)
            | Union (expr, term)
            | UnionAll (expr, term)
            | Intersect (expr, term)
            | Except (expr, term) ->
                this.Compound(expr)
                this.CompoundTerm(term)
    member this.Select(select : TSelectStmt) : unit =
        Option.iter this.WithClause select.Value.With
        this.Compound(select.Value.Compound)
        Option.iter this.Limit select.Value.Limit
        match select.Value.OrderBy with
        | Some terms -> for term in terms do this.OrderingTerm(term)
        | None -> ()
    member this.Delete(delete : TDeleteStmt) =
        this.ReferenceObject(WriteReference, delete.DeleteFrom.TableName)
        Option.iter this.WithClause delete.With
        Option.iter this.Expr delete.Where
        Option.iter this.Limit delete.Limit
        match delete.OrderBy with
        | Some terms -> for term in terms do this.OrderingTerm(term)
        | None -> ()
    member this.Insert(insert : TInsertStmt) =
        this.ReferenceObject(WriteReference, insert.InsertInto)
        Option.iter this.WithClause insert.With
        Option.iter this.Select insert.Data
    member this.Update(update : TUpdateStmt) =
        this.ReferenceObject(WriteReference, update.UpdateTable.TableName)
        Option.iter this.WithClause update.With
        Option.iter this.Expr update.Where
        Option.iter this.Limit update.Limit
        for _, setTo in update.Set do this.Expr(setTo)
        match update.OrderBy with
        | Some terms -> for term in terms do this.OrderingTerm(term)
        | None -> ()
    member this.Stmt(stmt : TStmt) =
        match stmt with
        | DeleteStmt delete -> this.Delete(delete)
        | InsertStmt insert -> this.Insert(insert)
        | SelectStmt select -> this.Select(select)
        | UpdateStmt update -> this.Update(update)
        | AlterTableStmt _
        | CreateIndexStmt _
        | CreateTableStmt _
        | CreateViewStmt _
        | DropObjectStmt _
        | BeginStmt
        | CommitStmt
        | RollbackStmt -> ()