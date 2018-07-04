/// Checks that aggregate expressions are used correctly: that is, aggregates are not mixed with non-aggregate
/// expressions of columns unless grouping by those columns.
module private Rezoom.SQL.Compiler.AggregateChecker
open System
open System.Collections.Generic
open Rezoom.SQL.Compiler.InferredTypes

[<NoComparison>]
type private AggReference =
    | Aggregate of SourceInfo
    | ColumnOutsideAggregate of InfExpr

let private columnOutside = function
    | Aggregate _ -> None
    | ColumnOutsideAggregate expr -> Some expr

let rec private aggReferencesSelectCore (select : InfSelectCore) =
    seq {
        for col in select.Columns.Columns do
            match col.Case with
            | Column (ex, _) -> yield! aggReferences ex
            | ColumnsWildcard
            | TableColumnsWildcard _
            | ColumnNav _ -> bug "Typechecker should've eliminated these column cases"
        match select.Where with
        | None -> ()
        | Some where ->
            for ref in aggReferences where do
                // where clause runs prior to aggregation, so we don't yield any aggreferences from it
                // (because column refs are ok) but we do yell if aggregate functions are used
                match ref with
                | Aggregate source ->
                    failAt source Error.aggregateInWhereClause
                | _ -> ()
    }

and private aggReferencesCompoundTerm (term : InfCompoundTerm) =
    match term.Value with
    | Values vs ->
        seq {
            for row in vs do
            for col in row.Value do
                yield! aggReferences col
        }
    | Select sel ->
        aggReferencesSelectCore sel

and private aggReferencesCompound (compound : InfCompoundExpr) =
    match compound.Value with
    | CompoundTerm term -> aggReferencesCompoundTerm term
    | Union (expr, term)
    | UnionAll (expr, term)
    | Intersect (expr, term)
    | Except (expr, term) ->
        Seq.append (aggReferencesCompound expr) (aggReferencesCompoundTerm term)

and private aggReferencesSelect (select : InfSelectStmt) =
    let select = select.Value
    seq {
        yield! aggReferencesCompound select.Compound
        match select.OrderBy with
        | None -> ()
        | Some orderBy ->
            for term in orderBy do yield! aggReferences term.By
        match select.Limit with
        | None -> ()
        | Some limit ->
            yield! aggReferences limit.Limit
            match limit.Offset with
            | None -> ()
            | Some off -> yield! aggReferences off
    }

and private aggReferences (expr : InfExpr) =
    match expr.Value with
    | LiteralExpr _
    | BindParameterExpr _ 
    | ExistsExpr _
    // scalar subqueries have been internally checked by typechecker
    | ScalarSubqueryExpr _ -> Seq.empty
    | ColumnNameExpr _ -> Seq.singleton (ColumnOutsideAggregate expr)
    | InExpr inex ->
        seq {
            yield! aggReferences inex.Input
            match inex.Set.Value with
            | InExpressions exs -> yield! Seq.collect aggReferences exs
            | InSelect sel -> yield! aggReferencesSelect sel
            | InTable _ | InParameter _ -> ()
        }
    | CastExpr cast -> aggReferences cast.Expression
    | CollateExpr collate -> aggReferences collate.Input
    | FunctionInvocationExpr f ->
        let mapping = ASTMapping((fun _ -> ()), fun _ -> ())
        match expr.Info.Function with
        | Some funcInfo when mapping.FunctionInvocation(f).Arguments |> funcInfo.Aggregate |> Option.isSome ->
            Seq.singleton (Aggregate expr.Source)
        | _ ->
            match f.Arguments with
            | ArgumentWildcard -> Seq.empty
            | ArgumentList (_, exprs) -> Seq.collect aggReferences exprs  
    | SimilarityExpr sim ->
        seq {
            yield! aggReferences sim.Input
            yield! aggReferences sim.Pattern
            match sim.Escape with
            | Some escape ->
                yield! aggReferences escape
            | None -> ()
        }
    | BinaryExpr bin -> Seq.append (aggReferences bin.Left) (aggReferences bin.Right)
    | UnaryExpr un -> aggReferences un.Operand
    | BetweenExpr bet ->
        [   aggReferences bet.Input
            aggReferences bet.Low
            aggReferences bet.High
        ] |> Seq.concat
    | CaseExpr case ->
        seq {
            match case.Input with
            | Some inp -> yield! aggReferences inp
            | None -> ()
            for whenExpr, thenExpr in case.Cases do
                yield! aggReferences whenExpr
                yield! aggReferences thenExpr
            match case.Else.Value with
            | Some els -> yield! aggReferences els
            | None -> ()
        }

let check (select : InfSelectCore) =
    let references = aggReferencesSelectCore select
    match select.GroupBy with
    | None ->
        if references |> Seq.exists (function | Aggregate _ -> true | _ -> false) then
            // If we have aggregates, but we're not grouping by anything, we better
            // not have columns referenced outside the aggregates.
            match references |> Seq.tryPick columnOutside with
            | None -> ()
            | Some { Source = src } ->
                failAt src Error.columnNotAggregated
    | Some group ->
        let legal = group.By |> HashSet
        let havingReferences =
            match group.Having with
            | None -> Seq.empty
            | Some having -> aggReferences having
        let outside = Seq.append references havingReferences |> Seq.choose columnOutside
        for outsideExpr in outside do
            if not <| legal.Contains(outsideExpr) then
                failAt outsideExpr.Source Error.columnNotGroupedBy
    select

