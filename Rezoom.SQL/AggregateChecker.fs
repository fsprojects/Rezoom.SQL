/// Checks that aggregate expressions are used correctly: that is, aggregates are not mixed with non-aggregate
/// expressions of columns unless grouping by those columns.
module private Rezoom.SQL.AggregateChecker
open System
open System.Collections.Generic
open Rezoom.SQL.InferredTypes

type private AggReference =
    | Aggregate
    | ColumnOutsideAggregate of InfExpr

let private columnOutside = function
    | Aggregate -> None
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
            yield! aggReferences where
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
    | ExistsExpr _
    | LiteralExpr _
    | BindParameterExpr _
    | RaiseExpr _ -> Seq.empty
    | ColumnNameExpr _ -> Seq.singleton (ColumnOutsideAggregate expr)
    | InExpr inex ->
        seq {
            yield! aggReferences inex.Input
            match inex.Set.Value with
            | InExpressions exs -> yield! Seq.collect aggReferences exs
            | InSelect sel -> yield! aggReferencesSelect sel
            | InTable _ | InParameter _ -> ()
        }
    | ScalarSubqueryExpr subq -> aggReferencesSelect subq
    | CastExpr cast -> aggReferences cast.Expression
    | CollateExpr collate -> aggReferences collate.Input
    | FunctionInvocationExpr f ->
        let mapping = ASTMapping((fun _ -> ()), fun _ -> ())
        match expr.Info.Function with
        | Some funcInfo when mapping.FunctionInvocation(f).Arguments |> funcInfo.Aggregate |> Option.isSome ->
            Seq.singleton Aggregate
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

let private noGroupByMsg =
    "Can't reference column outside of an aggregate function"
    + " because this query uses aggregate functions without a GROUP BY clause"

let notGroupedByMsg =
    "Can't reference column outside of an aggregate function"
    + " because the GROUP BY clause does not include this column"

let check (select : InfSelectCore) =
    let references = aggReferencesSelectCore select
    match select.GroupBy with
    | None ->
        if references |> Seq.contains Aggregate then
            // If we have aggregates, but we're not grouping by anything, we better
            // not have columns referenced outside the aggregates.
            match references |> Seq.tryPick columnOutside with
            | None -> ()
            | Some { Source = src } ->
                failAt src noGroupByMsg
    | Some group ->
        let legal = group.By |> HashSet
        let outside = references |> Seq.choose columnOutside
        for outsideExpr in outside do
            if not <| legal.Contains(outsideExpr) then
                failAt outsideExpr.Source notGroupedByMsg
    select

