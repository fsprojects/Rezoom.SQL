namespace Rezoom.ORM.SQLProvider
open System
open System.Collections.Generic
open Rezoom.ORM.SQLProvider.InferredTypes

module private InferenceExtensions =
    type ITypeInferenceContext with
        member this.Unify(inferredType, coreType : CoreColumnType) =
            this.Unify(inferredType, DependentlyNullType(inferredType, coreType))
        member this.Unify(inferredType, resultType : Result<InferredType, string>) =
            match resultType with
            | Ok t -> this.Unify(inferredType, t)
            | Error _ as e -> e
        member this.Unify(types : InferredType seq) =
            types
            |> Seq.fold
                (function | Ok s -> (fun t -> this.Unify(s, t)) | Error _ as e -> (fun _ -> e))
                (Ok InferredType.Any)
open InferenceExtensions

type private Inferrer(cxt : ITypeInferenceContext, scope : InferredSelectScope) =
    member this.ResolveTableInvocation(source : SourceInfo, tableInvocation : TableInvocation) =
        match tableInvocation.Arguments with
        | None ->
            foundAt source (scope.ResolveTableReference(tableInvocation.Table))
        | Some args ->
            failAt source "Table invocations with arguments are not supported"

    member this.InferScalarSubqueryType(subquery : SelectStmt) : InferredType =
        failwith "Not supported -- need to support fancy query table-oriented stuff"

    member this.InferSimilarityExprType(op, input, pattern, escape) =
        result {
            let! inputType = cxt.Unify(this.InferExprType(input), StringType)
            let! patternType = cxt.Unify(this.InferExprType(input), StringType)
            match escape with
            | None -> ()
            | Some escape -> ignore <| cxt.Unify(this.InferExprType(escape), StringType)
            let! unified = cxt.Unify(inputType, patternType)
            return DependentlyNullType(unified, BooleanType)
        }

    member this.InferInExprType(input, set) =
        let inputType = this.InferExprType(input)
        result {
            let! setType =
                match set with
                | InExpressions exprs ->
                    let commonType = cxt.Unify(exprs |> Seq.map this.InferExprType)
                    cxt.Unify(inputType, commonType)
                | InSelect select ->
                    cxt.Unify(inputType, this.InferScalarSubqueryType(select))
                | InTable table ->
                    failwith "Not supported -- need to figure out how to deal with table invocations"
            return DependentlyNullType(setType, BooleanType)
        }

    member this.RequireExprType(expr : Expr, mustMatch : CoreColumnType) =
        let inferred = this.InferExprType(expr)
        cxt.Unify(inferred, mustMatch)
        |> resultAt expr.Source
        |> ignore

    member this.InferExprType(expr : Expr) : InferredType =
        match expr.Value with
        | LiteralExpr lit -> InferredType.OfLiteral(lit)
        | BindParameterExpr par -> cxt.Variable(par)
        | ColumnNameExpr name ->
            let column = scope.ResolveColumnReference(name) |> foundAt expr.Source
            column.InferredType
        | CastExpr cast -> InferredType.OfTypeName(cast.AsType, this.InferExprType(cast.Expression))
        | CollateExpr (subExpr, collation) ->
            let inferred = this.InferExprType(subExpr)
            cxt.Unify(inferred, DependentlyNullType(inferred, StringType))
            |> resultAt expr.Source
        | FunctionInvocationExpr funcInvoke ->
            failwith "Not supported -- need to make list of built-in SQLite functions"
        | SimilarityExpr (op, input, pattern, escape) ->
            this.InferSimilarityExprType(op, input, pattern, escape)
            |> resultAt expr.Source
        | BinaryExpr (binop, left, right) ->
            failwith "Not supported -- need to define method to handle the whole zoo of binary operators"
        | UnaryExpr (unop, operand) ->
            failwith "Not supported -- need to define method to handle the whole zoo of unary operators"
        | BetweenExpr (input, low, high)
        | NotBetweenExpr (input, low, high) ->
            result {
                let! unified = cxt.Unify([ input; low; high ] |> Seq.map this.InferExprType)
                return DependentlyNullType(unified, BooleanType)
            } |> resultAt expr.Source
        | InExpr (input, set)
        | NotInExpr (input, set) ->
            this.InferInExprType(input, set)
            |> resultAt expr.Source
        | ExistsExpr select ->
            failwith "Not supported -- need to analyze subquery for validity"
        | CaseExpr cases ->
            failwith "Not supported -- need to check cases for boolean-ness"
        | ScalarSubqueryExpr subquery -> this.InferScalarSubqueryType(subquery)
        | RaiseExpr _ -> InferredType.Any

    member this.CTEScope(withClause) =
        match withClause with
        | None -> scope
        | Some ctes ->
            ctes.Tables |> Seq.fold (fun scope cte ->
                let cteQuery =
                    match cte.ColumnNames with
                    | None -> this.InferQueryType(cte.AsSelect)
                    | Some names ->
                        this.InferQueryType(cte.AsSelect).RenameColumns(names.Value)
                        |> resultAt names.Source
                { scope with
                    CTEVariables = appendDicts scope.CTEVariables (ciSingle cte.Name cteQuery)
                }) scope

    member private this.TableExprScope(dict : Dictionary<string, _>, tableExpr : TableExpr) =
        let add name query =
            if dict.ContainsKey(name) then
                failAt tableExpr.Source <| sprintf "Table name already in scope: ``%s``" name
            else
                dict.Add(name, query)
        match tableExpr.Value with
        | TableOrSubquery (Table (invoc, alias, indexHint)) ->
            let tbl = this.ResolveTableInvocation(tableExpr.Source, invoc)
            let name = defaultArg alias invoc.Table.ObjectName
            add name tbl
            tbl
        | TableOrSubquery (Subquery (select, alias)) ->
            let sub = this.InferQueryType(select)
            match alias with
            | Some alias -> add alias sub
            | None -> ()
            sub
        | AliasedTableExpr (expr, alias) ->
            let sub = this.TableExprScope(dict, expr)
            match alias with
            | Some alias -> add alias sub
            | None -> ()
            sub
        | Join join ->
            let left = this.TableExprScope(dict, join.LeftTable)
            let right = this.TableExprScope(dict, join.RightTable)
            left.Append(right)

    member this.TableExprScope(tableExpr : TableExpr) =
        let dict = Dictionary(StringComparer.OrdinalIgnoreCase)
        let wildcard = this.TableExprScope(dict, tableExpr)
        { FromVariables = dict; Wildcard = wildcard }

    member this.ValidateTableExprConstraints(tableExpr : TableExpr) =
        match tableExpr.Value with
        | TableOrSubquery _ -> ()
        | AliasedTableExpr (ex, _) -> this.ValidateTableExprConstraints(ex)
        | Join join ->
            this.ValidateTableExprConstraints(join.LeftTable)
            this.ValidateTableExprConstraints(join.RightTable)
            match join.JoinType, join.Constraint with
            | Natural _, JoinUnconstrained ->
                let columnSet texpr = 
                    this.TableExprScope(texpr).Wildcard.Columns
                    |> Seq.map (fun c -> c.ColumnName.ToUpperInvariant())
                    |> Set.ofSeq
                let intersection = Set.intersect (columnSet join.LeftTable) (columnSet join.RightTable)
                if Set.isEmpty intersection then
                    failAt tableExpr.Source
                        "The left and right sides of a NATURAL JOIN must have at least one column name in common"
            | Natural _, _ -> failAt tableExpr.Source "A NATURAL JOIN cannot have an ON or USING clause"
            | _, JoinOn ex -> this.RequireExprType(ex, BooleanType)
            | _, JoinUsing names ->
                let leftColumns = this.TableExprScope(join.LeftTable).Wildcard
                let rightColumns = this.TableExprScope(join.RightTable).Wildcard
                for name in names do
                    leftColumns.ColumnByName(name) |> foundAt tableExpr.Source |> ignore
                    rightColumns.ColumnByName(name) |> foundAt tableExpr.Source |> ignore
            | _, JoinUnconstrained -> ()

    member this.InferSelectCoreType(select : SelectCore) : InferredQuery =
        let this, scope =
            match select.From with
            | None -> this, scope
            | Some tableExpr ->
                let fromScope = { scope with FromClause = this.TableExprScope(tableExpr) |> Some }
                let fromInferrer = Inferrer(cxt, fromScope)
                fromInferrer.ValidateTableExprConstraints(tableExpr)
                fromInferrer, fromScope
        match select.Where with
        | None -> ()
        | Some whereExpr -> this.RequireExprType(whereExpr, BooleanType)
        match select.GroupBy with
        | None -> ()
        | Some groupBy ->
            for expr in groupBy.By do this.RequireExprType(expr, AnyType)
            match groupBy.Having with
            | None -> ()
            | Some havingExpr -> this.RequireExprType(havingExpr, BooleanType)
        let resultColumns =
            seq {
              for col in select.Columns.Columns do
                match col.Value with
                | ColumnsWildcard ->
                    match scope.FromClause with
                    | None -> failAt col.Source "Can't use wildcard without a FROM clause"
                    | Some from -> yield! from.Wildcard.Columns
                | TableColumnsWildcard objectName ->
                    match scope.FromClause with
                    | None ->
                        failAt col.Source <|
                            sprintf "Can't use wildcard ``%O.*`` without a FROM clause" objectName
                    | Some from ->
                        let table = from.ResolveTable(objectName) |> foundAt col.Source
                        yield! table.Columns
                | Column (expr, alias) ->
                    let inferred = this.InferExprType(expr)
                    let name, fromAlias =
                        match alias with
                        | None ->
                            match expr.Value with
                            | ColumnNameExpr name -> name.ColumnName, name.Table |> Option.map (fun t -> t.ObjectName)
                            | _ -> failAt expr.Source "An expression-valued column must have an alias"
                        | Some name -> name, None
                    yield
                        { ColumnName = name; InferredType = inferred; FromAlias = fromAlias }
            } |> toReadOnlyList
        {
            Columns = resultColumns
        }
    member this.InferQueryType(select : SelectStmt) : InferredQuery =
        let scope = this.CTEScope(select.Value.With)
        let this = Inferrer(cxt, scope)
        failwith ""
