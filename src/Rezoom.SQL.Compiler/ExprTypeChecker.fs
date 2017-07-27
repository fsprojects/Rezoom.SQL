namespace Rezoom.SQL.Compiler
open System
open System.Collections.Generic
open Rezoom.SQL.Compiler.InferredTypes

type IQueryTypeChecker =
    abstract member Select : SelectStmt -> InfSelectStmt
    abstract member CreateView : CreateViewStmt -> InfCreateViewStmt

type private ExprTypeChecker(cxt : ITypeInferenceContext, scope : InferredSelectScope, queryChecker : IQueryTypeChecker) =
    member this.Scope = scope
    member this.ObjectName(objectName : ObjectName) = this.ObjectName(objectName, false)
    member this.ObjectName(objectName : ObjectName, allowNotFound) : InfObjectName =
        {   SchemaName = objectName.SchemaName
            ObjectName = objectName.ObjectName
            Source = objectName.Source
            Info =
                if allowNotFound then Missing else
                let inferView view = (concreteMapping cxt).CreateView(queryChecker.CreateView(view))
                match scope.ResolveObjectReference(objectName, inferView) with
                | Found f -> f
                | Ambiguous r
                | NotFound r -> failAt objectName.Source r
        }

    member this.ColumnName(source : SourceInfo, columnName : ColumnName) =
        let tblAlias, tblInfo, name = scope.ResolveColumnReference(columnName) |> foundAt source
        {   Expr.Source = source
            Value =
                {   Table =
                        match tblAlias with
                        | None -> None
                        | Some tblAlias ->
                            {   Source = source
                                SchemaName = None
                                ObjectName = tblAlias
                                Info = TableLike tblInfo
                            } |> Some
                    ColumnName = columnName.ColumnName
                } |> ColumnNameExpr
            Info = name.Expr.Info
        }

    member this.Literal(source : SourceInfo, literal : Literal) =
        {   Expr.Source = source
            Value = LiteralExpr literal
            Info = ExprInfo<_>.OfType(InferredType.OfLiteral(literal))
        }

    member this.BindParameter(source : SourceInfo, par : BindParameter) =
        {   Expr.Source = source
            Value = BindParameterExpr par
            Info = ExprInfo<_>.OfType(cxt.Variable(par))
        }

    member this.Binary(source : SourceInfo, binary : BinaryExpr) =
        let left = this.Expr(binary.Left)
        let right = this.Expr(binary.Right)
        {   Expr.Source = source
            Value =
                {   Operator = binary.Operator
                    Left = left
                    Right = right
                } |> BinaryExpr
            Info =
                {   Type = cxt.Binary(source, binary.Operator, left.Info.Type, right.Info.Type)
                    Idempotent = left.Info.Idempotent && right.Info.Idempotent
                    Function = None
                    Column = None
                }
        }

    member this.Unary(source : SourceInfo, unary : UnaryExpr) =
        let operand = this.Expr(unary.Operand)
        {   Expr.Source = source
            Value =
                {   Operator = unary.Operator
                    Operand = operand
                } |> UnaryExpr
            Info =
                {   Type = cxt.Unary(source, unary.Operator, operand.Info.Type)
                    Idempotent = operand.Info.Idempotent
                    Function = None
                    Column = None
                }
        }

    member this.Cast(source : SourceInfo, cast : CastExpr) =
        let input = this.Expr(cast.Expression)
        let ty = InferredType.OfTypeName(cast.AsType, input.Info.Type)
        {   Expr.Source = source
            Value =
                {   Expression = input
                    AsType = cast.AsType
                } |> CastExpr
            Info =
                {   Type = ty
                    Idempotent = input.Info.Idempotent
                    Function = None
                    Column = None
                }
        }

    member this.Collation(source : SourceInfo, collation : CollationExpr) =
        let input = this.Expr(collation.Input)
        ignore <| cxt.Unify(source, input.Info.Type, InferredType.String)
        {   Expr.Source = source
            Value = 
                {   Input = this.Expr(collation.Input)
                    Collation = collation.Collation
                } |> CollateExpr
            Info =
                {   Type = input.Info.Type
                    Idempotent = input.Info.Idempotent
                    Function = None
                    Column = None
                }
        }

    member this.FunctionArguments(args : FunctionArguments) =
        match args with
        | ArgumentWildcard -> ArgumentWildcard
        | ArgumentList (distinct, args) ->
            ArgumentList (distinct, args |> Array.map this.Expr)

    member this.FunctionInvocation(source : SourceInfo, func : FunctionInvocationExpr) =
        match scope.Model.Builtin.Functions.TryFind(func.FunctionName) with
        | None -> failAt source <| Error.noSuchFunction func.FunctionName
        | Some funcType ->
            let args, output = cxt.Function(source, funcType, this.FunctionArguments(func.Arguments))
            {   Expr.Source = source
                Value =
                    if funcType.Erased then
                        match args with
                        | ArgumentList (None, [| arg |]) -> arg.Value
                        | _ ->
                            bug <| sprintf "Bug in backend: erased function ``%O`` must take a single argument"
                                func.FunctionName
                    else
                        { FunctionName = func.FunctionName; Arguments = args } |> FunctionInvocationExpr
                Info =
                    {   Type = output
                        Idempotent =
                            funcType.Idempotent &&
                            match args with
                            | ArgumentWildcard -> true
                            | ArgumentList (_, args) -> args |> Seq.forall (fun a -> a.Info.Idempotent)
                        Function = Some funcType
                        Column = None
                    }
            }

    member this.Similarity(source : SourceInfo, sim : SimilarityExpr) =
        let input = this.Expr(sim.Input)
        let pattern = this.Expr(sim.Pattern)
        let escape = Option.map this.Expr sim.Escape
        let output =
            let inputType = cxt.Unify(source, input.Info.Type, StringType)
            let patternType = cxt.Unify(source, pattern.Info.Type, StringType)
            match escape with
            | None -> ()
            | Some escape -> ignore <| cxt.Unify(source, escape.Info.Type, StringType)
            let unified = cxt.Unify(source, inputType, patternType)
            InferredType.Dependent(unified, BooleanType)
        {   Expr.Source = source
            Value =
                {   Invert = sim.Invert
                    Operator = sim.Operator
                    Input = input
                    Pattern = pattern
                    Escape = escape
                } |> SimilarityExpr
            Info =
                {   Type = output
                    Idempotent = input.Info.Idempotent && pattern.Info.Idempotent
                    Function = None
                    Column = None
                }
        }

    member this.Between(source : SourceInfo, between : BetweenExpr) =
        let input = this.Expr(between.Input)
        let low = this.Expr(between.Low)
        let high = this.Expr(between.High)
        let operandType = cxt.Unify(source, [ input.Info.Type; low.Info.Type; high.Info.Type ])
        {   Expr.Source = source
            Value = { Invert = between.Invert; Input = input; Low = low; High = high } |> BetweenExpr
            Info =
                {   Type = InferredType.Dependent(operandType, BooleanType)
                    Idempotent = input.Info.Idempotent && low.Info.Idempotent && high.Info.Idempotent
                    Function = None
                    Column = None
                }
        }

    member this.TableInvocation(table : TableInvocation) =
        {   Table = this.ObjectName(table.Table)
            Arguments = table.Arguments |> Option.map (rmap this.Expr)
        }

    member this.In(source : SourceInfo, inex : InExpr) =
        let input = this.Expr(inex.Input)
        let set, idempotent =
            match inex.Set.Value with
            | InExpressions exprs ->
                let exprs = exprs |> rmap this.Expr
                let involvedInfos =
                    Seq.append (Seq.singleton input) exprs |> Seq.map (fun e -> e.Info) |> toReadOnlyList
                ignore <| cxt.Unify(inex.Set.Source, involvedInfos |> Seq.map (fun e -> e.Type))
                InExpressions exprs,
                    (involvedInfos |> Seq.forall (fun i -> i.Idempotent))
            | InSelect select ->
                let select = queryChecker.Select(select)
                let columnCount = select.Value.Info.Columns.Count
                if columnCount <> 1 then
                    failAt select.Source <| Error.multipleColumnsForInSelect columnCount
                InSelect select, (input.Info.Idempotent && select.Value.Info.Idempotent)
            | InTable table ->
                let table = this.TableInvocation(table)
                InTable table, input.Info.Idempotent
            | InParameter par ->
                cxt.UnifyList(inex.Set.Source, input.Info.Type.InferredType, par)
                InParameter par, true
        {   Expr.Source = source
            Value =
                {   Invert = inex.Invert
                    Input = this.Expr(inex.Input)
                    Set = { Source = inex.Set.Source; Value = set }
                } |> InExpr
            Info =
                {   Type = InferredType.Dependent(input.Info.Type, BooleanType)
                    Idempotent = input.Info.Idempotent && idempotent
                    Function = None
                    Column = None
                }
        }

    member this.Case(source : SourceInfo, case : CaseExpr) =
        let case =
            {   Input = Option.map this.Expr case.Input
                Cases =
                    [|
                        for whenExpr, thenExpr in case.Cases ->
                            this.Expr(whenExpr), this.Expr(thenExpr)
                    |]
                Else =
                    {   Source = case.Else.Source
                        Value = Option.map this.Expr case.Else.Value
                    }
            }
        let outputType =
            seq {
                for _, thenExpr in case.Cases -> thenExpr.Info.Type
                match case.Else.Value with
                | None -> yield InferredType.OfLiteral(NullLiteral)
                | Some els -> yield els.Info.Type
            } |> fun s -> cxt.Unify(source, s)
        cxt.Unify(source,
            seq {
                yield
                    match case.Input with
                    | None -> InferredType.Boolean
                    | Some input -> input.Info.Type
                for whenExpr, _ in case.Cases -> whenExpr.Info.Type
            }) |> ignore
        let subExprs =
            seq {
                match case.Input with
                | None -> ()
                | Some input -> yield input
                for whenExpr, thenExpr in case.Cases do
                    yield whenExpr
                    yield thenExpr
                match case.Else.Value with
                | None -> ()
                | Some els -> yield els
            }
        {   Expr.Source = source
            Value = case |> CaseExpr
            Info =
                {   Type = outputType
                    Idempotent = subExprs |> Seq.forall (fun e -> e.Info.Idempotent)
                    Function = None
                    Column = None
                }
        }

    member this.Exists(source : SourceInfo, exists : SelectStmt) =
        let exists = queryChecker.Select(exists)
        {   Expr.Source = source
            Value = ExistsExpr exists
            Info =
                {   Type = InferredType.Boolean
                    Idempotent = exists.Value.Info.Idempotent
                    Function = None
                    Column = None
                }
        }

    member this.ScalarSubquery(source : SourceInfo, select : SelectStmt) =
        let select = queryChecker.Select(select)
        let tbl = select.Value.Info.Table.Query
        if tbl.Columns.Count <> 1 then
            failAt source <| Error.multipleColumnsForScalarSubquery tbl.Columns.Count
        {   Expr.Source = source
            Value = ScalarSubqueryExpr select
            Info = tbl.Columns.[0].Expr.Info
        }

    member this.Expr(expr : Expr) : InfExpr =
        let source = expr.Source
        match expr.Value with
        | LiteralExpr lit -> this.Literal(source, lit)
        | BindParameterExpr par -> this.BindParameter(source, par)
        | ColumnNameExpr name -> this.ColumnName(source, name)
        | CastExpr cast -> this.Cast(source, cast)
        | CollateExpr collation -> this.Collation(source, collation)
        | FunctionInvocationExpr func -> this.FunctionInvocation(source, func)
        | SimilarityExpr sim -> this.Similarity(source, sim)
        | BinaryExpr bin -> this.Binary(source, bin)
        | UnaryExpr un -> this.Unary(source, un)
        | BetweenExpr between -> this.Between(source, between)
        | InExpr inex -> this.In(source, inex)
        | ExistsExpr select -> this.Exists(source, select)
        | CaseExpr case -> this.Case(source, case)
        | ScalarSubqueryExpr select -> this.ScalarSubquery(source, select)

    member this.Expr(expr : Expr, ty : CoreColumnType) =
        let expr = this.Expr(expr)
        ignore <| cxt.Unify(expr.Source, expr.Info.Type, ty)
        expr