module SQLow.AST
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
        {   Operator = sim.Operator
            Input = this.Expr(sim.Input)
            Pattern = this.Expr(sim.Pattern)
            Escape = Option.map this.Expr sim.Escape
        }
    member this.Between(between : BetweenExpr<'t1, 'e1>) =
        {   Input = this.Expr(between.Input)
            Low = this.Expr(between.Low)
            High = this.Expr(between.High)
        }
    member this.In(inex : InExpr<'t1, 'e1>) =
        {   Input = this.Expr(inex.Input)
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
        | NotSimilarityExpr sim -> NotSimilarityExpr <| this.Similarity(sim)
        | BinaryExpr bin -> BinaryExpr <| this.Binary(bin)
        | UnaryExpr un -> UnaryExpr <| this.Unary(un)
        | BetweenExpr between -> BetweenExpr <| this.Between(between)
        | NotBetweenExpr between -> BetweenExpr <| this.Between(between)
        | InExpr inex -> InExpr <| this.In(inex)
        | NotInExpr inex -> NotInExpr <| this.In(inex)
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
    member this.Select(select : SelectStmt<'t1, 'e1>) : SelectStmt<'t2, 'e2> =
        failwith ""