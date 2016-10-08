namespace StaticQL.Translators
open StaticQL
open StaticQL.Mapping
open StaticQL.BackendUtilities

[<AbstractClass>]
type DefaultExprTranslator(statement : StatementTranslator, indexer : IParameterIndexer) =
    inherit ExprTranslator(statement, indexer)
    override __.BinaryOperator op =
        CommandText <|
        match op with
        | Concatenate -> "||"
        | Multiply -> "*"
        | Divide -> "/"
        | Modulo -> "%"
        | Add -> "+"
        | Subtract -> "-"
        | BitShiftLeft -> "<<"
        | BitShiftRight -> ">>"
        | BitAnd -> "&"
        | BitOr -> "|"
        | LessThan -> "<"
        | LessThanOrEqual -> "<="
        | GreaterThan -> ">"
        | GreaterThanOrEqual -> ">="
        | Equal -> "="
        | NotEqual -> "<>"
        | Is -> "IS"
        | IsNot -> "IS NOT"
        | And -> "AND"
        | Or -> "OR"
    override __.UnaryOperator op =
        CommandText <|
        match op with
        | Negative -> "-"
        | Not -> "NOT"
        | BitNot -> "~"
        | NotNull -> "NOT NULL"
        | IsNull -> "IS NULL"
    override __.SimilarityOperator op =
        CommandText <|
        match op with
        | Like -> "LIKE"
        | Glob -> "GLOB"
        | Match -> "MATCH"
        | Regexp -> "REGEXP"
    override __.BindParameter par = indexer.ParameterIndex(par) |> Parameter
    override this.ObjectName name =
        seq {
            match name.SchemaName with
            | Some schema ->
                yield text (schema.Value + ".")
            | None -> ()
            yield this.Name(name.ObjectName) 
        }
    override this.ColumnName col =
        seq {
            match col.Table with
            | Some tbl ->
                yield! this.ObjectName(tbl)
                yield text "."
            | None -> ()
            yield this.Name(col.ColumnName)
        }
    override this.Cast(castExpr) =
        seq {
            yield text "CAST("
            yield! this.Expr(castExpr.Expression)
            yield ws
            yield text "AS"
            yield ws
            yield! this.TypeName(castExpr.AsType)
            yield text ")"
        }
    override this.Collate(expr, collation) =
        seq {
            yield! this.Expr(expr)
            yield ws
            yield text "COLLATE"
            yield ws
            yield this.Name(collation)
        }
    override this.Invoke(func) =
        seq {
            yield this.Name(func.FunctionName)
            yield text "("
            match func.Arguments with
            | ArgumentWildcard -> yield text "*"
            | ArgumentList (distinct, args) ->
                match distinct with
                | Some distinct ->
                    yield text "DISTINCT"
                    yield ws
                | None -> ()
                yield! args |> Seq.map this.Expr |> join ","
            yield text ")"
        }
    override this.Similarity(sim : TSimilarityExpr) =
        seq {
            yield! this.Expr(sim.Input)
            yield ws
            if sim.Invert then
                yield text "NOT"
                yield ws
            yield this.SimilarityOperator(sim.Operator)
            yield ws
            yield! this.Expr(sim.Pattern)
            match sim.Escape with
            | None -> ()
            | Some escape ->
                yield ws
                yield text "ESCAPE"
                yield ws
                yield! this.Expr(escape)
        }
    override this.Binary(bin) =
        seq {
            yield! this.Expr(bin.Left)
            yield ws
            yield this.BinaryOperator(bin.Operator)
            yield ws
            yield! this.Expr(bin.Right)
        }
    override this.Unary(un) =
        match un.Operator with
        | Negative
        | Not
        | BitNot ->
            seq {
                yield this.UnaryOperator(un.Operator)
                yield ws
                yield! this.Expr(un.Operand)
            }
        | NotNull
        | IsNull ->
            seq {
                yield! this.Expr(un.Operand)
                yield ws
                yield this.UnaryOperator(un.Operator)
            }
    override this.Between(between) =
        seq {
            yield! this.Expr(between.Input)
            yield ws
            if between.Invert then
                yield text "NOT"
                yield ws
            yield text "BETWEEN"
            yield ws
            yield! this.Expr(between.Low)
            yield ws
            yield text "AND"
            yield ws
            yield! this.Expr(between.High)
        }
    override this.Table(tbl) =
        seq {
            yield! this.ObjectName(tbl.Table)
            match tbl.Arguments with
            | None -> ()
            | Some args ->
                yield text "("
                yield! args |> Seq.map this.Expr |> join ","
                yield text ")"
        }
    override this.In(inex) =
        seq {
            yield! this.Expr(inex.Input)
            yield ws
            if inex.Invert then
                yield text "NOT"
                yield ws
            yield text "IN"
            yield ws
            match inex.Set.Value with
            | InExpressions exprs ->
                yield text "("
                yield! exprs |> Seq.map this.Expr |> join ","
                yield text ")"
            | InSelect select ->
                yield text "("
                yield! statement.Select(select)
                yield text ")"
            | InTable tbl ->
                yield! this.Table(tbl)
        }
    override this.Case(case) =
        seq {
            yield text "CASE"
            yield ws
            match case.Input with
            | None -> ()
            | Some input ->
                yield! this.Expr(input)
                yield ws
            for input, output in case.Cases do
                yield text "WHEN"
                yield ws
                yield! this.Expr(input)
                yield ws
                yield text "THEN"
                yield ws
                yield! this.Expr(output)
            match case.Else.Value with
            | None -> ()
            | Some els ->
                yield ws
                yield text "ELSE"
                yield ws
                yield! this.Expr(els)
            yield ws
            yield text "END"
        }
    override this.Raise(raise) =
        let raiseMsg ty msg =
            seq {
                yield text "RAISE("
                yield text ty
                yield text ","
                yield this.Literal.StringLiteral(msg)
                yield text ")"
            }
        match raise with
        | RaiseIgnore -> Seq.singleton (text "RAISE(IGNORE)")
        | RaiseRollback msg -> raiseMsg "ROLLBACK" msg
        | RaiseAbort msg -> raiseMsg "ABORT" msg
        | RaiseFail msg -> raiseMsg "FAIL" msg
    override this.Exists(subquery) =
        seq {
            yield text "EXISTS("
            yield! statement.Select(subquery)
            yield text ")"
        }
    override this.ScalarSubquery(subquery) =
        seq {
            yield text "("
            yield! statement.Select(subquery)
            yield text ")"
        }
    override __.NeedsParens(expr) =
        match expr with
        | LiteralExpr _
        | BindParameterExpr _
        | ColumnNameExpr _ 
        | CastExpr _
        | FunctionInvocationExpr _
        | ScalarSubqueryExpr _ -> false
        | _ -> true
    override this.Expr(expr) =
        let needsParens = this.NeedsParens(expr.Value)
        seq {
            if needsParens then yield text "("
            yield!
                match expr.Value with
                | LiteralExpr lit -> this.Literal.Literal(lit) |> Seq.singleton
                | BindParameterExpr bind -> this.BindParameter(bind) |> Seq.singleton
                | ColumnNameExpr name -> this.ColumnName(name)
                | CastExpr cast -> this.Cast(cast)
                | CollateExpr { Input = expr; Collation = collation } -> this.Collate(expr, collation)
                | FunctionInvocationExpr func -> this.Invoke(func)
                | SimilarityExpr sim -> this.Similarity(sim)
                | BinaryExpr bin -> this.Binary(bin)
                | UnaryExpr un -> this.Unary(un)
                | BetweenExpr between -> this.Between(between)
                | InExpr inex -> this.In(inex)
                | ExistsExpr select -> this.Exists(select)
                | CaseExpr case -> this.Case(case)
                | ScalarSubqueryExpr subquery -> this.ScalarSubquery(subquery)
                | RaiseExpr raise -> this.Raise(raise)
            if needsParens then yield text ")"
        }


