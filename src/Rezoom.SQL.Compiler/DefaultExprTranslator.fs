namespace Rezoom.SQL.Compiler.Translators
open Rezoom.SQL.Compiler
open Rezoom.SQL.Compiler.BackendUtilities
open Rezoom.SQL.Mapping

type DefaultExprTranslator(statement : StatementTranslator, indexer : IParameterIndexer) =
    inherit ExprTranslator()
    override __.Literal = upcast DefaultLiteralTranslator()
    override __.Name(name) =
        "\"" + name.Value.Replace("\"", "\"\"") + "\""
        |> text
    override this.CollationName(name) = this.Name(name)
    override __.TypeName(name, _) =
        (Seq.singleton << text) <|
            match name with
            | BooleanTypeName -> "BOOL"
            | GuidTypeName -> "GUID"
            | IntegerTypeName Integer16 -> "INT16"
            | IntegerTypeName Integer32 -> "INT32"
            | IntegerTypeName Integer64 -> "INT64"
            | FloatTypeName Float32 -> "FLOAT32"
            | FloatTypeName Float64 -> "FLOAT64"
            | StringTypeName(Some size) -> "STRING(" + string size + ")"
            | StringTypeName(None) -> "STRING"
            | BinaryTypeName(Some size) -> "BINARY(" + string size + ")"
            | BinaryTypeName(None) -> "BINARY"
            | DecimalTypeName -> "DECIMAL"
            | DateTimeTypeName -> "DATETIME"
            | DateTimeOffsetTypeName -> "DATETIMEOFFSET"
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
    override __.SimilarityOperator(invert, op) =
        CommandText <|
        (if invert then "NOT " else "")
        +
            match op with
            | Like -> "LIKE"
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
            yield! this.Expr(castExpr.Expression, FirstClassValue)
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
            yield this.CollationName(collation)
        }
    override this.Invoke(func) =
        seq {
            yield text func.FunctionName.Value
            yield text "("
            match func.Arguments with
            | ArgumentWildcard -> yield text "*"
            | ArgumentList (distinct, args) ->
                match distinct with
                | Some Distinct ->
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
            yield this.SimilarityOperator(sim.Invert, sim.Operator)
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
        let context = if bin.Operator.IsLogicalOperator then Predicate else FirstClassValue
        seq {
            yield! this.Expr(bin.Left, context)
            yield ws
            yield this.BinaryOperator(bin.Operator)
            yield ws
            yield! this.Expr(bin.Right, context)
        }
    override this.Unary(un) =
        let context = if un.Operator.IsLogicalOperator then Predicate else FirstClassValue
        match un.Operator with
        | Negative
        | Not
        | BitNot ->
            seq {
                yield this.UnaryOperator(un.Operator)
                yield ws
                yield! this.Expr(un.Operand, context)
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
            yield! this.Expr(inex.Input, FirstClassValue)
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
                yield! statement.Select(select) |> indent
                yield text ")"
            | InTable tbl ->
                yield! this.Table(tbl)
            | InParameter par ->
                yield this.BindParameter(par)
        }
    override this.Case(case) =
        seq {
            yield text "CASE"
            yield tabin
            let whenContext =
                match case.Input with
                | None -> Predicate
                | Some _ -> FirstClassValue
            match case.Input with
            | None -> ()
            | Some input ->
                yield ws
                yield! this.Expr(input, FirstClassValue)
            for input, output in case.Cases do
                yield linebreak
                yield text "WHEN"
                yield ws
                yield! this.Expr(input, whenContext)
                yield ws
                yield text "THEN"
                yield ws
                yield! this.Expr(output, FirstClassValue)
            match case.Else.Value with
            | None -> ()
            | Some els ->
                yield linebreak
                yield text "ELSE"
                yield ws
                yield! this.Expr(els, FirstClassValue)
            yield tabout
            yield linebreak
            yield text "END"
        }
    override this.Exists(subquery) =
        seq {
            yield text "EXISTS("
            yield! statement.Select(subquery) |> indent
            yield text ")"
        }
    override this.ScalarSubquery(subquery) =
        seq {
            yield text "("
            yield! statement.Select(subquery) |> indent
            yield text ")"
        }
    override __.NeedsParens(expr) =
        match expr.Value with
        | LiteralExpr _
        | ColumnNameExpr _ 
        | CastExpr _
        | FunctionInvocationExpr _
        | ScalarSubqueryExpr _ -> false
        | BindParameterExpr _ when expr.Info.Type.Type <> RawSQLType -> false
        | _ -> true
    override this.Expr(expr, _) =
        let needsParens = this.NeedsParens(expr)
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
            if needsParens then yield text ")"
        }
    member this.Expr(expr) = this.Expr(expr, FirstClassValue)


