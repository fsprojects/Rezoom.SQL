module Rezoom.ORM.SQLProvider.BackendUtilities
open System
open System.Globalization
open System.Collections.Generic
open Rezoom.ORM
open SQLow

type Fragment = CommandFragment
type Fragments = Fragment seq

let simplifyFragments (fragments : Fragments) =
    fragments // TODO: combine consecutive CommandText fragments

let ws = Whitespace
let text str = CommandText str

let join separator (fragments : Fragments seq) =
    seq {
        let separator = CommandText separator
        let mutable first = true
        for element in fragments do
            if not first then yield separator
            else first <- false
            yield! element
    }

let join1 separator sequence = join separator (sequence |> Seq.map Seq.singleton)

[<AbstractClass>]
type LiteralTranslator() =
    abstract member NullLiteral : Fragment
    default __.NullLiteral = CommandText "NULL"
    abstract member CurrentTimeLiteral : Fragment
    default __.CurrentTimeLiteral = CommandText "CURRENT_TIME"
    abstract member CurrentDateLiteral : Fragment
    default __.CurrentDateLiteral = CommandText "CURRENT_DATE"
    abstract member CurrentTimestampLiteral : Fragment
    default __.CurrentTimestampLiteral = CommandText "CURRENT_TIMESTAMP"
    abstract member StringLiteral : str : string -> Fragment
    abstract member BlobLiteral : bytes : byte array -> Fragment
    abstract member IntegerLiteral : i : uint64 -> Fragment
    default __.IntegerLiteral i = CommandText (i.ToString(CultureInfo.InvariantCulture))
    abstract member FloatLiteral : f : float -> Fragment
    default __.FloatLiteral f = CommandText (f.ToString("0.0##############", CultureInfo.InvariantCulture))
    abstract member Literal : literal : Literal -> Fragment
    default this.Literal literal =
        match literal with
        | NullLiteral -> this.NullLiteral
        | CurrentTimeLiteral -> this.CurrentTimeLiteral
        | CurrentDateLiteral -> this.CurrentDateLiteral
        | CurrentTimestampLiteral -> this.CurrentTimestampLiteral
        | StringLiteral str -> this.StringLiteral(str)
        | BlobLiteral blob -> this.BlobLiteral(blob)
        | NumericLiteral (IntegerLiteral i) -> this.IntegerLiteral(i)
        | NumericLiteral (FloatLiteral f) -> this.FloatLiteral(f)
    abstract member SignedLiteral : literal : SignedNumericLiteral -> Fragments
    default this.SignedLiteral literal =
        let literalValue = literal.Value |> NumericLiteral |> this.Literal
        if literal.Sign >= 0 then Seq.singleton literalValue else
        seq {
            yield text "-"
            yield literalValue
        }

[<AbstractClass>]
type StatementTranslator() =
    abstract member Expr : ExprTranslator
    abstract member OrderDirection : OrderDirection -> Fragment
    default __.OrderDirection(dir) =
        match dir with
        | Ascending -> text "ASC"
        | Descending -> text "DESC"
    abstract member CTE : cte : CommonTableExpression -> Fragments
    default this.CTE(cte) =
        seq {
            yield this.Expr.Name(cte.Name)
            yield ws
            match cte.ColumnNames with
            | None -> ()
            | Some names ->
                yield text "("
                yield! names.Value |> Seq.map this.Expr.Name |> join1 ", "
                yield text ") "
            yield text "AS ("
            yield! this.Select(cte.AsSelect)
            yield text ")"
        }
    abstract member With : withClause : WithClause -> Fragments
    default this.With(withClause) =
        seq {
            yield text "WITH"
            yield ws
            if withClause.Recursive then
                yield text "RECURSIVE"
                yield ws
            yield! withClause.Tables |> Seq.map this.CTE |> join ","
        }
    abstract member Values : vals : Expr ResizeArray WithSource ResizeArray -> Fragments
    default this.Values(vals) =
        vals |> Seq.map (fun row ->
            seq {
                yield text "("
                yield! row.Value |> Seq.map this.Expr.Expr |> join ","
                yield text ")"
            }) |> join ","
    abstract member ResultColumn : ResultColumn -> Fragments
    default this.ResultColumn(col) =
        match col with
        | ColumnsWildcard -> text "*" |> Seq.singleton
        | TableColumnsWildcard name ->
            seq {
                yield! this.Expr.ObjectName(name)
                yield text ".*"
            }
        | Column (expr, alias) ->
            seq {
                yield! this.Expr.Expr(expr)
                match alias with
                | None -> ()
                | Some alias ->
                    yield ws
                    yield text "AS"
                    yield ws
                    yield this.Expr.Name(alias)
            }
    abstract member ResultColumns : ResultColumns -> Fragments
    default this.ResultColumns(cols) =
        seq {
            match cols.Distinct with
            | None
            | Some AllColumns -> ()
            | Some DistinctColumns -> yield text "DISTINCT"; yield ws
            yield! cols.Columns |> Seq.map (fun c -> this.ResultColumn(c.Value)) |> join ","
        }
    abstract member TableOrSubquery : TableOrSubquery -> Fragments
    default this.TableOrSubquery(tbl) =
        seq {
            match tbl with
            | Table (tbl, alias, indexHint) ->
                yield! this.Expr.Table(tbl)
                match alias with
                | None -> ()
                | Some alias ->
                    yield ws
                    yield text "AS"
                    yield ws
                    yield this.Expr.Name(alias)
                match indexHint with
                | None -> ()
                | Some NotIndexed ->
                    yield ws
                    yield text "NOT INDEXED"
                | Some (IndexedBy name) ->
                    yield ws
                    yield text "INDEXED BY"
                    yield ws
                    yield this.Expr.Name(name)
            | Subquery (select, alias) ->
                yield text "("
                yield! this.Select(select)
                yield text ")"
                match alias with
                | None -> ()
                | Some alias ->
                    yield ws
                    yield text "AS"
                    yield ws
                    yield this.Expr.Name(alias)
        }
    abstract member TableExpr : TableExpr -> Fragments
    default this.TableExpr(texpr) =
        match texpr.Value with
        | TableOrSubquery tbl -> this.TableOrSubquery(tbl)
        | Join join -> this.Join(join)
    abstract member JoinType : JoinType -> Fragment
    default __.JoinType(join) =
        let rec joinText join =
            match join with
            | Inner -> "INNER JOIN"
            | LeftOuter -> "LEFT OUTER JOIN"
            | Cross -> "CROSS JOIN"
            | Natural ty -> "NATURAL " + joinText ty
        joinText join |> text
    abstract member Join : Join -> Fragments
    default this.Join(join) =
        seq {
            yield! this.TableExpr(join.LeftTable)
            yield ws
            yield this.JoinType(join.JoinType)
            yield ws
            yield! this.TableExpr(join.RightTable)
            match join.Constraint with
            | JoinOn expr ->
                yield ws
                yield text "ON"
                yield ws
                yield! this.Expr.Expr(expr)
            | JoinUsing names ->
                yield ws 
                yield text "USING"
                yield ws
                yield text "("
                yield! names |> Seq.map this.Expr.Name |> join1 ","
                yield text ")"
            | JoinUnconstrained -> ()
        }
    abstract member SelectCore : select : SelectCore -> Fragments
    default this.SelectCore(select) =
        seq {
            yield text "SELECT"
            yield ws
            yield! this.ResultColumns(select.Columns)
            match select.From with
            | None -> ()
            | Some from ->
                yield ws
                yield text "FROM"
                yield ws
                yield! this.TableExpr(from)
            match select.Where with
            | None -> ()
            | Some where ->
                yield ws
                yield text "WHERE"
                yield ws
                yield! this.Expr.Expr(where)
            match select.GroupBy with
            | None -> ()
            | Some groupBy ->
                yield ws
                yield text "GROUP BY"
                yield ws
                yield! groupBy.By |> Seq.map this.Expr.Expr |> join ","
                match groupBy.Having with
                | None -> ()
                | Some having ->
                    yield ws
                    yield text "HAVING"
                    yield ws
                    yield! this.Expr.Expr(having)
        }
    abstract member CompoundTerm : compound : CompoundTermCore -> Fragments
    default this.CompoundTerm(compound) =
        match compound with
        | Values vals -> this.Values(vals)
        | Select select -> this.SelectCore(select)
    abstract member Compound : compound : CompoundExprCore -> Fragments
    default this.Compound(compound) =
        let op name (expr : CompoundExpr) (term : CompoundTerm) =
            seq {
                yield! this.Compound(expr.Value)
                yield ws
                yield text name
                yield ws
                yield! this.CompoundTerm(term.Value)
            }
        match compound with
        | CompoundTerm term -> this.CompoundTerm(term.Value)
        | Union (expr, term) -> op "UNION" expr term
        | UnionAll (expr, term) -> op "UNION ALL" expr term
        | Intersect (expr, term) -> op "INTERSECT" expr term
        | Except (expr, term) -> op "EXCEPT" expr term
    abstract member Limit : Limit -> Fragments
    default this.Limit(limit) =
        seq {
            yield text "LIMIT"
            yield ws
            yield! this.Expr.Expr(limit.Limit)
            match limit.Offset with
            | None -> ()
            | Some offset ->
                yield ws
                yield text "OFFSET"
                yield ws
                yield! this.Expr.Expr(offset)
        }
    abstract member OrderingTerm : OrderingTerm -> Fragments
    default this.OrderingTerm(term) =
        seq {
            yield! this.Expr.Expr(term.By)
            yield ws
            yield this.OrderDirection(term.Direction)
        }
    abstract member Select : select : SelectStmt -> Fragments
    default this.Select(select) =
        let select = select.Value
        seq {
            match select.With with
            | None -> ()
            | Some withClause ->
                yield! this.With(withClause)
                yield ws
            yield! this.Compound(select.Compound.Value)
            match select.OrderBy with
            | None -> ()
            | Some orderBy -> yield! orderBy |> Seq.map this.OrderingTerm |> join ","
            match select.Limit with
            | None -> ()
            | Some limit -> yield! this.Limit(limit)
        }
    abstract member Statement : Stmt -> Fragments
    default this.Statement(stmt) =
        match stmt with
        | SelectStmt select -> this.Select(select)
        | _ -> failwith "Not implemented"
    abstract member Statements : Stmt seq -> Fragments
    default this.Statements(stmts) =
        stmts |> Seq.map this.Statement |> join ";"

and [<AbstractClass>] ExprTranslator(statement : StatementTranslator, indexer : IParameterIndexer) =
    abstract member Literal : LiteralTranslator
    abstract member Name : name : Name -> Fragment
    abstract member BinaryOperator : op : BinaryOperator -> Fragment
    default __.BinaryOperator op =
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
    abstract member UnaryOperator : op : UnaryOperator -> Fragment
    default __.UnaryOperator op =
        CommandText <|
        match op with
        | Negative -> "-"
        | Not -> "NOT"
        | BitNot -> "~"
        | NotNull -> "NOT NULL"
        | IsNull -> "IS NULL"
    abstract member SimilarityOperator : op : SimilarityOperator -> Fragment
    default __.SimilarityOperator op =
        CommandText <|
        match op with
        | Like -> "LIKE"
        | Glob -> "GLOB"
        | Match -> "MATCH"
        | Regexp -> "REGEXP"
    abstract member BindParameter : par : BindParameter -> Fragment
    default __.BindParameter par = indexer.ParameterIndex(par) |> Parameter
    abstract member ObjectName : name : ObjectName -> Fragments
    default this.ObjectName name =
        seq {
            match name.SchemaName with
            | Some schema ->
                yield text (schema.Value + ".")
            | None -> ()
            yield this.Name(name.ObjectName) 
        }
    abstract member ColumnName : column : ColumnName -> Fragments
    default this.ColumnName col =
        seq {
            match col.Table with
            | Some tbl ->
                yield! this.ObjectName(tbl)
                yield text "."
            | None -> ()
            yield this.Name(col.ColumnName)
        }
    abstract member TypeName : TypeName -> Fragments
    default this.TypeName(name) =
        seq {
            yield! name.TypeName |> Seq.map this.Name |> join1 " "
            match name.Bounds with
            | None -> ()
            | Some bounds ->
                yield text "("
                yield! this.Literal.SignedLiteral(bounds.Low)
                match bounds.High with
                | None -> ()
                | Some high ->
                    yield text ","
                    yield! this.Literal.SignedLiteral(high)
                yield text ")"
        }
    abstract member Cast : castExpr : CastExpr -> Fragments
    default this.Cast(castExpr) =
        seq {
            yield text "CAST("
            yield! this.Expr(castExpr.Expression)
            yield ws
            yield text "AS"
            yield ws
            yield! this.TypeName(castExpr.AsType)
            yield text ")"
        }
    abstract member Collate : expr : Expr * collation : Name -> Fragments
    default this.Collate(expr, collation) =
        seq {
            yield! this.Expr(expr)
            yield ws
            yield text "COLLATE"
            yield ws
            yield this.Name(collation)
        }
    abstract member Invoke : func : FunctionInvocationExpr -> Fragments
    default this.Invoke(func) =
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
    abstract member Similarity
        : invert : bool
        * op : SimilarityOperator
        * input : Expr
        * pattern : Expr
        * escape : Expr option
        -> Fragments
    default this.Similarity(invert, op, input, pattern, escape) =
        seq {
            yield! this.Expr(input)
            yield ws
            if invert then
                yield text "NOT"
                yield ws
            yield this.SimilarityOperator(op)
            yield ws
            yield! this.Expr(pattern)
            match escape with
            | None -> ()
            | Some escape ->
                yield ws
                yield text "ESCAPE"
                yield ws
                yield! this.Expr(escape)
        }
    abstract member Binary : op : BinaryOperator * left : Expr * right : Expr -> Fragments
    default this.Binary(op, left, right) =
        seq {
            yield! this.Expr(left)
            yield ws
            yield this.BinaryOperator(op)
            yield ws
            yield! this.Expr(right)
        }
    abstract member Unary : op : UnaryOperator * expr : Expr -> Fragments
    default this.Unary(op, expr) =
        match op with
        | Negative
        | Not
        | BitNot ->
            seq {
                yield this.UnaryOperator(op)
                yield ws
                yield! this.Expr(expr)
            }
        | NotNull
        | IsNull ->
            seq {
                yield! this.Expr(expr)
                yield ws
                yield this.UnaryOperator(op)
            }
    abstract member Between
        : invert : bool
        * input : Expr
        * low : Expr
        * high : Expr
        -> Fragments
    default this.Between(invert, input, low, high) =
        seq {
            yield! this.Expr(input)
            yield ws
            if invert then
                yield text "NOT"
                yield ws
            yield text "BETWEEN"
            yield ws
            yield! this.Expr(low)
            yield ws
            yield text "AND"
            yield ws
            yield! this.Expr(high)
        }
    abstract member Table : TableInvocation -> Fragments
    default this.Table(tbl) =
        seq {
            yield! this.ObjectName(tbl.Table)
            match tbl.Arguments with
            | None -> ()
            | Some args ->
                yield text "("
                yield! args |> Seq.map this.Expr |> join ","
                yield text ")"
        }
    abstract member In
        : invert : bool
        * input : Expr
        * set : InSet
        -> Fragments
    default this.In(invert, input, set) =
        seq {
            yield! this.Expr(input)
            yield ws
            if invert then
                yield text "NOT"
                yield ws
            yield text "IN"
            yield ws
            match set with
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
    abstract member Case : case : CaseExpr -> Fragments
    default this.Case(case) =
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
    abstract member Raise : raise : Raise -> Fragments
    default this.Raise(raise) =
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
    abstract member Exists : subquery : SelectStmt -> Fragments
    default this.Exists(subquery) =
        seq {
            yield text "EXISTS("
            yield! statement.Select(subquery)
            yield text ")"
        }
    abstract member ScalarSubquery : subquery : SelectStmt -> Fragments
    default this.ScalarSubquery(subquery) =
        seq {
            yield text "("
            yield! statement.Select(subquery)
            yield text ")"
        }
    abstract member NeedsParens : ExprType -> bool
    default __.NeedsParens(expr) =
        match expr with
        | LiteralExpr _
        | BindParameterExpr _
        | ColumnNameExpr _ 
        | CastExpr _
        | FunctionInvocationExpr _
        | ScalarSubqueryExpr _ -> false
        | _ -> true
    abstract member Expr : expr : Expr -> Fragments
    default this.Expr(expr) =
        let needsParens = this.NeedsParens(expr.Value)
        seq {
            if needsParens then yield text "("
            yield!
                match expr.Value with
                | LiteralExpr lit -> this.Literal.Literal(lit) |> Seq.singleton
                | BindParameterExpr bind -> this.BindParameter(bind) |> Seq.singleton
                | ColumnNameExpr name -> this.ColumnName(name)
                | CastExpr cast -> this.Cast(cast)
                | CollateExpr (expr, input) -> this.Collate(expr, input)
                | FunctionInvocationExpr func -> this.Invoke(func)
                | SimilarityExpr (op, input, pattern, escape) -> this.Similarity(false, op, input, pattern, escape)
                | NotSimilarityExpr (op, input, pattern, escape) -> this.Similarity(true, op, input, pattern, escape)
                | BinaryExpr (op, left, right) -> this.Binary(op, left, right)
                | UnaryExpr (op, expr) -> this.Unary(op, expr)
                | BetweenExpr (input, low, high) -> this.Between(false, input, low, high)
                | NotBetweenExpr (input, low, high) -> this.Between(true, input, low, high)
                | InExpr (expr, inset) -> this.In(false, expr, inset.Value)
                | NotInExpr (expr, inset) -> this.In(true, expr, inset.Value)
                | ExistsExpr select -> this.Exists(select)
                | CaseExpr case -> this.Case(case)
                | ScalarSubqueryExpr subquery -> this.ScalarSubquery(subquery)
                | RaiseExpr raise -> this.Raise(raise)
            if needsParens then yield text ")"
        }
