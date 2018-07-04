namespace Rezoom.SQL.Compiler.TSQL
open System
open System.Globalization
open Rezoom.SQL.Compiler
open Rezoom.SQL.Compiler.BackendUtilities
open Rezoom.SQL.Compiler.Translators
open Rezoom.SQL.Mapping

type private TSQLLiteral() =
    inherit DefaultLiteralTranslator()
    override __.BooleanLiteral(t) = CommandText <| if t then "1" else "0"
    override __.BlobLiteral(bytes) =
        let hexPairs = bytes |> Array.map (fun b -> b.ToString("X2", CultureInfo.InvariantCulture))
        "0x" + String.Concat(hexPairs) |> text
    override __.DateTimeLiteral(dt) =
        CommandText <| "'" + dt.ToString("yyyy'-'MM'-'dd'T'HH':'mm':'ss'.'fff") + "'"
    override __.DateTimeOffsetLiteral(dt) =
        CommandText <| "'" + dt.ToString("yyyy'-'MM'-'dd'T'HH':'mm':'ss'.'fffzzz") + "'"
    override __.StringLiteral(str) =
        CommandText <| "N'" + str.Replace("'", "''") + "'"

type private TSQLExpression(statement : StatementTranslator, indexer) =
    inherit DefaultExprTranslator(statement, indexer)
    let literal = TSQLLiteral()
    override __.Literal = upcast literal
    override __.Name(name) =
        "[" + name.Value.Replace("]", "]]") + "]"
        |> text
    override __.CollationName(name) = text name.Value
    override __.TypeName(name, _) =
        (Seq.singleton << text) <|
            match name with
            | BooleanTypeName -> "BIT"
            | GuidTypeName -> "UNIQUEIDENTIFIER"
            | IntegerTypeName Integer16 -> "SMALLINT"
            | IntegerTypeName Integer32 -> "INT"
            | IntegerTypeName Integer64 -> "BIGINT"
            | FloatTypeName Float32 -> "FLOAT(24)"
            | FloatTypeName Float64 -> "FLOAT(53)"
            | StringTypeName(Some len) -> "NVARCHAR(" + string len + ")"
            | StringTypeName(None) -> "NVARCHAR(max)"
            | BinaryTypeName(Some len) -> "VARBINARY(" + string len + ")"
            | BinaryTypeName(None) -> "VARBINARY(max)"
            | DecimalTypeName -> "NUMERIC(38, 19)"
            | DateTimeTypeName -> "DATETIME2"
            | DateTimeOffsetTypeName -> "DATETIMEOFFSET"
    override this.ObjectName name =
        seq {
            match name.SchemaName with
            | Some schema ->
                if schema = Name("temp") then
                    yield this.Name("#" + name.ObjectName)
                else
                    yield this.Name(schema)
                    yield text "."
                    yield this.Name(name.ObjectName) 
            | None -> yield this.Name(name.ObjectName) 
        }
    override __.BinaryOperator(op) =
        CommandText <|
        match op with
        | Concatenate -> "+"
        | Multiply -> "*"
        | Divide -> "/"
        | Modulo -> "%"
        | Add -> "+"
        | Subtract -> "-"
        | BitAnd -> "&"
        | BitOr -> "|"
        | LessThan -> "<"
        | LessThanOrEqual -> "<="
        | GreaterThan -> ">"
        | GreaterThanOrEqual -> ">="
        | Equal -> "="
        | NotEqual -> "<>"
        | And -> "AND"
        | Or -> "OR"
        | Is
        | IsNot -> bug "should have been handled for TSQL before we got here"
        | BitShiftLeft
        | BitShiftRight -> failwithf "Not supported by TSQL: %A" op
    override this.Binary(bin) =
        match bin.Operator, bin.Right.Value with
        | Is, LiteralExpr NullLiteral
        | IsNot, LiteralExpr NullLiteral ->
            seq {
                yield! this.Expr(bin.Left, FirstClassValue)
                yield ws
                yield text "IS"
                yield ws
                if bin.Operator = IsNot then
                    yield text "NOT"
                    yield ws
                yield text "NULL"
            }
        | Is, _
        | IsNot, _ ->
            seq {
                if bin.Operator = IsNot then
                    yield text "NOT"
                    yield ws
                yield text "EXISTS(SELECT"
                yield ws
                yield! this.Expr(bin.Left, FirstClassValue)
                yield ws
                yield text "INTERSECT SELECT"
                yield ws
                yield! this.Expr(bin.Right, FirstClassValue)
                yield text ")"
            }
        | _ -> base.Binary(bin)
    override __.UnaryOperator(op) =
        CommandText <|
        match op with
        | Negative -> "-"
        | Not -> "NOT"
        | BitNot -> "~"
    override __.SimilarityOperator(invert, op) =
        CommandText <|
        match op with
        | Like -> if invert then "NOT LIKE" else "LIKE"
        | Match
        | Regexp -> fail <| Error.backendDoesNotSupportFeature "TSQL" "MATCH/REGEXP operators"
    /// Identifies expressions that are set up to use as predicates in T-SQL.
    /// These expressions don't produce actual values.
    /// For example, you can't `SELECT 1=1`, but you can do `SELECT 1 WHERE 1=1`.
    /// Conversely, you can't `SELECT 1 WHERE tbl.BitColumn`, but you can do `SELECT tbl.BitColumn`.
    static member private IsPredicateBoolean(expr : TExpr) =
        expr.Info.Type.Type = BooleanType
        &&  match expr.Value with
            | SimilarityExpr _
            | BetweenExpr _
            | InExpr _
            | ExistsExpr _
            | BinaryExpr _
            | UnaryExpr _ -> true
            | _ -> false
    member private __.BaseExpr(expr, context) = base.Expr(expr, context)
    override this.Expr(expr, context) =
        match context with
        | FirstClassValue ->
            if TSQLExpression.IsPredicateBoolean(expr) then
                seq {
                    yield text "CAST((CASE WHEN"
                    yield ws
                    yield! this.BaseExpr(expr, Predicate)
                    yield ws
                    yield text "THEN 1 ELSE 0 END) AS BIT)"
                }
            else
                base.Expr(expr, context)
        | Predicate ->
            if TSQLExpression.IsPredicateBoolean(expr) then
                base.Expr(expr, context)
            else
                seq {
                    yield text "(("
                    yield! this.BaseExpr(expr, FirstClassValue)
                    yield text ")<>0)"
                }
    override this.Invoke(func) =
        match TSQLFunctions.getCustom func.FunctionName with
        | Some custom -> custom (this :> ExprTranslator) func
        | None -> base.Invoke(func)
