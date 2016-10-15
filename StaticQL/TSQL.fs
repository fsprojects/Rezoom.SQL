namespace StaticQL.TSQL
open System
open System.Data
open System.Collections.Generic
open System.Globalization
open StaticQL
open StaticQL.Mapping
open StaticQL.BackendUtilities
open StaticQL.Translators

type private TSQLLiteral() =
    inherit DefaultLiteralTranslator()
    override __.BooleanLiteral(t) = CommandText <| if t then "1" else "0"
    override __.BlobLiteral(bytes) =
        let hexPairs = bytes |> Array.map (fun b -> b.ToString("X2", CultureInfo.InvariantCulture))
        "0x" + String.Concat(hexPairs) |> text

type private TSQLExpression(statement : StatementTranslator, indexer) =
    inherit DefaultExprTranslator(statement, indexer)
    let literal = DefaultLiteralTranslator()
    override __.Literal = upcast literal
    override __.TypeName(name) =
        (Seq.singleton << text) <|
            match name with
            | BooleanTypeName -> "BIT"
            | IntegerTypeName Integer8 -> "TINYINT"
            | IntegerTypeName Integer16 -> "SMALLINT"
            | IntegerTypeName Integer32 -> "INT"
            | IntegerTypeName Integer64 -> "BIGINT"
            | FloatTypeName Float32 -> "FLOAT(24)"
            | FloatTypeName Float64 -> "FLOAT(53)"
            | StringTypeName(len) -> "NVARCHAR(" + string len + ")"
            | BinaryTypeName(len) -> "VARBINARY(" + string len + ")"
            | DecimalTypeName -> "NUMERIC(38, 19)"
            | DateTimeTypeName -> "DATETIME2"
            | DateTimeOffsetTypeName -> "DATETIMEOFFSET"
    override __.BinaryOperator op =
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
        | IsNot
        | BitShiftLeft
        | BitShiftRight -> failwithf "Not supported by TSQL: %A" op
    override __.UnaryOperator op =
        CommandText <|
        match op with
        | Negative -> "-"
        | Not -> "NOT"
        | NotNull -> "IS NOT NULL"
        | IsNull -> "IS NULL"
        | BitNot -> failwithf "Not supported by TSQL: %A" op
    override __.SimilarityOperator op =
        CommandText <|
        match op with
        | Like -> "LIKE"
        | Glob
        | Match
        | Regexp -> failwithf "Not supported by TSQL: %A" op
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

type private TSQLStatement(indexer : IParameterIndexer) as this =
    inherit DefaultStatementTranslator(indexer)
    let expr = TSQLExpression(this :> StatementTranslator, indexer)
    override __.Expr = upcast expr

type TSQLBackend() =
    static let initialModel =
        let main, temp = Name("dbo"), Name("temp")
        {   Schemas =
                [   {   SchemaName = main
                        Tables = Map.empty
                        Views = Map.empty
                    }
                    {   SchemaName = temp
                        Tables = Map.empty
                        Views = Map.empty
                    }
                ] |> List.map (fun s -> s.SchemaName, s) |> Map.ofList
            DefaultSchema = main
            TemporarySchema = temp
            Builtin =
                {   Functions = Map.empty
                }
        }
    interface IBackend with
        member this.InitialModel = initialModel
        member this.ParameterTransform(columnType) = ParameterTransform.Default(columnType)
        member this.ToCommandFragments(indexer, stmts) =
            let translator = TSQLStatement(indexer)
            translator.Statements(stmts)
            |> BackendUtilities.simplifyFragments
            |> ResizeArray
            :> _ IReadOnlyList
       