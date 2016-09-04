module private Rezoom.ORM.SQLProvider.TypeChecker
open System
open System.Collections.Generic

type Scope =
    {
        ParentScope : Scope option
        Model : IModel
        QueryVariables : IReadOnlyDictionary<string, ISchemaQuery>
    }

type Result<'x, 'err> =
    | Ok of 'x
    | Error of 'err

let inferred (columnType : ColumnType) =
    {
        Inferred = ExactlyType columnType.Type
        Nullable = Some columnType.Nullable
    }

let rec allQueryVariables (scope : Scope) =
    seq {
        yield! scope.QueryVariables
        match scope.ParentScope with
        | Some scope -> yield! allQueryVariables scope
        | None -> ()
    } |> Seq.distinctBy (fun kv -> kv.Key)

let resolveUnqualifiedColumnName (scope : Scope) (name : string) =
    let possible =
        seq {
            for KeyValue(varName, query) in allQueryVariables scope do
                let succ, col = query.ColumnsByName.TryGetValue(name)
                if succ then yield varName, col
        } |> Seq.truncate 2 |> Seq.toList
    match possible with
    | [] -> Error <| sprintf "No such column in scope: ``%s``" name
    | [ _, one ] -> Ok one
    | (v1, c1) :: (v2, c2) :: _ ->
        Error <| sprintf "Ambiguous column: ``%s`` (could be from %s.%s or %s.%s)"
            name v1 c2.ColumnName v2 c2.ColumnName

let resolveQueryNameBySchema (schema : ISchema) (name : string) =
    let succ, view = schema.Views.TryGetValue(name)
    if succ then Ok view.Query else
    let succ, table = schema.Tables.TryGetValue(name)
    if succ then Ok table.Query else
    Error <| sprintf "No such object in schema %s: ``%s``" schema.SchemaName name

let rec resolveUnqualifiedQueryName (scope : Scope) (name : string) =
    let succ, tbl = scope.QueryVariables.TryGetValue(name)
    if succ then Ok tbl else
    match scope.ParentScope with
    | None -> 
        let schema = scope.Model.Schemas.[scope.Model.DefaultSchema]
        resolveQueryNameBySchema schema name
    | Some parent -> resolveUnqualifiedQueryName parent name

let resolveQueryName (scope : Scope) (name : ObjectName) =
    match name.SchemaName with
    | None -> resolveUnqualifiedQueryName scope name.ObjectName
    | Some schemaName ->
        let succ, schema = scope.Model.Schemas.TryGetValue(schemaName)
        if not succ then Error <| sprintf "No such schema: ``%s``" schemaName else
        resolveQueryNameBySchema schema name.ObjectName

let resolveColumnName (scope : Scope) (name : ColumnName) =
    match name.Table with
    | None -> resolveUnqualifiedColumnName scope name.ColumnName
    | Some queryName ->
        let query = resolveQueryName scope queryName
        match query with
        | Error e -> Error e
        | Ok query ->
            let succ, col = query.ColumnsByName.TryGetValue(name.ColumnName)
            if succ then Ok col else
            Error <| sprintf "No such column in %O: ``%s``" queryName name.ColumnName

let literalType (literal : Literal) =
    match literal with
    | NullLiteral -> { Nullable = Some true; Inferred = AnyType }
    | CurrentTimeLiteral
    | CurrentDateLiteral
    | CurrentTimestampLiteral
    | StringLiteral _ -> { Nullable = Some false; Inferred = ExactlyType StringType }
    | BlobLiteral _ -> { Nullable = Some false; Inferred = ExactlyType BlobType }
    | NumericLiteral (IntegerLiteral _) -> { Nullable = Some false; Inferred = ExactlyType IntegerType }
    | NumericLiteral (FloatLiteral _) -> { Nullable = Some false; Inferred = ExactlyType FloatType }

/// Matches SQLite rules for determining the type affinity of a free-form type name.
let private typeAffinityRules =
    [|
        "INT", IntegerType
        "CHAR", StringType
        "CLOB", StringType
        "TEXT", StringType
        "BLOB", BlobType
        "REAL", FloatType
        "FLOA", FloatType
        "DOUB", FloatType
    |]
let typeAffinity (typeName : TypeName) =
    let names = String.concat " " typeName.TypeName
    let byRules =
        seq {
            for substr, affinity in typeAffinityRules do
                if ciContains substr names then yield affinity
        } |> Seq.tryHead
    match byRules with
    | Some affinity -> affinity
    | None -> FloatType // "numeric" affinity

type IParameterTypeInferrer =
    abstract member ConstrainParameter : BindParameter * InferredType -> unit

type Inferrer(scope : Scope, parameters : IParameterTypeInferrer) =
    member this.InferCommonExprType(exprs : Expr seq, expected : InferredType) =
        let mutable inferred = expected
        for expr in exprs do
            inferred <- this.InferExprTypeStrict(expr, inferred)
        for expr in exprs do // 2nd pass to add parameter constraints to exprs based on later restricted type
            // TODO: can we get rid of the need to do this?
            ignore <| this.InferExprTypeStrict(expr, inferred)
        inferred
    member this.InferExprTypeStrict(expr : Expr, expected : InferredType) =
        let inferred = this.InferExprType(expr, expected)
        match inferred =^= expected with
        | Some actual -> actual
        | None ->
            failAt expr.Source <|
            sprintf "Expression was expected to have type %O, but its type was inferred to be %O"
                expected inferred
    member this.InferScalarSubqueryType(subquery : SelectStmt, expected : InferredType) =
        failwith "Not supported -- need to support fancy query table-oriented stuff"
    member this.InferScalarSubqueryTypeStrict(subquery : SelectStmt, expected : InferredType) =
        let inferred = this.InferScalarSubqueryType(subquery, expected)
        match inferred =^= expected with
        | Some actual -> actual
        | None ->
            failAt subquery.Source <|
            sprintf "Subquery was expected to have scalar type %O, but its type was inferred to be %O"
                expected inferred
    member this.InferExprType(expr : Expr, expected : InferredType) =
        match expr.Value with
        | LiteralExpr lit -> literalType lit
        | BindParameterExpr par ->
            parameters.ConstrainParameter(par, expected)
            expected
        | ColumnNameExpr name ->
            let column = resolveColumnName scope name
            match column with
            | Error err -> failAt expr.Source err
            | Ok column -> inferred column.ColumnType
        | CastExpr cast ->
            let exprType = this.InferExprType(cast.Expression, InferredType.Any)
            let toType = typeAffinity cast.AsType
            {
                Nullable = exprType.Nullable
                Inferred = ExactlyType toType
            }
        | CollateExpr (subExpr, collation) ->
            ignore <| this.InferExprTypeStrict(subExpr, InferredType.String)
            InferredType.String
        | FunctionInvocationExpr funcInvoke ->
            failwith "Not supported -- need to make list of built-in SQLite functions"
        | SimilarityExpr (op, input, pattern, escape) ->
            let inputType = this.InferExprTypeStrict(input, InferredType.String)
            let patternType = this.InferExprTypeStrict(pattern, InferredType.String)
            match escape with
            | None -> ()
            | Some escape -> ignore <| this.InferExprTypeStrict(escape, InferredType.String)
            let nullable =
                match inputType =^= patternType with
                | Some unified -> unified.Nullable
                | None -> None
            { InferredType.Boolean with Nullable = nullable }
        | BinaryExpr (binop, left, right) ->
            failwith "Not supported -- need to define method to handle the whole zoo of binary operators"
        | UnaryExpr (unop, operand) ->
            failwith "Not supported -- need to define method to handle the whole zoo of unary operators"
        | BetweenExpr (input, low, high)
        | NotBetweenExpr (input, low, high) ->
            let commonType = this.InferCommonExprType([ input; low; high ], InferredType.Any)
            InferredType.Boolean
        | InExpr (input, set)
        | NotInExpr (input, set) ->
            let inputType = this.InferExprType(input, InferredType.Any)
            let setType =
                match set with
                | InExpressions exprs ->
                    ignore <| this.InferCommonExprType(exprs, inputType)
                | InSelect select ->
                    ignore <| this.InferScalarSubqueryType(select, inputType)
                | InTable table ->
                    failwith "Not supported -- need to figure out how to deal with table invocations"
            InferredType.Boolean   
        | ExistsExpr select ->
            failwith "Not supported -- need to analyze subquery for validity"
            InferredType.Boolean
        | CaseExpr cases ->
            failwith "Not supported -- need to check cases for boolean-ness"
        | ScalarSubqueryExpr subquery ->
            this.InferScalarSubqueryType(subquery, expected)
        | RaiseExpr _ -> InferredType.Any
