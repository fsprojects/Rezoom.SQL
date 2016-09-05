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

let joinQueryResults (left : ISchemaQuery) (right : ISchemaQuery) =
    let mutable newQuery = Unchecked.defaultof<ISchemaQuery>
    let reparentColumn (column : ISchemaQueryColumn) =
        { new ISchemaQueryColumn with
            member __.Query = newQuery
            member __.ColumnName = column.ColumnName
            member __.ColumnType = column.ColumnType
            member __.SourceColumn = column.SourceColumn
        }
    let columns =
        lazy (seq {
            for col in left.Columns -> reparentColumn col
            for col in right.Columns -> reparentColumn col
        } |> toReadOnlyList)
    let dictionary =
        lazy (columns.Value |> ciDictBy (fun c -> c.ColumnName))
    let referenced =
        lazy (
            Seq.append left.ReferencedTables.Values right.ReferencedTables.Values
            |> ciDictBy (fun t -> t.TableName)
        )
    newQuery <-
        { new ISchemaQuery with
            member __.Columns = columns.Value
            member __.ColumnsByName = dictionary.Value
            member __.ReferencedTables = referenced.Value
        }
    newQuery

let renameColumns (source : SourceInfo) (query : ISchemaQuery) (names : Name IReadOnlyList) =
    if names.Count <> query.Columns.Count then
        failAt source <|
            sprintf "Names were given for %d columns, but the query has %d columns" names.Count query.Columns.Count
    else
        let mutable newQuery = Unchecked.defaultof<ISchemaQuery>
        let renameColumn name (column : ISchemaQueryColumn) =
            { new ISchemaQueryColumn with
                member __.Query = newQuery
                member __.ColumnName = name
                member __.ColumnType = column.ColumnType
                member __.SourceColumn = column.SourceColumn
            }
        let columns =
            (names, query.Columns) ||> Seq.map2 renameColumn |> toReadOnlyList
        let dictionary =
            lazy (columns |> ciDictBy (fun c -> c.ColumnName))
        newQuery <-
            { new ISchemaQuery with
                member __.Columns = columns
                member __.ColumnsByName = dictionary.Value
                member __.ReferencedTables = query.ReferencedTables
            }
        newQuery

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

type IStatementInfoBuilder =
    abstract member ConstrainParameter : BindParameter * InferredType -> unit
    abstract member ReadTable : ISchemaTable -> unit
    abstract member WriteTable : ISchemaTable -> unit

type Inferrer(scope : Scope, stmtInfo : IStatementInfoBuilder) =
    member this.ResolveTableInvocation(source : SourceInfo, tableInvocation : TableInvocation) =
        match tableInvocation.Arguments with
        | None ->
            match resolveQueryName scope tableInvocation.Table with
            | Error err -> failAt source err
            | Ok query -> query
        | Some args ->
            failAt source "Table invocations with arguments are not supported"

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

    member this.InferSimilarityExprType(op, input, pattern, escape) =
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

    member this.InferInExprType(input, set) =
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

    member this.InferExprType(expr : Expr, expected : InferredType) =
        match expr.Value with
        | LiteralExpr lit -> literalType lit
        | BindParameterExpr par ->
            stmtInfo.ConstrainParameter(par, expected)
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
        | SimilarityExpr (op, input, pattern, escape) -> this.InferSimilarityExprType(op, input, pattern, escape)
        | BinaryExpr (binop, left, right) ->
            failwith "Not supported -- need to define method to handle the whole zoo of binary operators"
        | UnaryExpr (unop, operand) ->
            failwith "Not supported -- need to define method to handle the whole zoo of unary operators"
        | BetweenExpr (input, low, high)
        | NotBetweenExpr (input, low, high) ->
            let commonType = this.InferCommonExprType([ input; low; high ], InferredType.Any)
            InferredType.Boolean
        | InExpr (input, set)
        | NotInExpr (input, set) -> this.InferInExprType(input, set)
        | ExistsExpr select ->
            failwith "Not supported -- need to analyze subquery for validity"
            InferredType.Boolean
        | CaseExpr cases ->
            failwith "Not supported -- need to check cases for boolean-ness"
        | ScalarSubqueryExpr subquery ->
            this.InferScalarSubqueryType(subquery, expected)
        | RaiseExpr _ -> InferredType.Any

    member this.CteScope(withClause) =
        match withClause with
        | None -> scope
        | Some ctes ->
            ctes.Tables |> Seq.fold (fun scope cte ->
                let cteQuery =
                    match cte.ColumnNames with
                    | None -> this.InferQueryType(cte.AsSelect)
                    | Some names -> renameColumns names.Source (this.InferQueryType(cte.AsSelect)) names.Value
                { scope with
                    ParentScope = Some scope
                    QueryVariables = ciSingle cte.Name cteQuery
                }) scope

    member this.TableExprScope(dict : Dictionary<string, _>, tableExpr : TableExpr) =
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
        | Join (_, left, right, _) ->
            let left = this.TableExprScope(dict, left)
            let right = this.TableExprScope(dict, right)
            joinQueryResults left right

    member this.TableExprScope(tableExpr : TableExpr) =
        let dict = Dictionary(StringComparer.OrdinalIgnoreCase)
        let wildcardQuery = this.TableExprScope(dict, tableExpr)
        { scope with
            ParentScope = Some scope
            QueryVariables = dict
        }, Some wildcardQuery

    member this.InferSelectCoreType(select : SelectCore) =
        let fromScope, fromWildcard =
            match select.From with
            | None -> { scope with QueryVariables = emptyDictionary }, None
            | Some tableExpr ->
                this.TableExprScope(tableExpr)
        let from = Inferrer(fromScope, stmtInfo)
        let resultColumns =
            seq {
              for col in select.Columns.Columns do
                match col with
                | ColumnsWildcard ->
                    match fromWildcard with
                    | None -> failAt (failwith "Where?") "Can't use wildcard without a `from` clause"
                    | Some q -> yield! q.Columns
                | TableColumnsWildcard objectName ->
                    match resolveQueryName fromScope objectName with
                    | Error err -> failAt (failwith "Where?") err
                    | Ok q -> yield! q.Columns
                | Column (expr, alias) ->
                    let inferred = from.InferExprType(expr, InferredType.Any)
                    yield
                        { new ISchemaQueryColumn with
                            member __.Query = Unchecked.defaultof<ISchemaQuery>
                            member __.ColumnName =
                                match alias with
                                | None -> failAt expr.Source "An expression-valued column should have an alias"
                                | Some name -> name
                            member __.ColumnType =
                                failwith "Figure out how to translate inferred to real types!"
                            member __.SourceColumn = None
                        }
            }
        failwith "" : ISchemaQuery
    member this.InferQueryType(select : SelectStmt) =
        let scope = this.CteScope(select.Value.With)
        failwith "" : ISchemaQuery
