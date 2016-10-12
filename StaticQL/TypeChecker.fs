namespace StaticQL
open System
open System.Collections.Generic
open StaticQL.InferredTypes

module TypeInferenceExtensions =
    type ITypeInferenceContext with
        member typeInference.Unify(inferredType, coreType : CoreColumnType) =
            typeInference.Unify(inferredType, InferredType.Dependent(inferredType, coreType))
        member typeInference.Unify(inferredType, resultType : Result<InferredType, string>) =
            match resultType with
            | Ok t -> typeInference.Unify(inferredType, t)
            | Error _ as e -> e
        member typeInference.Unify(types : InferredType seq) =
            types
            |> Seq.fold
                (function | Ok s -> (fun t -> typeInference.Unify(s, t)) | Error _ as e -> (fun _ -> e))
                (Ok InferredType.Any)
        member typeInference.Concrete(inferred) = typeInference.Concrete(inferred)
        member typeInference.Binary(op, left, right) =
            match op with
            | Concatenate -> typeInference.Unify([ left; right; InferredType.String ])
            | Multiply
            | Divide
            | Add
            | Subtract -> typeInference.Unify([ left; right; InferredType.Number ])
            | Modulo
            | BitShiftLeft
            | BitShiftRight
            | BitAnd
            | BitOr -> typeInference.Unify([ left; right; InferredType.Integer ])
            | LessThan
            | LessThanOrEqual
            | GreaterThan
            | GreaterThanOrEqual
            | Equal
            | NotEqual
            | Is
            | IsNot ->
                result {
                    let! operandType = typeInference.Unify(left, right)
                    return InferredType.Dependent(operandType, BooleanType)
                }
            | And
            | Or -> typeInference.Unify([ left; right; InferredType.Boolean ])
        member typeInference.Unary(op, operandType) =
            match op with
            | Negative
            | BitNot -> typeInference.Unify(operandType, InferredType.Number)
            | Not -> typeInference.Unify(operandType, InferredType.Boolean)
            | IsNull
            | NotNull -> result { return InferredType.Boolean }
open TypeInferenceExtensions

type TypeChecker(cxt : ITypeInferenceContext, scope : InferredSelectScope) =
    member this.Scope = scope
    member this.WithScope(scope) = TypeChecker(cxt, scope)
    member this.ObjectName(objectName : ObjectName) = this.ObjectName(objectName, false)
    member this.ObjectName(objectName : ObjectName, allowNotFound) : InfObjectName =
        {   SchemaName = objectName.SchemaName
            ObjectName = objectName.ObjectName
            Source = objectName.Source
            Info =
                match scope.ResolveObjectReference(objectName) with
                | Ambiguous r -> failAt objectName.Source r
                | Found f -> f
                | NotFound r ->
                    if not allowNotFound then failAt objectName.Source r
                    else Missing
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
                {   Type = cxt.Binary(binary.Operator, left.Info.Type, right.Info.Type) |> resultAt source
                    Aggregate = left.Info.Aggregate || right.Info.Aggregate
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
                {   Type = cxt.Unary(unary.Operator, operand.Info.Type) |> resultAt source
                    Aggregate = operand.Info.Aggregate
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
                    Aggregate = input.Info.Aggregate
                    Function = None
                    Column = None
                }
        }
    member this.Collation(source : SourceInfo, collation : CollationExpr) =
        let input = this.Expr(collation.Input)
        cxt.Unify(input.Info.Type, InferredType.String) |> resultOk source
        {   Expr.Source = source
            Value = 
                {   Input = this.Expr(collation.Input)
                    Collation = collation.Collation
                } |> CollateExpr
            Info =
                {   Type = input.Info.Type
                    Aggregate = input.Info.Aggregate
                    Function = None
                    Column = None
                }
        }
    member this.FunctionInvocation(source : SourceInfo, func : FunctionInvocationExpr) =
        match scope.Model.Builtin.Functions.TryFind(func.FunctionName) with
        | None -> failAt source <| sprintf "No such function: ``%O``" func.FunctionName
        | Some funcType ->
            let functionVars = Dictionary()
            let toInferred (ty : ArgumentType) =
                match ty with
                | ArgumentConcrete t -> ConcreteType t
                | ArgumentTypeVariable name ->
                    let succ, tvar = functionVars.TryGetValue(name)
                    if succ then tvar else
                    let avar = cxt.AnonymousVariable()
                    functionVars.[name] <- avar
                    avar
            let mutable argsAggregate = false
            let args, output =
                match func.Arguments with
                | ArgumentWildcard ->
                    if funcType.AllowWildcard then ArgumentWildcard, toInferred funcType.Output
                    else failAt source <| sprintf "Function does not permit wildcards: ``%O``" func.FunctionName
                | ArgumentList (distinct, args) ->
                    if Option.isSome distinct && not funcType.AllowDistinct then
                        failAt source <| sprintf "Function does not permit DISTINCT keyword: ``%O``" func.FunctionName
                    else
                        let outArgs = ResizeArray()
                        let add expr =
                            let arg = this.Expr(expr)
                            outArgs.Add(arg)
                            argsAggregate <- argsAggregate || arg.Info.Aggregate
                            arg.Info.Type
                        let mutable lastIndex = 0
                        for i, expectedTy in funcType.FixedArguments |> Seq.indexed do
                            if i >= args.Count then
                                failAt source <|
                                    sprintf "Function %O expects at least %d arguments but given only %d"
                                        func.FunctionName
                                        funcType.FixedArguments.Count
                                        args.Count
                            else
                                cxt.Unify(toInferred expectedTy, add args.[i]) |> resultOk args.[i].Source
                            lastIndex <- i
                        for i = lastIndex + 1 to args.Count - 1 do
                            match funcType.VariableArgument with
                            | None ->
                                failAt args.[i].Source <|
                                    sprintf "Function %O does not accept more than %d arguments"
                                        func.FunctionName
                                        funcType.FixedArguments.Count
                            | Some varArg ->
                                cxt.Unify(toInferred varArg, add args.[i]) |> resultOk args.[i].Source
                        ArgumentList (distinct, outArgs), toInferred funcType.Output
            {   Expr.Source = source
                Value = { FunctionName = func.FunctionName; Arguments = args } |> FunctionInvocationExpr
                Info =
                    {   Type = output
                        Aggregate = argsAggregate || funcType.Aggregate
                        Function = Some funcType
                        Column = None
                    }
            }
    member this.Similarity(source : SourceInfo, sim : SimilarityExpr) =
        let input = this.Expr(sim.Input)
        let pattern = this.Expr(sim.Pattern)
        let escape = Option.map this.Expr sim.Escape
        let output =
            result {
                let! inputType = cxt.Unify(input.Info.Type, StringType)
                let! patternType = cxt.Unify(pattern.Info.Type, StringType)
                match escape with
                | None -> ()
                | Some escape -> ignore <| cxt.Unify(escape.Info.Type, StringType)
                let! unified = cxt.Unify(inputType, patternType)
                return InferredType.Dependent(unified, BooleanType)
            } |> resultAt source
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
                    Aggregate = input.Info.Aggregate || pattern.Info.Aggregate
                    Function = None
                    Column = None
                }
        }
    member this.Between(source : SourceInfo, between : BetweenExpr) =
        let input = this.Expr(between.Input)
        let low = this.Expr(between.Low)
        let high = this.Expr(between.High)
        {   Expr.Source = source
            Value = { Invert = between.Invert; Input = input; Low = low; High = high } |> BetweenExpr
            Info =
                {   Type = cxt.Unify([ input.Info.Type; low.Info.Type; high.Info.Type ]) |> resultAt source
                    Aggregate = input.Info.Aggregate || low.Info.Aggregate || high.Info.Aggregate
                    Function = None
                    Column = None
                }
        }
    member this.In(source : SourceInfo, inex : InExpr) =
        let input = this.Expr(inex.Input)
        let set =
            match inex.Set.Value with
            | InExpressions exprs ->
                let exprs = exprs |> rmap this.Expr
                seq {
                    yield input
                    yield! exprs
                } |> Seq.map (fun e -> e.Info.Type) |> cxt.Unify |> resultOk inex.Set.Source
                exprs |> InExpressions
            | InSelect select -> InSelect <| this.Select(select)
            | InTable table -> InTable <| this.TableInvocation(table)
        {   Expr.Source = source
            Value =
                {   Invert = inex.Invert
                    Input = this.Expr(inex.Input)
                    Set = { Source = inex.Set.Source; Value = set }
                } |> InExpr
            Info =
                {   Type = InferredType.Dependent(input.Info.Type, BooleanType)
                    Aggregate = input.Info.Aggregate
                    Function = None
                    Column = None
                }
        }
    member this.Case(source : SourceInfo, case : CaseExpr) =
        let case =
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
        let outputType =
            seq {
                for _, thenExpr in case.Cases -> thenExpr.Info.Type
                match case.Else.Value with
                | None -> ()
                | Some els -> yield els.Info.Type
            } |> cxt.Unify |> resultAt source
        seq {
            yield
                match case.Input with
                | None -> InferredType.Boolean
                | Some input -> input.Info.Type
            for whenExpr, _ in case.Cases -> whenExpr.Info.Type
        } |> cxt.Unify |> resultOk source
        {   Expr.Source = source
            Value = case |> CaseExpr
            Info =
                {   Type = outputType
                    Aggregate =
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
                        } |> Seq.exists (fun e -> e.Info.Aggregate)
                    Function = None
                    Column = None
                }
        }
    member this.Exists(source : SourceInfo, exists : SelectStmt) =
        {   Expr.Source = source
            Value = this.Select(exists) |> ExistsExpr
            Info = ExprInfo<_>.OfType(InferredType.Boolean)
        }
    member this.ScalarSubquery(source : SourceInfo, select : SelectStmt) =
        let select = this.Select(select)
        let tbl = select.Value.Info.Table.Query
        if tbl.Columns.Count <> 1 then
            failAt source <| sprintf "Scalar subquery must have 1 column (this one has %d)" tbl.Columns.Count
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
        | RaiseExpr raise -> { Source = source; Value = RaiseExpr raise; Info = ExprInfo<_>.OfType(InferredType.Any) }
    member this.Expr(expr : Expr, ty : CoreColumnType) =
        let expr = this.Expr(expr)
        cxt.Unify(expr.Info.Type, ty) |> resultOk expr.Source
        expr

    member private this.TableOrSubqueryScope(tsub : TableOrSubquery) =
        match tsub.Table with
        | Table (tinvoc, index) ->
            tsub.Alias |? tinvoc.Table.ObjectName, this.ObjectName(tinvoc.Table).Info
        | Subquery select ->
            match tsub.Alias with
            | None -> failAt select.Source "Subquery requires an alias"
            | Some alias ->
                alias, this.Select(select).Value.Info

    member private this.TableExprScope(dict : Dictionary<Name, InferredType ObjectInfo>, texpr : TableExpr) =
        let add name objectInfo =
            if dict.ContainsKey(name) then
                failAt texpr.Source <| sprintf "Table name already in scope: ``%O``" name
            else
                dict.Add(name, objectInfo)
        match texpr.Value with
        | TableOrSubquery tsub ->
            let (alias, objectInfo) as pair = this.TableOrSubqueryScope(tsub)
            add alias objectInfo
            Seq.singleton pair
        | Join join ->
            seq {
                yield! this.TableExprScope(dict, join.LeftTable)
                yield! this.TableExprScope(dict, join.RightTable)
            }
    member private this.TableExprScope(texpr : TableExpr) =
        let dict = Dictionary()
        { FromVariables = dict; FromObjects = this.TableExprScope(dict, texpr) |> ResizeArray }

    member this.TableInvocation(table : TableInvocation) =
        {   Table = this.ObjectName(table.Table)
            Arguments = table.Arguments |> Option.map (rmap this.Expr)
        }

    member private this.TableOrSubquery(tsub : TableOrSubquery) =
        let tbl, info =
            match tsub.Table with
            | Table (tinvoc, index) ->
                let invoke = this.TableInvocation(tinvoc)
                Table (invoke, index), invoke.Table.Info
            | Subquery select ->
                let select = this.Select(select)
                Subquery select, select.Value.Info
        {   Table = tbl
            Alias = tsub.Alias
            Info = info
        }

    member private this.TableExpr(constraintChecker : TypeChecker, texpr : TableExpr) =
        {   TableExpr.Source = texpr.Source
            Value =
                match texpr.Value with
                | TableOrSubquery tsub -> TableOrSubquery <| this.TableOrSubquery(tsub)
                | Join join ->
                    {   JoinType = join.JoinType
                        LeftTable = this.TableExpr(constraintChecker, join.LeftTable)
                        RightTable = this.TableExpr(constraintChecker, join.RightTable)
                        Constraint =
                            match join.Constraint with
                            | JoinOn e -> constraintChecker.Expr(e, BooleanType) |> JoinOn
                            | JoinUsing names -> JoinUsing names
                            | JoinUnconstrained -> JoinUnconstrained
                    } |> Join
        }

    member this.TableExpr(texpr : TableExpr) =
        let checker = TypeChecker(cxt, { scope with FromClause = Some <| this.TableExprScope(texpr) })
        checker, this.TableExpr(checker, texpr)

    member this.ResultColumn(resultColumn : ResultColumn WithSource) =
        let qualify (alias : Name) fromTable (col : _ ColumnExprInfo) =
            Column
                ({  Source = resultColumn.Source
                    Value =
                        {   ColumnName = col.ColumnName
                            Table =
                                {   Source = resultColumn.Source
                                    ObjectName = alias
                                    SchemaName = None
                                    Info = fromTable
                                } |> Some
                        } |> ColumnNameExpr
                    Info = col.Expr.Info }, None)
        match resultColumn.Value with
        | ColumnsWildcard ->
            match scope.FromClause with
            | None -> failAt resultColumn.Source "Must have a FROM clause to use * wildcard"
            | Some from ->
                seq {
                    for alias, fromTable in from.FromObjects do
                    for col in fromTable.Table.Query.Columns do
                        yield qualify alias fromTable col
                }
        | TableColumnsWildcard tbl ->
            match scope.FromClause with
            | None -> failAt resultColumn.Source <| sprintf "Must have a FROM clause to use ``%O``" tbl
            | Some from ->
                let succ, fromTable = from.FromVariables.TryGetValue(tbl)
                if not succ then failAt resultColumn.Source <| sprintf "No such table: ``%O``" tbl
                seq {
                    for col in fromTable.Table.Query.Columns do
                        yield qualify tbl fromTable col
                }
        | Column (expr, alias) -> Column (this.Expr(expr), alias) |> Seq.singleton
    member this.ResultColumns(resultColumns : ResultColumns) =
        {   Distinct = resultColumns.Distinct
            Columns =
                resultColumns.Columns
                |> Seq.collect
                    (fun rc -> this.ResultColumn(rc) |> Seq.map (fun c -> { WithSource.Source = rc.Source; Value = c }))
                |> ResizeArray
        }
    member this.GroupBy(groupBy : GroupBy) =
        {   By = groupBy.By |> rmap this.Expr
            Having = groupBy.Having |> Option.map this.Expr
        }
    member this.SelectCore(select : SelectCore) =
        let checker, from =
            match select.From with
            | None -> this, None
            | Some from ->
                let checker, texpr = this.TableExpr(from)
                checker, Some texpr
        let columns = checker.ResultColumns(select.Columns)
        let infoColumns =
            seq {
                for column in columns.Columns do
                    match column.Value with
                    | Column (expr, alias) ->
                        yield
                            {   Expr = expr
                                FromAlias = None
                                ColumnName =
                                    match alias with
                                    | Some alias -> alias
                                    | None ->
                                        match expr.Info.Column with
                                        | None -> failAt column.Source "Expression-valued column requires an alias"
                                        | Some col -> col.ColumnName
                            }
                     | _ -> failwith "All columns must be qualified" // bug if we get here
            } |> toReadOnlyList
        {   Columns = columns
            From = from
            Where = Option.map checker.Expr select.Where
            GroupBy = Option.map checker.GroupBy select.GroupBy
            Info =
                {   Table = SelectResults
                    Query = { Columns = infoColumns }
                } |> TableLike
        }
    member this.CTE(cte : CommonTableExpression) =
        let select = this.Select(cte.AsSelect)
        {   Name = cte.Name
            ColumnNames = cte.ColumnNames
            AsSelect = select
            Info = select.Value.Info
        }
    member this.WithClause(withClause : WithClause) =
        let mutable scope = scope
        TypeChecker(cxt, scope),
            {   Recursive = withClause.Recursive
                Tables =
                    seq {
                        for cte in withClause.Tables ->
                            let cte = TypeChecker(cxt, scope).CTE(cte)
                            scope <-
                                { scope with
                                    CTEVariables = scope.CTEVariables |> Map.add cte.Name cte.Info.Table.Query
                                }
                            cte
                    } |> ResizeArray
            }
    member this.OrderingTerm(orderingTerm : OrderingTerm) =
        {   By = this.Expr(orderingTerm.By)
            Direction = orderingTerm.Direction
        }
    member this.Limit(limit : Limit) =
        {   Limit = this.Expr(limit.Limit, IntegerType Integer64)
            Offset = limit.Offset |> Option.map (fun e -> this.Expr(e, IntegerType Integer64))
        }
    member this.CompoundTerm(term : CompoundTerm) : InfCompoundTerm =
        let info, value =
            match term.Value with
            | Values vals ->
                let vals = vals |> rmap (fun w -> { WithSource.Value = rmap this.Expr w.Value; Source = w.Source })
                let columns =
                    seq {
                        for value in vals.[0].Value ->
                            {   Expr = value
                                FromAlias = None
                                ColumnName = Name("")
                            }
                    } |> toReadOnlyList
                TableLike
                    {   Table = CompoundTermResults
                        Query = { Columns = columns }
                    }, Values vals
            | Select select ->
                let select = this.SelectCore(select)
                select.Info, Select select
        {   Source = term.Source
            Value = value
            Info = info
        }
    member this.Compound(compound : CompoundExpr) =
        {   CompoundExpr.Source = compound.Source
            Value = 
                match compound.Value with
                | CompoundTerm term -> CompoundTerm <| this.CompoundTerm(term)
                | Union (expr, term) -> Union (this.Compound(expr), this.CompoundTerm(term))
                | UnionAll (expr, term) -> UnionAll (this.Compound(expr), this.CompoundTerm(term))
                | Intersect (expr, term) -> Intersect (this.Compound(expr), this.CompoundTerm(term))
                | Except (expr, term) -> Except (this.Compound(expr), this.CompoundTerm(term))
        }
    member this.Select(select : SelectStmt) : InfSelectStmt =
        {   Source = select.Source
            Value =
                let select = select.Value
                let checker, withClause =
                    match select.With with
                    | None -> this, None
                    | Some withClause ->
                        let checker, withClause = this.WithClause(withClause)
                        checker, Some withClause
                let compound = checker.Compound(select.Compound)
                {   With = withClause
                    Compound = compound
                    OrderBy = Option.map (rmap checker.OrderingTerm) select.OrderBy
                    Limit = Option.map checker.Limit select.Limit
                    Info = compound.Value.Info
                }
        }
    member this.ForeignKey(foreignKey) =
        {   ReferencesTable = this.ObjectName(foreignKey.ReferencesTable)
            ReferencesColumns = foreignKey.ReferencesColumns
            Rules = foreignKey.Rules
            Defer = foreignKey.Defer
        }
    member this.ColumnConstraint(constr : ColumnConstraint) =
        {   Name = constr.Name
            ColumnConstraintType =
                match constr.ColumnConstraintType with
                | NullableConstraint -> NullableConstraint
                | PrimaryKeyConstraint clause -> PrimaryKeyConstraint clause
                | NotNullConstraint clause -> NotNullConstraint clause
                | UniqueConstraint conflict -> UniqueConstraint conflict
                | CheckConstraint expr -> CheckConstraint <| this.Expr(expr)
                | DefaultConstraint def -> DefaultConstraint <| this.Expr(def)
                | CollateConstraint name -> CollateConstraint name
                | ForeignKeyConstraint foreignKey -> ForeignKeyConstraint <| this.ForeignKey(foreignKey)
        }
    member this.ColumnDef(cdef : ColumnDef) =
        {   Name = cdef.Name
            Type = cdef.Type
            Constraints = rmap this.ColumnConstraint cdef.Constraints
        }
    member this.Alteration(alteration : AlterTableAlteration) =
        match alteration with
        | RenameTo name -> RenameTo name
        | AddColumn cdef -> AddColumn <| this.ColumnDef(cdef)
    member this.CreateIndex(createIndex : CreateIndexStmt) =
        let tableName = this.ObjectName(createIndex.TableName)
        let checker =
            this.WithScope({ scope with FromClause = Some <| InferredFromClause.FromSingleObject(tableName) })
        {   Unique = createIndex.Unique
            IfNotExists = createIndex.IfNotExists
            IndexName = this.ObjectName(createIndex.IndexName)
            TableName = tableName
            IndexedColumns = createIndex.IndexedColumns |> rmap (fun (e, d) -> checker.Expr(e), d)
            Where = createIndex.Where |> Option.map this.Expr
        }
    member this.TableIndexConstraint(constr : TableIndexConstraintClause) =
        {   Type = constr.Type
            IndexedColumns = constr.IndexedColumns |> rmap (fun (e, d) -> this.Expr(e), d)
            ConflictClause = constr.ConflictClause
        }
    member this.TableConstraint(constr : TableConstraint) =
        {   Name = constr.Name
            TableConstraintType =
                match constr.TableConstraintType with
                | TableIndexConstraint clause ->
                    TableIndexConstraint <| this.TableIndexConstraint(clause)
                | TableForeignKeyConstraint (names, foreignKey) ->
                    TableForeignKeyConstraint (names, this.ForeignKey(foreignKey))
                | TableCheckConstraint expr -> TableCheckConstraint <| this.Expr(expr)
        }
    member this.CreateTableDefinition(tableName : InfObjectName, createTable : CreateTableDefinition) =
        let fake =
            SchemaTable.OfCreateDefinition
                ( tableName.SchemaName |? scope.Model.DefaultSchema
                , tableName.ObjectName
                , createTable
                )
        let from =
            InferredFromClause.FromSingleObject
                ({ tableName with
                    Info =
                        {   Table = TableReference fake
                            Query = InferredQuery.OfTable(fake)
                        } |> TableLike })
        let this = this.WithScope({ scope with FromClause = Some from })
        let columns = createTable.Columns |> rmap this.ColumnDef
        {   Columns = columns
            Constraints = createTable.Constraints |> rmap this.TableConstraint
            WithoutRowId = createTable.WithoutRowId
        }
    member this.CreateTable(createTable : CreateTableStmt) =
        let name = this.ObjectName(createTable.Name, true)
        {   Temporary = createTable.Temporary
            IfNotExists = createTable.IfNotExists
            Name = name
            As =
                match createTable.As with
                | CreateAsSelect select -> CreateAsSelect <| this.Select(select)
                | CreateAsDefinition def -> CreateAsDefinition <| this.CreateTableDefinition(name, def)
        }
    member this.TriggerAction(action : TriggerAction) =
        match action with
        | TriggerUpdate update -> TriggerUpdate <| this.Update(update)
        | TriggerInsert insert -> TriggerInsert <| this.Insert(insert)
        | TriggerDelete delete -> TriggerDelete <| this.Delete(delete)
        | TriggerSelect select -> TriggerSelect <| this.Select(select)
    member this.CreateTrigger(createTrigger : CreateTriggerStmt) =
        {   Temporary = createTrigger.Temporary
            IfNotExists = createTrigger.IfNotExists
            TriggerName = this.ObjectName(createTrigger.TriggerName, true)
            TableName = this.ObjectName(createTrigger.TableName)
            Schedule = createTrigger.Schedule
            Cause = createTrigger.Cause
            Condition = Option.map this.Expr createTrigger.Condition
            Actions = rmap this.TriggerAction createTrigger.Actions
        }
    member this.CreateView(createView : CreateViewStmt) =
        {   Temporary = createView.Temporary
            IfNotExists = createView.IfNotExists
            ViewName = this.ObjectName(createView.ViewName, true)
            ColumnNames = createView.ColumnNames
            AsSelect = this.Select(createView.AsSelect)
        }
    member this.QualifiedTableName(qualified : QualifiedTableName) =
        {   TableName = this.ObjectName(qualified.TableName)
            IndexHint = qualified.IndexHint
        }
    member this.Delete(delete : DeleteStmt) =
        let checker, withClause =
            match delete.With with
            | None -> this, None
            | Some withClause ->
                let checker, withClause = this.WithClause(withClause)
                checker, Some withClause
        let deleteFrom = checker.QualifiedTableName(delete.DeleteFrom)
        let checker =
            checker.WithScope
                ({ checker.Scope with FromClause = InferredFromClause.FromSingleObject(deleteFrom.TableName) |> Some })
        {   With = withClause
            DeleteFrom = deleteFrom
            Where = Option.map checker.Expr delete.Where
            OrderBy = Option.map (rmap checker.OrderingTerm) delete.OrderBy
            Limit = Option.map checker.Limit delete.Limit
        }
    member this.DropObject(drop : DropObjectStmt) =
        {   Drop = drop.Drop
            IfExists = drop.IfExists
            ObjectName = this.ObjectName(drop.ObjectName)
        }
    member this.Insert(insert : InsertStmt) =
        let checker, withClause =
            match insert.With with
            | None -> this, None
            | Some withClause ->
                let checker, withClause = this.WithClause(withClause)
                checker, Some withClause
        {   With = withClause
            Or = insert.Or
            InsertInto = checker.ObjectName(insert.InsertInto)
            Columns = insert.Columns
            Data = Option.map checker.Select insert.Data
        }
    member this.Update(update : UpdateStmt) =
        let checker, withClause =
            match update.With with
            | None -> this, None
            | Some withClause ->
                let checker, withClause = this.WithClause(withClause)
                checker, Some withClause
        let updateTable = checker.QualifiedTableName(update.UpdateTable)
        let checker =
            checker.WithScope
                ({ checker.Scope with FromClause = InferredFromClause.FromSingleObject(updateTable.TableName) |> Some })
        {   With = withClause
            UpdateTable = updateTable
            Or = update.Or
            Set = update.Set |> rmap (fun (name, expr) -> name, checker.Expr(expr))
            Where = Option.map checker.Expr update.Where
            OrderBy = Option.map (rmap checker.OrderingTerm) update.OrderBy
            Limit = Option.map checker.Limit update.Limit
        }
    member this.Stmt(stmt : Stmt) =
        match stmt with
        | AlterTableStmt alter ->
            AlterTableStmt <|
                {   Table = this.ObjectName(alter.Table)
                    Alteration = this.Alteration(alter.Alteration)
                }
        | CreateIndexStmt index -> CreateIndexStmt <| this.CreateIndex(index)
        | CreateTableStmt createTable -> CreateTableStmt <| this.CreateTable(createTable)
        | CreateTriggerStmt createTrigger -> CreateTriggerStmt <| this.CreateTrigger(createTrigger)
        | CreateViewStmt createView -> CreateViewStmt <| this.CreateView(createView)
        | DeleteStmt delete -> DeleteStmt <| this.Delete(delete)
        | DropObjectStmt drop -> DropObjectStmt <| this.DropObject(drop)
        | InsertStmt insert -> InsertStmt <| this.Insert(insert)
        | SelectStmt select -> SelectStmt <| this.Select(select)
        | UpdateStmt update -> UpdateStmt <| this.Update(update)
        | BeginStmt -> BeginStmt
        | CommitStmt -> CommitStmt
        | RollbackStmt -> RollbackStmt