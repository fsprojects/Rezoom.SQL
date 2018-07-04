module private Rezoom.SQL.Compiler.InferredTypes
open Rezoom.SQL
open System
open System.Collections.Generic

type TypeVariableId = int

type CoreInferredType =
    | TypeKnown of CoreColumnType
    | TypeVariable of TypeVariableId

type InferredNullable =
    | NullableUnknown
    | NullableKnown of bool
    | NullableVariable of TypeVariableId
    | NullableEither of InferredNullable * InferredNullable
    | NullableDueToJoin of InferredNullable // outer joins make nulls that wouldn't otherwise happen
    member this.JoinInducedNullabilityDepth() =
        match this with
        | NullableDueToJoin wrap -> 1 + wrap.JoinInducedNullabilityDepth()
        | NullableEither (l, r) -> max (l.JoinInducedNullabilityDepth()) (r.JoinInducedNullabilityDepth())
        | _ -> 0
    /// Remove layers of nullability induced by an outer join.
    member this.StripJoinInducedNullability(depth) =
        if depth <= 0 then this else
        match this with
        | NullableEither (l, r) ->
            NullableEither (l.StripJoinInducedNullability(depth), r.StripJoinInducedNullability(depth))
        | NullableDueToJoin n -> n.StripJoinInducedNullability(depth - 1)
        | _ -> this
    static member Any(nulls) =
        nulls |> Seq.fold (fun l r -> InferredNullable.Either(l, r)) NullableUnknown
    static member Either(left, right) =
        match left, right with
        | (NullableUnknown | NullableKnown false), x
        | x, (NullableUnknown | NullableKnown false) -> x
        | NullableKnown true as t, _ -> t
        | _, (NullableKnown true as t) -> t
        | NullableVariable x as v, NullableVariable y when x = y -> v
        | l, r -> NullableEither(l, r)
    member this.Simplify() =
        match this with
        | NullableUnknown -> NullableKnown false
        | NullableKnown false
        | NullableKnown true
        | NullableVariable _ -> this
        | NullableDueToJoin n -> NullableDueToJoin (n.Simplify())
        | NullableEither (l, r) ->
            match l.Simplify(), r.Simplify() with
            | NullableKnown true, _
            | _, NullableKnown true -> NullableKnown true
            | NullableKnown false, x -> x
            | x, NullableKnown false -> x
            | l, r -> NullableEither(l, r)

type InferredType =
    {   InferredType : CoreInferredType
        InferredNullable : InferredNullable
    }
    member this.StripNullDueToJoin(depth) =
        { this with InferredNullable = this.InferredNullable.StripJoinInducedNullability(depth) }
    static member Of(col) = { InferredNullable = NullableKnown col.Nullable; InferredType = TypeKnown col.Type }
    static member Of(core) = { InferredNullable = NullableUnknown; InferredType = TypeKnown core }
    static member Float = InferredType.Of(FractionalTypeClass)
    static member Integer = InferredType.Of(IntegralTypeClass)
    static member Number = InferredType.Of(NumericTypeClass)
    static member String = InferredType.Of(StringType)
    static member Stringish = InferredType.Of(StringishTypeClass)
    static member Boolean = InferredType.Of(BooleanType)
    static member DateTime = InferredType.Of(DateTimeType)
    static member DateTimeOffset = InferredType.Of(DateTimeOffsetType)
    static member Blob = InferredType.Of(BinaryType)
    static member Scalar = InferredType.Of(ScalarTypeClass)
    static member Dependent(ifNull : InferredType, outputType : CoreColumnType) =
        {   InferredNullable = ifNull.InferredNullable
            InferredType = TypeKnown outputType
        }
    static member OfLiteral(literal : Literal) =
        match literal with
        | NullLiteral -> { InferredNullable = NullableKnown true; InferredType = TypeKnown ScalarTypeClass }
        | BooleanLiteral _ -> InferredType.Boolean
        | StringLiteral _ -> InferredType.String
        | BlobLiteral _ -> InferredType.Blob
        | NumericLiteral (IntegerLiteral _) -> InferredType.Number
        | NumericLiteral (FloatLiteral _) -> InferredType.Float
        | DateTimeLiteral _ -> InferredType.DateTime
        | DateTimeOffsetLiteral _ -> InferredType.DateTimeOffset
    static member OfTypeName(typeName : TypeName, inputType : InferredType) =
        let affinity = CoreColumnType.OfTypeName(typeName)
        {   InferredNullable = inputType.InferredNullable
            InferredType = TypeKnown affinity
        }

type InfExprType = ExprType<InferredType ObjectInfo, InferredType ExprInfo>
type InfExpr = Expr<InferredType ObjectInfo, InferredType ExprInfo>
type InfInExpr = InExpr<InferredType ObjectInfo, InferredType ExprInfo>
type InfCollationExpr = CollationExpr<InferredType ObjectInfo, InferredType ExprInfo>
type InfBetweenExpr = BetweenExpr<InferredType ObjectInfo, InferredType ExprInfo>
type InfSimilarityExpr = SimilarityExpr<InferredType ObjectInfo, InferredType ExprInfo>
type InfBinaryExpr = BinaryExpr<InferredType ObjectInfo, InferredType ExprInfo>
type InfUnaryExpr = UnaryExpr<InferredType ObjectInfo, InferredType ExprInfo>
type InfObjectName = ObjectName<InferredType ObjectInfo>
type InfColumnName = ColumnName<InferredType ObjectInfo>
type InfInSet = InSet<InferredType ObjectInfo, InferredType ExprInfo>
type InfCaseExpr = CaseExpr<InferredType ObjectInfo, InferredType ExprInfo>
type InfCastExpr = CastExpr<InferredType ObjectInfo, InferredType ExprInfo>
type InfFunctionArguments = FunctionArguments<InferredType ObjectInfo, InferredType ExprInfo>
type InfFunctionInvocationExpr = FunctionInvocationExpr<InferredType ObjectInfo, InferredType ExprInfo>
    
type InfWithClause = WithClause<InferredType ObjectInfo, InferredType ExprInfo>
type InfCommonTableExpression = CommonTableExpression<InferredType ObjectInfo, InferredType ExprInfo>
type InfCompoundExprCore = CompoundExprCore<InferredType ObjectInfo, InferredType ExprInfo>
type InfCompoundExpr = CompoundExpr<InferredType ObjectInfo, InferredType ExprInfo>
type InfCompoundTermCore = CompoundTermCore<InferredType ObjectInfo, InferredType ExprInfo>
type InfCompoundTerm = CompoundTerm<InferredType ObjectInfo, InferredType ExprInfo>
type InfCreateTableDefinition = CreateTableDefinition<InferredType ObjectInfo, InferredType ExprInfo>
type InfCreateTableStmt = CreateTableStmt<InferredType ObjectInfo, InferredType ExprInfo>
type InfSelectCore = SelectCore<InferredType ObjectInfo, InferredType ExprInfo>
type InfJoinConstraint = JoinConstraint<InferredType ObjectInfo, InferredType ExprInfo>
type InfJoin = Join<InferredType ObjectInfo, InferredType ExprInfo>
type InfLimit = Limit<InferredType ObjectInfo, InferredType ExprInfo>
type InfGroupBy = GroupBy<InferredType ObjectInfo, InferredType ExprInfo>
type InfOrderingTerm = OrderingTerm<InferredType ObjectInfo, InferredType ExprInfo>
type InfResultColumn = ResultColumn<InferredType ObjectInfo, InferredType ExprInfo>
type InfResultColumns = ResultColumns<InferredType ObjectInfo, InferredType ExprInfo>
type InfTableOrSubquery = TableOrSubquery<InferredType ObjectInfo, InferredType ExprInfo>
type InfTableExprCore = TableExprCore<InferredType ObjectInfo, InferredType ExprInfo>
type InfTableExpr = TableExpr<InferredType ObjectInfo, InferredType ExprInfo>
type InfTableInvocation = TableInvocation<InferredType ObjectInfo, InferredType ExprInfo>
type InfSelectStmt = SelectStmt<InferredType ObjectInfo, InferredType ExprInfo>
type InfColumnConstraint = ColumnConstraint<InferredType ObjectInfo, InferredType ExprInfo>
type InfColumnDef = ColumnDef<InferredType ObjectInfo, InferredType ExprInfo>
type InfAlterTableStmt = AlterTableStmt<InferredType ObjectInfo, InferredType ExprInfo>
type InfAlterTableAlteration = AlterTableAlteration<InferredType ObjectInfo, InferredType ExprInfo>
type InfCreateIndexStmt = CreateIndexStmt<InferredType ObjectInfo, InferredType ExprInfo>
type InfTableIndexConstraintClause = TableIndexConstraintClause<InferredType ObjectInfo, InferredType ExprInfo>
type InfTableConstraint = TableConstraint<InferredType ObjectInfo, InferredType ExprInfo>
type InfCreateViewStmt = CreateViewStmt<InferredType ObjectInfo, InferredType ExprInfo>
type InfDeleteStmt = DeleteStmt<InferredType ObjectInfo, InferredType ExprInfo>
type InfDropObjectStmt = DropObjectStmt<InferredType ObjectInfo>
type InfUpdateStmt = UpdateStmt<InferredType ObjectInfo, InferredType ExprInfo>
type InfInsertStmt = InsertStmt<InferredType ObjectInfo, InferredType ExprInfo>
type InfStmt = Stmt<InferredType ObjectInfo, InferredType ExprInfo>
type InfVendorStmt = VendorStmt<InferredType ObjectInfo, InferredType ExprInfo>
type InfTotalStmt = TotalStmt<InferredType ObjectInfo, InferredType ExprInfo>

type ITypeInferenceContext =
    abstract member AnonymousVariable : unit -> CoreInferredType
    abstract member Variable : BindParameter -> InferredType
    /// Unify the two types (ensure they are compatible and add constraints)
    /// and produce the most specific type.
    abstract member Unify : SourceInfo * CoreInferredType * CoreInferredType -> CoreInferredType
    abstract member UnifyList : SourceInfo * elem : CoreInferredType * list : BindParameter -> unit
    abstract member ForceNullable : SourceInfo * InferredNullable -> unit
    abstract member Concrete : InferredType -> ColumnType
    abstract member Parameters : BindParameter seq

let exprInfoOfColumn (column : SchemaColumn) =
    { ExprInfo<_>.OfType(InferredType.Of(column.ColumnType)) with Column = Some column }

let private queryColumnInfoOf (fromAlias : Name option) (column : SchemaColumn) =
    {   Expr =  
            {   Expr.Source = SourceInfo.Invalid
                Info = exprInfoOfColumn column
                Value = ColumnNameExpr { ColumnName = column.ColumnName; Table = None }
            }
        ColumnName = column.ColumnName
        FromAlias = fromAlias
    }

let foundAt source nameResolution =
    match nameResolution with
    | Found x -> x
    | NotFound err
    | Ambiguous err -> failAt source err

let inferredOfTable (table : SchemaTable) =
    {   Columns =
            table.Columns
            |> Seq.map (function KeyValue(_, c) -> queryColumnInfoOf (Some table.Name.ObjectName) c)
            |> toReadOnlyList
        StaticRowCount = None
        ClausesIdempotent = true
    }

[<NoComparison>]
[<NoEquality>]
type InferredFromClause =
    {   /// The tables named in the "from" clause of the query, if any.
        /// These are keyed on the alias of the table, if any, or the table name.
        FromVariables : IReadOnlyDictionary<Name, InferredType ObjectInfo>
    }
    static member FromSingleObject(tableName : InfObjectName) =
        let d = Dictionary()
        d.Add(Name(""), tableName.Info)
        {   FromVariables = d :> IReadOnlyDictionary<_, _>
        }
    member this.ResolveTable(tableName : ObjectName) =
        match tableName.SchemaName with
        // We don't currently support referencing columns like "main.users.id". Use table aliases instead!
        | Some _ -> Ambiguous <| Error.schemaNameInColumnReference tableName
        | None ->
            let succ, query = this.FromVariables.TryGetValue(tableName.ObjectName)
            if succ then Found query
            else NotFound <| Error.noSuchTableInFrom tableName.ObjectName
    member this.ResolveColumnReference(name : ColumnName) =
        match name.Table with
        | None ->
            let matches =
                seq {
                    for KeyValue(tableAlias, objectInfo) in this.FromVariables do
                        let table = objectInfo.Table
                        match table.Query.ColumnByName(name.ColumnName) with
                        | Found column ->
                            yield Ok ((if tableAlias.Value = "" then None else Some tableAlias), table, column)
                        | NotFound _ -> ()
                        | Ambiguous err -> yield Error err
                } |> toReadOnlyList
            if matches.Count = 1 then
                match matches.[0] with
                | Ok triple -> Found triple
                | Error e -> Ambiguous e
            elif matches.Count <= 0 then
                NotFound <| Error.noSuchColumnInFrom name
            else
                Ambiguous <| Error.ambiguousColumn name
        | Some tableName ->
            match this.ResolveTable(tableName) with
            | Found objectInfo ->
                let table = objectInfo.Table
                match table.Query.ColumnByName(name.ColumnName) with
                | Found column -> Found (Some tableName.ObjectName, table, column)
                | NotFound err -> NotFound err
                | Ambiguous err -> Ambiguous err
            | NotFound err -> NotFound err
            | Ambiguous err -> Ambiguous err

and [<NoComparison>]
    [<NoEquality>] InferredSelectScope =
    {   /// If this scope is that of a subquery, the parent query's scope can also be used
        /// to resolve column and CTE names.
        ParentScope : InferredSelectScope option
        /// The model this select is running against.
        /// This includes tables and views that are part of the database, and may be used to resolve
        /// table names in the "from" clause of the query.
        Model : Model
        /// Any CTEs defined by the query.
        /// These may be referenced in the "from" clause of the query.
        CTEVariables : Map<Name, InferredType QueryExprInfo>
        FromClause : InferredFromClause option
        SelectClause : InferredType QueryExprInfo option
    }

    static member Root(model) =
        {   ParentScope = None
            Model = model
            CTEVariables = Map.empty
            FromClause = None
            SelectClause = None
        }

    member this.Child() =
        {   InferredSelectScope.Root(this.Model) with
                ParentScope = Some this
        }

    member private this.ResolveObjectReferenceBySchema
        (schema : Schema, name : Name, inferView : CreateViewStmt -> TCreateViewStmt) =
        match schema.Objects |> Map.tryFind name with
        | Some (SchemaTable tbl) ->
            { Table = TableReference tbl; Query = inferredOfTable(tbl) } |> TableLike |> Found
        | Some (SchemaView view) ->
            let def = inferView view.CreateDefinition
            let query = def.AsSelect.Value.Info.Query.Map(InferredType.Of)
            { Table = ViewReference(view, def); Query = query } |> TableLike |> Found
        | Some (SchemaIndex index) -> index |> Index |> Found
        | Some (SchemaConstraint _)
        | None -> NotFound <| Error.noSuchObject "object" name

    /// Resolve a reference to a table which may occur as part of a TableExpr.
    /// This will resolve against the database model and CTEs, but not table aliases defined in the FROM clause.
    member this.ResolveObjectReference(name : ObjectName, inferView) =
        match name.SchemaName with
        | None ->
            match this.CTEVariables.TryFind(name.ObjectName) with
            | Some cte -> { Table = CTEReference name.ObjectName; Query = cte } |> TableLike |> Found
            | None ->
                match this.ParentScope with
                | Some parent -> parent.ResolveObjectReference(name, inferView)
                | None ->
                    let schema = this.Model.Schemas.[this.Model.DefaultSchema]
                    this.ResolveObjectReferenceBySchema(schema, name.ObjectName, inferView)
        | Some schema ->
            let schema = this.Model.Schemas.[schema]
            this.ResolveObjectReferenceBySchema(schema, name.ObjectName, inferView)

    /// Resolve a column reference, which may be qualified with a table alias.
    /// This resolves against the tables referenced in the FROM clause, and the columns explicitly named
    /// in the SELECT clause, if any.
    member this.ResolveColumnReference(name : ColumnName) =
        let findFrom() =
            let thisLevel =
                match this.FromClause with
                | None -> NotFound <| Error.columnReferenceWithoutFrom name
                | Some fromClause -> fromClause.ResolveColumnReference(name)
            match this.ParentScope, thisLevel with
            | Some parent, NotFound _ ->
                parent.ResolveColumnReference(name)
            | _ -> thisLevel
        match name.Table, this.SelectClause with
        | None, Some selected ->
            match selected.ColumnByName(name.ColumnName) with
            | Found column ->
                Found (None, { Table = SelectClauseReference name.ColumnName; Query = selected }, column)
            | Ambiguous reason -> Ambiguous reason
            | NotFound _ -> findFrom()
        | _ -> findFrom()

let concreteMapping (inference : ITypeInferenceContext) =
    ASTMapping<InferredType ObjectInfo, InferredType ExprInfo, _, _>
        ((fun t -> t.Map(inference.Concrete)), fun e -> e.Map(inference.Concrete))
