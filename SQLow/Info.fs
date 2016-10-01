namespace SQLow
open System
open System.Collections.Generic

type NameResolution<'a> =
    | Found of 'a
    | NotFound of string
    | Ambiguous of string

type ExprInfo<'t> =
    {   /// The inferred type of this expression.
        Type : 't
        /// Does this expression contain an aggregate? Not including window functions.
        Aggregate : bool
        /// If this expression is a function call, the function that it calls.
        Function : FunctionType option
        /// If this expression accesses a column of a table in the schema, the column's information.
        Column : SchemaColumn option
    }
    member this.PrimaryKey =
        match this.Column with
        | None -> false
        | Some c -> c.PrimaryKey
    static member OfType(t : 't) =
        {   Type = t
            Aggregate = false
            Function = None
            Column = None
        }
    member this.Map(f : 't -> _) =
        {   Type = f this.Type
            Aggregate = this.Aggregate
            Function = this.Function
            Column = this.Column
        }

type ColumnExprInfo<'t> =
    {   Expr : Expr<'t ObjectInfo, 't ExprInfo>
        FromAlias : Name option // table alias this was selected from, if any
        ColumnName : Name
    }
    member this.Map(f : 't -> _) =
        {   Expr =
                let mapping = ASTMapping<'t ObjectInfo, 't ExprInfo, _, _>((fun t -> t.Map(f)), fun e -> e.Map(f))
                mapping.Expr(this.Expr)
            FromAlias = this.FromAlias
            ColumnName = this.ColumnName
        }

and QueryExprInfo<'t> =
    { Columns : 't ColumnExprInfo IReadOnlyList }
    member this.ColumnByName(name) =
        let matches =
            this.Columns
            |> Seq.filter (fun c -> c.ColumnName = name)
            |> Seq.truncate 2
            |> Seq.toList
        match matches with
        | [] -> NotFound <| sprintf "No such column: ``%O``" name
        | [ single ] -> Found single
        | { FromAlias = Some a1 } :: { FromAlias = Some a2 } :: _ when a1 <> a2 ->
            Ambiguous <|
                sprintf "Ambiguous columm: ``%O`` (may refer to %O.%O or %O.%O)"
                    name a1 name a2 name
        | _ -> Ambiguous <| sprintf "Ambigous column: ``%O``" name
    member this.RenameColumns(names : Name IReadOnlyList) =
        if names.Count <> this.Columns.Count then
            Error <| sprintf "%d columns named for a query with %d columns" names.Count this.Columns.Count
        else
            let newColumns =
                (this.Columns, names)
                ||> Seq.map2 (fun col newName -> { col with ColumnName = newName })
                |> toReadOnlyList
            Ok { Columns = newColumns }
    member this.Append(right : 't QueryExprInfo) =
        { Columns = appendLists this.Columns right.Columns }
    member this.Map(f : 't -> _) =
        { Columns = this.Columns |> Seq.map (fun c -> c.Map(f)) |> toReadOnlyList }

and TableReference =
    | TableReference of SchemaTable
    | ViewReference of SchemaView
    | CTEReference of Name
    | FromClauseReference of Name
    | SelectClauseReference of Name
    | SelectResults
    | CompoundTermResults

and TableLikeExprInfo<'t> =
    {   Table : TableReference
        Query : QueryExprInfo<'t>
    }
    member this.Map(f : 't -> _) =
        {   Table = this.Table
            Query = this.Query.Map(f)
        }

and ObjectInfo<'t> =
    | TableLike of 't TableLikeExprInfo
    | Index
    | Trigger
    | Missing
    member this.Table =
        match this with
        | TableLike t -> t
        | other -> failwithf "Expected table, but found reference to %A" other
    member this.Map<'t1>(f : 't -> 't1) : ObjectInfo<'t1> =
        match this with
        | TableLike t -> TableLike (t.Map(f))
        | Index -> Index
        | Trigger -> Trigger
        | Missing -> Missing

type TExprType = ExprType<ColumnType ObjectInfo, ColumnType ExprInfo>
type TExpr = Expr<ColumnType ObjectInfo, ColumnType ExprInfo>
type TInExpr = InExpr<ColumnType ObjectInfo, ColumnType ExprInfo>
type TCollationExpr = CollationExpr<ColumnType ObjectInfo, ColumnType ExprInfo>
type TBetweenExpr = BetweenExpr<ColumnType ObjectInfo, ColumnType ExprInfo>
type TSimilarityExpr = SimilarityExpr<ColumnType ObjectInfo, ColumnType ExprInfo>
type TBinaryExpr = BinaryExpr<ColumnType ObjectInfo, ColumnType ExprInfo>
type TUnaryExpr = UnaryExpr<ColumnType ObjectInfo, ColumnType ExprInfo>
type TObjectName = ObjectName<ColumnType ObjectInfo>
type TColumnName = ColumnName<ColumnType ObjectInfo>
type TInSet = InSet<ColumnType ObjectInfo, ColumnType ExprInfo>
type TCaseExpr = CaseExpr<ColumnType ObjectInfo, ColumnType ExprInfo>
type TCastExpr = CastExpr<ColumnType ObjectInfo, ColumnType ExprInfo>
type TFunctionInvocationExpr = FunctionInvocationExpr<ColumnType ObjectInfo, ColumnType ExprInfo>
    
type TWithClause = WithClause<ColumnType ObjectInfo, ColumnType ExprInfo>
type TCommonTableExpression = CommonTableExpression<ColumnType ObjectInfo, ColumnType ExprInfo>
type TCompoundExprCore = CompoundExprCore<ColumnType ObjectInfo, ColumnType ExprInfo>
type TCompoundExpr = CompoundExpr<ColumnType ObjectInfo, ColumnType ExprInfo>
type TCompoundTermCore = CompoundTermCore<ColumnType ObjectInfo, ColumnType ExprInfo>
type TCompoundTerm = CompoundTerm<ColumnType ObjectInfo, ColumnType ExprInfo>
type TCreateTableDefinition = CreateTableDefinition<ColumnType ObjectInfo, ColumnType ExprInfo>
type TCreateTableStmt = CreateTableStmt<ColumnType ObjectInfo, ColumnType ExprInfo>
type TSelectCore = SelectCore<ColumnType ObjectInfo, ColumnType ExprInfo>
type TJoinConstraint = JoinConstraint<ColumnType ObjectInfo, ColumnType ExprInfo>
type TJoin = Join<ColumnType ObjectInfo, ColumnType ExprInfo>
type TLimit = Limit<ColumnType ObjectInfo, ColumnType ExprInfo>
type TGroupBy = GroupBy<ColumnType ObjectInfo, ColumnType ExprInfo>
type TOrderingTerm = OrderingTerm<ColumnType ObjectInfo, ColumnType ExprInfo>
type TResultColumn = ResultColumn<ColumnType ObjectInfo, ColumnType ExprInfo>
type TResultColumns = ResultColumns<ColumnType ObjectInfo, ColumnType ExprInfo>
type TTableOrSubquery = TableOrSubquery<ColumnType ObjectInfo, ColumnType ExprInfo>
type TTableExprCore = TableExprCore<ColumnType ObjectInfo, ColumnType ExprInfo>
type TTableExpr = TableExpr<ColumnType ObjectInfo, ColumnType ExprInfo>
type TTableInvocation = TableInvocation<ColumnType ObjectInfo, ColumnType ExprInfo>
type TSelectStmt = SelectStmt<ColumnType ObjectInfo, ColumnType ExprInfo>
type TColumnConstraint = ColumnConstraint<ColumnType ObjectInfo, ColumnType ExprInfo>
type TColumnDef = ColumnDef<ColumnType ObjectInfo, ColumnType ExprInfo>
type TAlterTableAlteration = AlterTableAlteration<ColumnType ObjectInfo, ColumnType ExprInfo>
type TCreateIndexStmt = CreateIndexStmt<ColumnType ObjectInfo, ColumnType ExprInfo>
type TTableIndexConstraintClause = TableIndexConstraintClause<ColumnType ObjectInfo, ColumnType ExprInfo>
type TTableConstraint = TableConstraint<ColumnType ObjectInfo, ColumnType ExprInfo>
type TTriggerAction = TriggerAction<ColumnType ObjectInfo, ColumnType ExprInfo>
type TCreateTriggerStmt = CreateTriggerStmt<ColumnType ObjectInfo, ColumnType ExprInfo>
type TCreateViewStmt = CreateViewStmt<ColumnType ObjectInfo, ColumnType ExprInfo>
type TCreateVirtualTableStmt = CreateVirtualTableStmt<ColumnType ObjectInfo>
type TQualifiedTableName = QualifiedTableName<ColumnType ObjectInfo>
type TDeleteStmt = DeleteStmt<ColumnType ObjectInfo, ColumnType ExprInfo>
type TDropObjectStmt = DropObjectStmt<ColumnType ObjectInfo>
type TUpdateStmt = UpdateStmt<ColumnType ObjectInfo, ColumnType ExprInfo>
type TInsertStmt = InsertStmt<ColumnType ObjectInfo, ColumnType ExprInfo>
type TStmt = Stmt<ColumnType ObjectInfo, ColumnType ExprInfo>