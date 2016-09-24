namespace SQLow
open System
open System.Collections.Generic

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

type ColumnExprInfo<'t> =
    {   Expr : Expr<'t ObjectInfo, 't ExprInfo>
        ColumnName : Name
    }

and QueryExprInfo<'t> =
    {   Columns : 't ColumnExprInfo IReadOnlyList
        ReferencedTables : SchemaTable seq
    }

and TableReference =
    | TableReference of SchemaTable
    | ViewReference of SchemaView
    | LocalQueryReference

and TableLikeExprInfo<'t> =
    {   Table : TableReference
        Query : QueryExprInfo<'t>
    }

and ObjectInfo<'t> =
    | TableLike of 't TableLikeExprInfo
    | Index
    | Trigger
    member this.Table =
        match this with
        | TableLike t -> t
        | other -> failwithf "Expected table, but found reference to %A" other

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
type TJoin = Join<ColumnType ObjectInfo, ColumnType ExprInfo>
type TLimit = Limit<ColumnType ObjectInfo, ColumnType ExprInfo>
type TOrderingTerm = OrderingTerm<ColumnType ObjectInfo, ColumnType ExprInfo>
type TResultColumn = ResultColumn<ColumnType ObjectInfo, ColumnType ExprInfo>
type TResultColumns = ResultColumns<ColumnType ObjectInfo, ColumnType ExprInfo>
type TTableOrSubquery = TableOrSubquery<ColumnType ObjectInfo, ColumnType ExprInfo>
type TTableExprCore = TableExprCore<ColumnType ObjectInfo, ColumnType ExprInfo>
type TTableExpr = TableExpr<ColumnType ObjectInfo, ColumnType ExprInfo>
type TTableInvocation = TableInvocation<ColumnType ObjectInfo, ColumnType ExprInfo>
type TSelectStmt = SelectStmt<ColumnType ObjectInfo, ColumnType ExprInfo>
type TStmt = Stmt<ColumnType ObjectInfo, ColumnType ExprInfo>