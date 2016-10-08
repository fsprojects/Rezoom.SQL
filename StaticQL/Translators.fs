namespace StaticQL.Translators
open System
open System.Data
open System.Collections.Generic
open StaticQL
open StaticQL.Mapping
open StaticQL.BackendUtilities

[<AbstractClass>]
type LiteralTranslator() =
    abstract member NullLiteral : Fragment
    abstract member CurrentTimeLiteral : Fragment
    abstract member CurrentDateLiteral : Fragment
    abstract member CurrentTimestampLiteral : Fragment
    abstract member StringLiteral : str : string -> Fragment
    abstract member BlobLiteral : bytes : byte array -> Fragment
    abstract member IntegerLiteral : i : uint64 -> Fragment
    abstract member FloatLiteral : f : float -> Fragment
    abstract member Literal : literal : Literal -> Fragment
    abstract member SignedLiteral : literal : SignedNumericLiteral -> Fragments

[<AbstractClass>]
type StatementTranslator() =
    abstract member Expr : ExprTranslator
    abstract member OrderDirection : OrderDirection -> Fragment
    abstract member CTE : cte : TCommonTableExpression -> Fragments
    abstract member With : withClause : TWithClause -> Fragments
    abstract member Values : vals : TExpr ResizeArray WithSource ResizeArray -> Fragments
    abstract member ResultColumn : TResultColumn -> Fragments
    abstract member ResultColumns : TResultColumns -> Fragments
    abstract member TableOrSubquery : TTableOrSubquery -> Fragments
    abstract member TableExpr : TTableExpr -> Fragments
    abstract member JoinType : JoinType -> Fragment
    abstract member Join : TJoin -> Fragments
    abstract member SelectCore : select : TSelectCore -> Fragments
    abstract member CompoundTerm : compound : TCompoundTermCore -> Fragments
    abstract member Compound : compound : TCompoundExprCore -> Fragments
    abstract member Limit : TLimit -> Fragments
    abstract member OrderingTerm : TOrderingTerm -> Fragments
    abstract member Select : select : TSelectStmt -> Fragments
    abstract member ConflictClause : clause : ConflictClause -> Fragments
    abstract member ForeignKeyRule : rule : ForeignKeyRule -> Fragments
    abstract member ForeignKeyClause : clause : TForeignKeyClause -> Fragments
    abstract member ColumnConstraint : constr : TColumnConstraint -> Fragments
    abstract member ColumnDefinition : col : TColumnDef -> Fragments
    abstract member CreateTableDefinition : create : TCreateTableDefinition -> Fragments
    abstract member CreateTable : create : TCreateTableStmt -> Fragments
    abstract member AlterTable : alter : TAlterTableStmt -> Fragments
    abstract member CreateView : create : TCreateViewStmt -> Fragments
    abstract member CreateIndex : create : TCreateIndexStmt -> Fragments
    abstract member DropObject : drop : TDropObjectStmt -> Fragments
    abstract member Insert : insert : TInsertStmt -> Fragments
    abstract member Begin : Fragments
    abstract member Commit : Fragments
    abstract member Rollback : Fragments
    abstract member Statement : TStmt -> Fragments
    abstract member Statements : TStmt seq -> Fragments

and [<AbstractClass>] ExprTranslator(statement : StatementTranslator, indexer : IParameterIndexer) =
    abstract member Literal : LiteralTranslator
    abstract member Name : name : Name -> Fragment
    abstract member BinaryOperator : op : BinaryOperator -> Fragment
    abstract member UnaryOperator : op : UnaryOperator -> Fragment
    abstract member SimilarityOperator : op : SimilarityOperator -> Fragment
    abstract member BindParameter : par : BindParameter -> Fragment
    abstract member ObjectName : name : TObjectName -> Fragments
    abstract member ColumnName : column : TColumnName -> Fragments
    abstract member TypeName : TypeName -> Fragments
    abstract member Cast : castExpr : TCastExpr -> Fragments
    abstract member Collate : expr : TExpr * collation : Name -> Fragments
    abstract member Invoke : func : TFunctionInvocationExpr -> Fragments
    abstract member Similarity : sim : TSimilarityExpr -> Fragments
    abstract member Binary : bin : TBinaryExpr -> Fragments
    abstract member Unary : un : TUnaryExpr -> Fragments
    abstract member Between : between : TBetweenExpr -> Fragments
    abstract member Table : TTableInvocation -> Fragments
    abstract member In : inex : TInExpr -> Fragments
    abstract member Case : case : TCaseExpr -> Fragments
    abstract member Raise : raise : Raise -> Fragments
    abstract member Exists : subquery : TSelectStmt -> Fragments
    abstract member ScalarSubquery : subquery : TSelectStmt -> Fragments
    abstract member NeedsParens : TExprType -> bool
    abstract member Expr : expr : TExpr -> Fragments


