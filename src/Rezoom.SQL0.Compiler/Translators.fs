namespace Rezoom.SQL.Compiler.Translators
open System
open System.Data
open System.Collections.Generic
open Rezoom.SQL.Compiler
open Rezoom.SQL.Mapping
open Rezoom.SQL.Compiler.BackendUtilities

// MSSQL doesn't treat booleans as first-class values, and we don't want to have to rewrite the
// entire statement translator for it, so we pass this context around to hint to the ExprTranslator
// that it may need to fudge in a "CASE WHEN expr THEN 1 ELSE 0 END" to get a usable value.
type ExprTranslationContext =
    /// The expression is expected to produce a first-class value
    /// that can be passed to functions, returned from a select, etc.
    | FirstClassValue
    /// The expression is expected to produce a value suitable for a predicate like a "WHERE" clause or
    /// condition within a "CASE" expression.
    | Predicate

[<AbstractClass>]
type LiteralTranslator() =
    abstract member NullLiteral : Fragment
    abstract member BooleanLiteral : t : bool -> Fragment
    abstract member StringLiteral : str : string -> Fragment
    abstract member BlobLiteral : bytes : byte array -> Fragment
    abstract member IntegerLiteral : i : uint64 -> Fragment
    abstract member FloatLiteral : f : float -> Fragment
    abstract member DateTimeLiteral : dt : DateTime -> Fragment
    abstract member DateTimeOffsetLiteral : dt : DateTimeOffset -> Fragment
    abstract member Literal : literal : Literal -> Fragment
    abstract member SignedLiteral : literal : SignedNumericLiteral -> Fragments

[<AbstractClass>]
type StatementTranslator() =
    abstract member Expr : ExprTranslator
    abstract member OrderDirection : OrderDirection -> Fragment
    abstract member CTE : cte : TCommonTableExpression -> Fragments
    abstract member With : withClause : TWithClause -> Fragments
    abstract member Values : vals : TExpr array WithSource array -> Fragments
    abstract member ResultColumn : expr : TExpr * alias : Alias -> Fragments
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
    abstract member ForeignKeyOnDelete : onDelete : OnDeleteAction -> Fragments
    abstract member ForeignKeyClause : clause : TForeignKeyClause -> Fragments
    abstract member TableConstraint : table : TObjectName * constr : TTableConstraint -> Fragments
    abstract member ColumnConstraint : table : TObjectName * constr : TColumnConstraint -> Fragments
    abstract member ColumnDefinition : table : TObjectName * col : TColumnDef -> Fragments
    abstract member CreateTableDefinition : table : TObjectName * create : TCreateTableDefinition -> Fragments
    abstract member CreateTable : create : TCreateTableStmt -> Fragments
    abstract member AlterTable : alter : TAlterTableStmt -> Fragments
    abstract member CreateView : create : TCreateViewStmt -> Fragments
    abstract member CreateIndex : create : TCreateIndexStmt -> Fragments
    abstract member DropObject : drop : TDropObjectStmt -> Fragments
    abstract member Insert : insert : TInsertStmt -> Fragments
    abstract member Update : update : TUpdateStmt -> Fragments
    abstract member Delete : delete : TDeleteStmt -> Fragments
    abstract member Begin : Fragments
    abstract member Commit : Fragments
    abstract member Rollback : Fragments
    abstract member Statement : TStmt -> Fragments
    abstract member Statements : TStmt seq -> Fragments
    abstract member Vendor : TVendorStmt -> Fragments
    abstract member TotalStatement : TTotalStmt -> Fragments
    abstract member TotalStatements : TTotalStmt seq -> Fragments

and [<AbstractClass>] ExprTranslator() =
    abstract member Literal : LiteralTranslator
    abstract member Name : name : Name -> Fragment
    abstract member CollationName : name : Name -> CommandFragment
    abstract member BinaryOperator : op : BinaryOperator -> Fragment
    abstract member UnaryOperator : op : UnaryOperator -> Fragment
    abstract member SimilarityOperator : invert : bool * op : SimilarityOperator -> Fragment
    abstract member BindParameter : par : BindParameter -> Fragment
    abstract member ObjectName : name : TObjectName -> Fragments
    abstract member ColumnName : column : TColumnName -> Fragments
    abstract member TypeName : ty : TypeName * autoIncrement : bool -> Fragments
    member this.TypeName(ty) = this.TypeName(ty, autoIncrement = false)
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
    abstract member Exists : subquery : TSelectStmt -> Fragments
    abstract member ScalarSubquery : subquery : TSelectStmt -> Fragments
    abstract member NeedsParens : TExpr -> bool
    abstract member Expr : expr : TExpr * context : ExprTranslationContext -> Fragments


