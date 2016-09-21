module Rezoom.ORM.SQLProvider.BackendUtilities
open System
open System.Globalization
open System.Collections.Generic
open Rezoom.ORM
open SQLow

type Fragments = CommandFragment seq

let simplifyFragments (fragments : Fragments) =
    fragments // TODO: combine consecutive CommandText fragments

[<AbstractClass>]
type LiteralTranslator() =
    abstract member NullLiteral : CommandFragment
    abstract member CurrentTimeLiteral : CommandFragment
    abstract member CurrentDateLiteral : CommandFragment
    abstract member CurrentTimestampLiteral : CommandFragment
    abstract member StringLiteral : str : string -> CommandFragment
    abstract member BlobLiteral : bytes : byte array -> CommandFragment
    abstract member IntegerLiteral : i : uint64 -> CommandFragment
    abstract member FloatLiteral : f : float -> CommandFragment
    abstract member Literal : literal : Literal -> CommandFragment
    default __.NullLiteral = CommandText "NULL"
    default __.CurrentTimeLiteral = CommandText "CURRENT_TIME"
    default __.CurrentDateLiteral = CommandText "CURRENT_DATE"
    default __.CurrentTimestampLiteral = CommandText "CURRENT_TIMESTAMP"
    default __.IntegerLiteral i = CommandText (i.ToString(CultureInfo.InvariantCulture))
    default __.FloatLiteral f = CommandText (f.ToString("0.0##############", CultureInfo.InvariantCulture))
    default this.Literal literal =
        match literal with
        | NullLiteral -> this.NullLiteral
        | CurrentTimeLiteral -> this.CurrentTimeLiteral
        | CurrentDateLiteral -> this.CurrentDateLiteral
        | CurrentTimestampLiteral -> this.CurrentTimestampLiteral
        | StringLiteral str -> this.StringLiteral(str)
        | BlobLiteral blob -> this.BlobLiteral(blob)
        | NumericLiteral (IntegerLiteral i) -> this.IntegerLiteral(i)
        | NumericLiteral (FloatLiteral f) -> this.FloatLiteral(f)

[<AbstractClass>]
type StatementTranslator() =
    abstract member Expr : ExprTranslator
    abstract member ObjectName : name : ObjectName -> Fragments
    abstract member Table : TableInvocation -> Fragments
    abstract member OrderDirection : OrderDirection -> Fragments
    abstract member With : withClause : WithClause -> Fragments
    abstract member Values : vals : Expr ResizeArray WithSource ResizeArray -> Fragments
    abstract member ResultColumn : ResultColumn -> Fragments
    abstract member SelectCore : select : SelectCore -> Fragments
    abstract member CompoundTerm : compound : CompoundTermCore -> Fragments
    abstract member Compound : compound : CompoundExprCore -> Fragments
    abstract member Select : select : SelectStmt -> Fragments
    abstract member Join : Join -> Fragments
    abstract member Statement : Stmt -> Fragments

and [<AbstractClass>] ExprTranslator() =
    abstract member Literal : LiteralTranslator
    abstract member Name : name : Name -> CommandFragment
    abstract member BinaryOperator : op : BinaryOperator -> CommandFragment
    abstract member UnaryOperator : op : UnaryOperator -> CommandFragment
    abstract member SimilarityOperator : op : SimilarityOperator -> CommandFragment

    abstract member BindParameter : par : BindParameter -> CommandFragment
    abstract member ColumnName : column : ColumnName -> CommandFragment
    abstract member Cast : castExpr : CastExpr -> Fragments
    abstract member Collate : expr : Expr * collation : Name -> Fragments
    abstract member Invoke : func : FunctionInvocationExpr -> Fragments
    abstract member Similarity
        : not : bool
        * op : SimilarityOperator
        * input : Expr
        * pattern : Expr
        * escape : Expr option
        -> Fragments
    abstract member Binary : op : BinaryOperator * left : Expr * right : Expr -> Fragments
    abstract member Unary : op : UnaryOperator * expr : Expr -> Fragments
    abstract member Between
        : not : bool
        * input : Expr
        * low : Expr
        * high : Expr
        -> Fragments
    abstract member In
        : not : bool
        * input : Expr
        * set : InSet
        -> Fragments
    abstract member Case : case : CaseExpr -> Fragments
    abstract member Raise : raise : Raise -> Fragments
    abstract member Exists : subquery : Fragments -> Fragments
    abstract member ScalarSubquery : subquery : Fragments -> Fragments

    abstract member Expr : expr : Expr * select : (SelectStmt -> Fragments) -> Fragments