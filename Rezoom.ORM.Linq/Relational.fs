namespace Rezoom.ORM.Linq.Relational
open System

// Represents a relational (table-oriented) query that can be materialized to a given element type.
// Should be easy enough to translate from this representation to various SQL dialects.

type TableName = string
type ColumnName = string
type Alias = string list

type BinaryOperator =
    | And
    | Or
    | Equal
    | NotEqual
    | GreaterThan
    | GreaterThanOrEqual
    | LessThan
    | LessThanOrEqual
    | Like
    | Add
    | Sub
    | Mul
    | Div
    | BitOr
    | BitXor
    | BitAnd
    | BitShiftL
    | BitShiftR

type UnaryOperator =
    | Negate
    | BitComplement
    | Not

type Expr =
    | LiteralExpr of obj
    | ParameterExpr of obj
    | ColumnExpr of TableName * ColumnName
    | BinaryExpr of BinaryOperator * Expr * Expr
    | UnaryExpr of UnaryOperator * Expr
    | BetweenExpr of Expr * (Expr * Expr)
    | InExpr of Expr * Expr seq
    | CaseExpr of (Expr * Expr) seq

type Table =
    {
        TableName : TableName
        AsAlias : Alias
    }

type JoinType =
    | Inner
    | LeftOuter

type Join =
    {
        Join : JoinType
        Table : Table
        On : Expr
    }

type Selection =
    {
        Select : Expr
        AsAlias : Alias option
    }

type OrderDirection =
    | Ascending
    | Descending

type Query =
    {
        Selections : Selection list
        // Used to avoid duplicating includes
        Inclusions : Alias Set
        FromTable : Table
        // Keyed on table alias of the joined table
        Joins : Map<Alias, Join>
        Predicates : Expr list
        OrderBy : (Expr * OrderDirection) list
    }

