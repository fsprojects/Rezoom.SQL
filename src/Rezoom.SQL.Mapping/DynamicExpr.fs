module Rezoom.SQL.DynamicExpr
open System
open System.Data
open Rezoom.SQL.Mapping
open System.Collections.Generic

type DynColumnName internal (name : string) =
    do
        if isNull name then invalidArg "name" "name cannot be null"
    member __.Value = name
    member __.Equals(other : DynColumnName) = name = other.Value
    override __.Equals(other : obj) =
        match other with
        | :? DynColumnName as other -> name = other.Value
        | _ -> false
    override __.GetHashCode() = name.GetHashCode()
    interface IEquatable<DynColumnName> with
        member this.Equals(other) = this.Equals(other)
    static member Unsafe<'a>(name : string) =
        DynColumnName<'a>(name)

and DynColumnName<'cty> internal (name : string) =
    inherit DynColumnName(name)

type DynOrderDirection =
    | DynAscending
    | DynDescending

type DynBinaryOperator =
    | DynEqual
    | DynNotEqual
    | DynIs
    | DynIsNot
    | DynLessThan
    | DynLessThanOrEqual
    | DynGreaterThan
    | DynGreaterThanOrEqual
    | DynAnd
    | DynOr
    | DynAdd
    | DynSubtract
    | DynMultiply
    | DynDivide
    | DynConcatenate

type DynUnaryOperator =
    | DynNot
    | DynNegative

[<NoComparison>]
type DynExpr =
    | DynColumn of DynColumnName
    | DynParameter of DbType * obj
    | DynBinary of DynBinaryOperator * DynExpr * DynExpr
    | DynUnary of DynUnaryOperator * DynExpr

type DynPage =
    | TotalCount
    | LimitOffset of int * int option

[<NoComparison>]
type DynExpr<'cty> =
    private
    | TDynExpr of DynExpr
    member this.Untyped = let (TDynExpr x) = this in x
    static member Unsafe(untyped) = TDynExpr untyped

[<NoComparison>]
type DynQueryWrapper =
    {   Where : DynExpr<bool> option
        OrderBy : (DynColumnName * DynOrderDirection) list
        Page : DynPage option
    }
    static member Empty =
        {   Where = None
            Page = None
            OrderBy = []
        }