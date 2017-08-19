module Rezoom.SQL.DynamicOperators
open System
open System.Data
open Rezoom.SQL.Mapping
open Rezoom.SQL.DynamicExpr
open System.Collections.Generic

let (.=) (left : 'a DynExpr) (right : 'a DynExpr) =
    DynExpr<bool>.Unsafe(DynBinary(DynEqual, left.Untyped, right.Untyped))
let (.<>) (left : 'a DynExpr) (right : 'a DynExpr) =
    DynExpr<bool>.Unsafe(DynBinary(DynNotEqual, left.Untyped, right.Untyped))
let (.>) (left : 'a DynExpr) (right : 'a DynExpr) =
    DynExpr<bool>.Unsafe(DynBinary(DynGreaterThan, left.Untyped, right.Untyped))
let (.<) (left : 'a DynExpr) (right : 'a DynExpr) =
    DynExpr<bool>.Unsafe(DynBinary(DynLessThan, left.Untyped, right.Untyped))
let (.>=) (left : 'a DynExpr) (right : 'a DynExpr) =
    DynExpr<bool>.Unsafe(DynBinary(DynGreaterThanOrEqual, left.Untyped, right.Untyped))
let (.<=) (left : 'a DynExpr) (right : 'a DynExpr) =
    DynExpr<bool>.Unsafe(DynBinary(DynLessThanOrEqual, left.Untyped, right.Untyped))

// IS and IS NOT
let (.===) (left : 'a DynExpr) (right : 'a DynExpr) =
    DynExpr<bool>.Unsafe(DynBinary(DynIs, left.Untyped, right.Untyped))
let (.!==) (left : 'a DynExpr) (right : 'a DynExpr) =
    DynExpr<bool>.Unsafe(DynBinary(DynIsNot, left.Untyped, right.Untyped))

let (.++) (left : string DynExpr) (right : string DynExpr) =
    DynExpr<string>.Unsafe(DynBinary(DynConcatenate, left.Untyped, right.Untyped))

let (.||) (left : bool DynExpr) (right : bool DynExpr) =
    DynExpr<bool>.Unsafe(DynBinary(DynOr, left.Untyped, right.Untyped))
let (.&&) (left : bool DynExpr) (right : bool DynExpr) =
    DynExpr<bool>.Unsafe(DynBinary(DynAnd, left.Untyped, right.Untyped))

let inline (.+) (left : ^a DynExpr) (right : ^a DynExpr) =
    let _ = Unchecked.defaultof< ^a > + Unchecked.defaultof< ^a >
    DynExpr< ^a >.Unsafe(DynBinary(DynAdd, left.Untyped, right.Untyped))

let inline (.-) (left : ^a DynExpr) (right : ^a DynExpr) =
    let _ = Unchecked.defaultof< ^a > - Unchecked.defaultof< ^a >
    DynExpr< ^a >.Unsafe(DynBinary(DynSubtract, left.Untyped, right.Untyped))

let inline (.*) (left : ^a DynExpr) (right : ^a DynExpr) =
    let _ = Unchecked.defaultof< ^a > * Unchecked.defaultof< ^a >
    DynExpr< ^a >.Unsafe(DynBinary(DynMultiply, left.Untyped, right.Untyped))
 
let inline (./) (left : ^a DynExpr) (right : ^a DynExpr) =
    let _ = Unchecked.defaultof< ^a > / Unchecked.defaultof< ^a >
    DynExpr< ^a >.Unsafe(DynBinary(DynDivide, left.Untyped, right.Untyped))

let NOT (x : bool DynExpr) =
    DynExpr<bool>.Unsafe(DynUnary(DynNot, x.Untyped))

let inline (~-.) (x : ^a DynExpr) =
    let _ = -Unchecked.defaultof< ^a >
    DynExpr< ^a >.Unsafe(DynUnary(DynNegative, x.Untyped))