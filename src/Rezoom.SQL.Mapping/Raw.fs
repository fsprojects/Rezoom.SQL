/// Helpers for building the raw SQL fragments and inline parameters that
/// `unsafe_inject_raw` accepts inside an otherwise-static `SQL<...>` query.
/// Anything passed through here bypasses RZSQL parsing, typechecking, and
/// dialect translation, so write your fragments in your backend's native SQL.
module Rezoom.SQL.Raw
open System
open System.Data
open Rezoom.SQL.Mapping
open System.Collections.Generic

let sql text = CommandText text

let argOfType dbType o =
    InlineParameter (dbType, o)

let private typeMap =
    [|  typeof<byte>, DbType.Byte
        typeof<sbyte>, DbType.SByte
        typeof<int16>, DbType.Int16
        typeof<uint16>, DbType.UInt16
        typeof<int>, DbType.Int32
        typeof<uint32>, DbType.UInt32
        typeof<int64>, DbType.Int64
        typeof<uint64>, DbType.UInt64
        typeof<string>, DbType.String
        typeof<double>, DbType.Double
        typeof<single>, DbType.Single
        typeof<bool>, DbType.Boolean
        typeof<Guid>, DbType.Guid
        typeof<decimal>, DbType.Decimal
        typeof<DateTime>, DbType.DateTime
        typeof<DateTimeOffset>, DbType.DateTimeOffset
    |] |> dict

let private guessDbType (ty : Type) =
    let succ, found = typeMap.TryGetValue(ty)
    if succ then found else DbType.Object

let arg (o : obj) =
    let dbType =
        if isNull o then DbType.Object
        else guessDbType (o.GetType())
    argOfType dbType o
