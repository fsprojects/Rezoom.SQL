/// Provides helpers for building raw SQL commands and parameters.
/// This stuff does *NOT* go through RZSQL parsing/typechecking/translation.
/// It should be a last resort for when you absolutely can't accomplish what you're doing statically.
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

let connectionDynamicCommand<'row> connectionName (sql : CommandFragment array) =
    let cmdData =
        {   ConnectionName = connectionName // should match the one in rzsql.json/App.config
            Fragments = sql
            Identity = ""
            DependencyMask = Rezoom.BitMask.Full
            InvalidationMask = Rezoom.BitMask.Full
            Cacheable = false
            ResultSetCount = None // not statically known
        }
    CommandConstructor.Command1<'row IReadOnlyList>(cmdData, [||])

let dynamicCommand<'row> (sql : CommandFragment array) =
    connectionDynamicCommand<'row> "rzsql" sql