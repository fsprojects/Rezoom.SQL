namespace Rezoom.SQL.Mapping
open System
open System.Collections.Generic

type ColumnType =
    | Invalid  = 0s
    | Object   = 1s // whatever it is goes through boxing
    | String   = 2s
    | Byte     = 3s
    | Int16    = 4s
    | Int32    = 5s
    | Int64    = 6s
    | SByte    = 7s
    | UInt16   = 8s
    | UInt32   = 9s
    | UInt64   = 10s
    | Single   = 11s
    | Double   = 12s
    | Decimal  = 13s
    | DateTime = 14s
    | DateTimeOffset = 15s
    | Boolean        = 16s
    | Guid           = 17s

[<Struct>]
type ColumnInfo =
    // must be mutable to be able to access with ldfld from generated code
    val mutable public Index : int16
    val mutable public Type : ColumnType
    new (index, rowValueType) = { Index = index; Type = rowValueType }

    static member IndexField = typeof<ColumnInfo>.GetField("Index")
    static member TypeField = typeof<ColumnInfo>.GetField("Type")
    member this.CLRType =
        match this.Type with
        | ColumnType.Invalid -> typeof<unit>
        | ColumnType.Object -> typeof<obj>
        | ColumnType.String -> typeof<string>
        | ColumnType.Byte -> typeof<byte>
        | ColumnType.Int16 -> typeof<int16>
        | ColumnType.Int32 -> typeof<int32>
        | ColumnType.Int64 -> typeof<int64>
        | ColumnType.SByte -> typeof<sbyte>
        | ColumnType.UInt16 -> typeof<uint16>
        | ColumnType.UInt32 -> typeof<uint32>
        | ColumnType.UInt64 -> typeof<uint64>
        | ColumnType.Single -> typeof<single>
        | ColumnType.Double -> typeof<double>
        | ColumnType.Decimal -> typeof<decimal>
        | ColumnType.DateTime -> typeof<DateTime>
        | _  -> invalidArg "type" "Unknown column type"

[<AllowNullLiteral>]
type ColumnMap(columns, subMaps) =
    static let columnMethod = typeof<ColumnMap>.GetMethod("Column")
    static let primaryColumnMethod = typeof<ColumnMap>.GetMethod("PrimaryColumn")
    static let subMapMethod = typeof<ColumnMap>.GetMethod("SubMap")
    new() =
        let columns = Dictionary<string, ColumnInfo>(StringComparer.OrdinalIgnoreCase)
        let subMaps = Dictionary<string, ColumnMap>(StringComparer.OrdinalIgnoreCase)
        ColumnMap(columns, subMaps)
    member private this.GetOrCreateSubMap(name) =
        let succ, sub = subMaps.TryGetValue(name)
        if succ then sub else
        let sub = ColumnMap()
        subMaps.[name] <- sub
        sub
    member private this.SetColumn(name, info) =
        columns.[name] <- info
    member private this.Load(columnNames : (string * ColumnType) array) =
        for i = 0 to columnNames.Length - 1 do
            let mutable current = this
            let name, rowValueType = columnNames.[i]
            let path = name.Split('.', '$')
            if path.Length > 1 then
                current <- this
                for j = 0 to path.Length - 2 do
                    current <- current.GetOrCreateSubMap(path.[j])
            current.SetColumn(Array.last path, ColumnInfo(int16 i, rowValueType))
    member this.Column(name) =
        let succ, info = columns.TryGetValue(name)
        if succ then info else ColumnInfo(-1s, ColumnType.Invalid)
    member this.PrimaryColumn() =
        columns.Values |> Seq.head
    member this.SubMap(name) =
        let succ, map = subMaps.TryGetValue(name)
        if succ then map
        else
            let succ, info = columns.TryGetValue(name)
            if succ then
                let cols = Dictionary()
                cols.[name] <- info
                ColumnMap(cols, Dictionary())
            else null
    member this.SubMaps = subMaps :> _ seq
    member this.Columns = columns :> _ seq
    static member Parse(columnNames) =
        let map = ColumnMap()
        map.Load(columnNames)
        map

    static member internal PrimaryColumnMethod = primaryColumnMethod
    static member internal ColumnMethod = columnMethod
    static member internal SubMapMethod = subMapMethod
