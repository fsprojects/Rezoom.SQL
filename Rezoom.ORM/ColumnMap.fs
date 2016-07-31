namespace Rezoom.ORM
open System
open System.Collections.Generic

type ColumnInfo =
    struct
        val Index : int16
        val Type : RowValueType
        new (index, rowValueType) = { Index = index; Type = rowValueType }
    end

[<AllowNullLiteral>]
type ColumnMap() =
    let columns = new Dictionary<string, ColumnInfo>(StringComparer.OrdinalIgnoreCase)
    let subMaps = new Dictionary<string, ColumnMap>(StringComparer.OrdinalIgnoreCase)
    static let columnMethod = typeof<ColumnMap>.GetMethod("Column")
    static let subMapMethod = typeof<ColumnMap>.GetMember("SubMap")
    member private this.GetOrCreateSubMap(name) =
        let succ, sub = subMaps.TryGetValue(name)
        if succ then sub else
        let sub = new ColumnMap()
        subMaps.[name] <- sub
        sub
    member private this.SetColumn(name, info) =
        columns.[name] <- info
    member private this.Load(columnNames : (string * RowValueType) array) =
        let root = this
        let mutable current = this
        for i = 0 to columnNames.Length - 1 do
            let name, rowValueType = columnNames.[i]
            let path = name.Split('.', '$')
            if path.Length > 1 then
                current <- root
                for j = 0 to path.Length - 2 do
                    current <- current.GetOrCreateSubMap(path.[j])
            current.SetColumn(Array.last path, ColumnInfo(int16 i, rowValueType))
    member this.Column(name) =
        let succ, info = columns.TryGetValue(name)
        if succ then info else ColumnInfo(-1s, RowValueType.Invalid)
    member this.SubMap(name) =
        let succ, map = subMaps.TryGetValue(name)
        if succ then map else null
    static member Parse(columnNames) =
        let map = new ColumnMap()
        map.Load(columnNames)
        map

    static member internal ColumnMethod = columnMethod
    static member internal SubMapMethod = subMapMethod
