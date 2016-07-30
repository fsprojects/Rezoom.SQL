namespace Rezoom.ORM
open System.Collections.Generic

[<AllowNullLiteral>]
type ColumnMap() =
    let columns = new Dictionary<string, int>()
    let subMaps = new Dictionary<string, ColumnMap>()
    member private this.GetOrCreateSubMap(name) =
        let succ, sub = subMaps.TryGetValue(name)
        if succ then sub else
        let sub = new ColumnMap()
        subMaps.[name] <- sub
        sub
    member private this.SetColumn(name, index) =
        columns.[name] <- index
    member private this.Load(columnNames : string array) =
        let root = this
        let mutable current = this
        for i = 0 to columnNames.Length - 1 do
            let path = columnNames.[i].Split('.', '$')
            if path.Length > 1 then
                current <- root
                for j = 0 to path.Length - 2 do
                    current <- current.GetOrCreateSubMap(path.[j])
            current.SetColumn(Array.last path, i)
    member this.GetColumn(name) =
        let succ, i = columns.TryGetValue(name)
        if succ then i else -1
    member this.SubMap(name) =
        let succ, map = subMaps.TryGetValue(name)
        if succ then map else null
    static member Parse(columnNames) =
        let map = new ColumnMap()
        map.Load(columnNames)
        map
