namespace StaticQL.Provider
open System
open System.Collections.Generic
open StaticQL

/// Same mapping as StaticQL.Mapping.ColumnMap, but carries more metadata about the columns
/// known from StaticQL.
type private CompileTimeColumnMap() =
    let columns = Dictionary<string, int16 * ColumnType ColumnExprInfo>(StringComparer.OrdinalIgnoreCase)
    let subMaps = Dictionary<string, CompileTimeColumnMap>(StringComparer.OrdinalIgnoreCase)
    member private this.GetOrCreateSubMap(name) =
        let succ, sub = subMaps.TryGetValue(name)
        if succ then sub else
        let sub = CompileTimeColumnMap()
        subMaps.[name] <- sub
        sub
    member private this.SetColumn(name, info) =
        columns.[name] <- info
    // TODO: use inline functions to have a single implementation for this load logic.
    // It's gross duplicating it between ColumnMap and CompileTimeColumnMap.
    member private this.Load(columns : ColumnType ColumnExprInfo IReadOnlyList) =
        let root = this
        let mutable current = this
        for i = 0 to columns.Count - 1 do
            let column = columns.[i]
            let path = column.ColumnName.Value.Split('.', '$')
            if path.Length > 1 then
                current <- root
                for j = 0 to path.Length - 2 do
                    current <- current.GetOrCreateSubMap(path.[j])
            current.SetColumn(Array.last path, (int16 i, column))
    member this.SubMaps = subMaps :> _ seq
    member this.Columns = columns :> _ seq
    static member Parse(columns) =
        let map = CompileTimeColumnMap()
        map.Load(columns)
        map
