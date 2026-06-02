namespace Rezoom.SQL.Provider
open System
open System.Collections.Generic
open Rezoom.SQL.Compiler

/// Same mapping as Rezoom.SQL.Mapping.ColumnMap, but carries more metadata about the columns
/// known from Rezoom.SQL.
type private CompileTimeColumnMap(rawColumns : ColumnType ColumnExprInfo IReadOnlyList) as this =
    let columns = Dictionary<string, int16 * ColumnType ColumnExprInfo>(StringComparer.OrdinalIgnoreCase)
    let subMaps = Dictionary<string, CompileTimeColumnMap>(StringComparer.OrdinalIgnoreCase)
    do
        for i = 0 to columns.Count - 1 do
            let mutable current = this
            let column = rawColumns.[i]
            let path = column.ColumnName.Value.Split('.', '$')
            if path.Length > 1 then
                current <- this
                for j = 0 to path.Length - 2 do
                    current <- current.GetOrCreateSubMap(path.[j], column)
            current.SetColumn(Array.last path, (int16 i, column))
    member private this.GetOrCreateSubMap(name, col) =
        let succ, sub = subMaps.TryGetValue(name)
        if succ then sub else
        let sub = CompileTimeColumnMap([| col |])
        subMaps.[name] <- sub
        sub
    member private this.SetColumn(name, info) =
        columns.[name] <- info
    member this.HasSubMaps = subMaps.Count > 0
    member this.SubMaps = subMaps :> _ seq
    member this.Columns = columns :> _ seq
    member this.FirstColumn = rawColumns.[0]
