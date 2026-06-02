namespace Rezoom.SQL.Provider
open System
open System.Collections.Generic
open Rezoom.SQL.Compiler

/// Same mapping as Rezoom.SQL.Mapping.ColumnMap, but carries more metadata about the columns
/// known from Rezoom.SQL.
type private CompileTimeColumnMap() =
    let columns = Dictionary<string, int16 * ColumnType ColumnExprInfo>(StringComparer.OrdinalIgnoreCase)
    let subMaps = Dictionary<string, CompileTimeColumnMap>(StringComparer.OrdinalIgnoreCase)
    let mutable firstColumn : ColumnType ColumnExprInfo option = None
    let recordFirst col =
        if firstColumn.IsNone then firstColumn <- Some col
    member private this.GetOrCreateSubMap(name, col) =
        recordFirst col
        let succ, sub = subMaps.TryGetValue(name)
        if succ then sub else
        let sub = CompileTimeColumnMap()
        sub.RecordFirst(col)
        subMaps.[name] <- sub
        sub
    member private this.RecordFirst(col) = recordFirst col
    member private this.SetColumn(name, ((_, col) as info)) =
        recordFirst col
        columns.[name] <- info
    member this.HasSubMaps = subMaps.Count > 0
    member this.SubMaps = subMaps :> _ seq
    member this.Columns = columns :> _ seq
    /// The first column added to this map. Used by interface-impl
    /// codegen as the source-location for errors about this
    /// shape level. Should be non-null because all column sets have at least one member.
    member this.FirstColumn = firstColumn.Value
    static member Parse(rawColumns : ColumnType ColumnExprInfo IReadOnlyList) =
        let root = CompileTimeColumnMap()
        for i = 0 to rawColumns.Count - 1 do
            let mutable current = root
            let column = rawColumns.[i]
            let path = column.ColumnName.Value.Split('.', '$')
            if path.Length > 1 then
                for j = 0 to path.Length - 2 do
                    current <- current.GetOrCreateSubMap(path.[j], column)
            current.SetColumn(Array.last path, (int16 i, column))
        root
