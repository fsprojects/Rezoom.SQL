namespace Rezoom.SQL.Mapping.CodeGeneration
open LicenseToCIL
open LicenseToCIL.Stack

/// A conversion that assumes an obj is on the stack, and pushes a value of whatever type is being
/// converted to (depends on the context in which you see the conversion).
type ConversionMethod = Op<E S, E S>

/// Takes `Row` and `ColumnInfo` and pushes a value of whatever type if being converted to.
type RowConversionMethod = Op<E S S, E S>