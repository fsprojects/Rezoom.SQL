namespace Rezoom.ORM
open LicenseToCIL
open LicenseToCIL.Stack

/// A conversion that assumes an obj is on the stack, and pushes a value of whatever type is being
/// converted to (depends on the context in which you see the conversion).
type ConversionMethod = Op<E S, E S>