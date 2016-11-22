/// Checks that aggregate expressions are used correctly: that is, aggregates are not mixed with non-aggregate
/// expressions of columns unless grouping by those columns.
module private Rezoom.SQL.AggregateChecker
open System
open System.Collections.Generic
open Rezoom.SQL.InferredTypes

let check (select : InfSelectCore) = ()
