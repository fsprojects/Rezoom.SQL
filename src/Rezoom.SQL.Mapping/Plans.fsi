namespace Rezoom.SQL.Plans
open System.Runtime.CompilerServices
open System.Collections.Generic
open Rezoom
open Rezoom.SQL.Mapping
open Rezoom.SQL

/// Extension methods for executing commands as Rezoom plans.
[<Extension>]
type ScalarCommandExtensions = class
    /// Create a plan which will execute the command and return its results.
    [<Extension>]
    static member Plan : cmd : Command<'a> -> Plan<'a>

    /// Create a plan which will execute the command and return its optional single result.
    /// If the command returns more than one row, an exception will be thrown.
    [<Extension>]
    static member TryExactlyOne : cmd : Command<#IReadOnlyList<'a>> -> Plan<'a option>

    /// Create a plan which will execute the command and return its single result.
    /// If the command returns no rows or more than one row, an exception will be thrown.
    [<Extension>]
    static member ExactlyOne : command : Command<#IReadOnlyList<'a>> -> Plan<'a>

    /// Create a plan which will execute the command and return its scalar result.
    [<Extension>]
    static member Scalar : cmd : Command<#IScalar<'a>> -> Plan<'a>
end

type SharedCommandFactory<'id, 'a when 'id : equality> = class
    new : buildCommand : ('id seq -> Command<'a IReadOnlyList>) * selector : ('a -> 'id) -> SharedCommandFactory<'id, 'a>
    member ErrandForKey : key : 'id -> Errand<'a IReadOnlyList>
end
