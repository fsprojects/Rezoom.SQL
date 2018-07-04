namespace Rezoom.SQL.Synchronous
open System.Collections.Generic
open System.Data.Common
open System.Runtime.CompilerServices
open Rezoom.SQL

/// Extension methods for executing commands synchronously against a database.
[<Extension>]
type Extensions = class
    /// Execute the command on a connection and return its result sets.
    [<Extension>]
    static member Execute : cmd : Command<'a> * conn : DbConnection -> 'a

    /// Execute the command on a connection and return its result sets.
    /// The connection is obtained from the given `ConnectionContext` according to the command's `ConnectionName`
    /// property.
    [<Extension>]
    static member Execute : cmd : Command<'a> * context:ConnectionContext -> 'a

    /// Execute the command on a connection and return the optional first and only row of its single result set.
    /// If the command returns more than 1 row, this throws an exception.
    [<Extension>]
    static member ExecuteTryExactlyOne : cmd : Command<#IReadOnlyList<'a>> * conn : DbConnection -> 'a option

    /// Execute the command on a connection and return the optional first and only row of its single result set.
    /// If the command returns more than 1 row, this throws an exception.
    /// The connection is obtained from the given `ConnectionContext` according to the command's `ConnectionName`
    /// property.
    [<Extension>]
    static member ExecuteTryExactlyOne : cmd : Command<#IReadOnlyList<'a>> * context : ConnectionContext -> 'a option

    /// Execute the command on a connection and return the first and only row of its single result set.
    /// If the command returns no rows or more than 1 row, this throws an exception.
    [<Extension>]
    static member ExecuteExactlyOne : cmd : Command<#IReadOnlyList<'a>> * conn : DbConnection -> 'a

    /// Execute the command on a connection and return the first and only row of its single result set.
    /// If the command returns no rows or more than 1 row, this throws an exception.
    /// The connection is obtained from the given `ConnectionContext` according to the command's `ConnectionName`
    /// property.
    [<Extension>]
    static member ExecuteExactlyOne : cmd : Command<#IReadOnlyList<'a>> * context : ConnectionContext -> 'a

    /// Execute the command on a connection and return its scalar result.
    [<Extension>]
    static member ExecuteScalar : cmd : Command<#IScalar<'a>> * conn : DbConnection -> 'a

    /// Execute the command on a connection and return its scalar result.
    /// The connection is obtained from the given `ConnectionContext` according to the command's `ConnectionName`
    /// property.
    [<Extension>]
    static member ExecuteScalar : cmd : Command<#IScalar<'a>> * context : ConnectionContext -> 'a
end