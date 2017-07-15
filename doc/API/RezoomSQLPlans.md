# Rezoom.SQL.Plans

This namespace contains extension methods to run SQL commands as [Rezoom
plans](../Rezoom/README.md).

## Simple execution

This extension creates a plan that will run the given command and return its result directly.

```fsharp
static member Plan : cmd : Command<'a> -> Plan<'a>
```

## Taking the only result

If you execute a query with a `limit 1` clause, you will probably not want to
deal with the list of rows you get back. You'll only care about the first
returned row, and you might want to assert that there aren't more rows just as a
sanity check. These methods do that. Both will throw if the result list contains
more than one object. The `TryExactlyOne` variant will accept empty lists and
return `None`, while the `ExactlyOne` variant will throw on empty lists.

```fsharp
static member TryExactlyOne : cmd : Command<#IReadOnlyList<'a>> -> Plan<'a option>
static member ExactlyOne : command : Command<#IReadOnlyList<'a>> -> Plan<'a>
```

Note that it is not necessary to use these methods when RZSQL can statically
infer that a query will always return exactly one row. For example, if you
execute a simple `select 1 as x, 2 as y;` with no `where` or `limit` clause,
that will always produce a single row, so RZSQL will generate the command type
as `Command<Query.Row>` instead of `Command<IReadOnlyList<Query.Row>>`.

## Processing scalar results

When a generated row type has only one column, it will get an automatic
implementation of `IScalar<TypeOfColumn>`. This allows you to use this
extension method for convenient handling of single-row, single-column queries
like `select rowcount() as x;`.

```fsharp
static member Scalar : cmd : Command<#IScalar<'a>> -> Plan<'a>
```
