# Rezoom.SQL.Asynchronous

This namespace contains extension methods to run SQL commands asynchronously as
`Task`s. Each of these methods has an overload taking a `CancellationToken` in
case you want to be able to stop execution of the query partway through.

You can use these methods on a `DbConnection` obtained however you like. You can
also use them with a `ConnectionContext` which will automatically open
`DbConnection`s as needed using the connection strings in your application's
config file.

## Simple execution

These extensions run the given command and return its result directly.

```fsharp
static member Execute : cmd : Command<'a> * conn : DbConnection -> Task<'a>
static member Execute : cmd : Command<'a> * context : ConnectionContext -> Task<'a>
static member Execute : cmd : Command<'a> * conn : DbConnection * token : CancellationToken -> Task<'a>
static member Execute : cmd : Command<'a> * context : ConnectionContext * token : CancellationToken -> Task<'a>
```

## Taking the only result

If you execute a query with a `limit 1` clause, you will probably not want to
deal with the list of rows you get back. You'll only care about the first
returned row, and you might want to assert that there aren't more rows just as a
sanity check. These methods do that. All will throw if the result list contains
more than one object. The `TryExactlyOne` variants will accept empty lists and
return `None`, while the `ExactlyOne` variants will throw on empty lists.

```fsharp
static member ExecuteExactlyOne
    : cmd : Command<#IReadOnlyList<'a>>
    * conn : DbConnection
    -> Task<'a>

static member ExecuteExactlyOne
    : cmd : Command<#IReadOnlyList<'a>>
    * context : ConnectionContext
    -> Task<'a>

static member ExecuteExactlyOne
    : cmd : Command<#IReadOnlyList<'a>>
    * conn : DbConnection
    * token : CancellationToken
    -> Task<'a>

static member ExecuteExactlyOne
    : cmd : Command<#IReadOnlyList<'a>>
    * context : ConnectionContext
    * token : CancellationToken
    -> Task<'a>

static member ExecuteTryExactlyOne
    : cmd : Command<#IReadOnlyList<'a>>
    * conn : DbConnection
    -> Task<'a option>

static member ExecuteTryExactlyOne
    : cmd : Command<#IReadOnlyList<'a>>
    * context : ConnectionContext
    -> Task<'a option>

static member ExecuteTryExactlyOne
    : cmd : Command<#IReadOnlyList<'a>>
    * conn : DbConnection
    * token : CancellationToken
    -> Task<'a option>

static member ExecuteTryExactlyOne
    : cmd : Command<#IReadOnlyList<'a>>
    * context : ConnectionContext
    * token : CancellationToken
    -> Task<'a option>
```

Note that it is not necessary to use these methods when RZSQL can statically
infer that a query will always return exactly one row. For example, if you
execute a simple `select 1 as x, 2 as y;` with no `where` or `limit` clause,
that will always produce a single row, so RZSQL will generate the command type
as `Command<Query.Row>` instead of `Command<IReadOnlyList<Query.Row>>`.

## Processing scalar results

When a generated row type has only one column, it will get an automatic
implementation of `IScalar<TypeOfColumn>`. This allows you to use these
extension methods for convenient handling of single-row, single-column queries
like `select rowcount() as x;`.

```fsharp
static member ExecuteScalar
    : cmd : Command<#IScalar<'a>>
    * conn : DbConnection
    -> Task<'a>

static member ExecuteScalar
    : cmd : Command<#IScalar<'a>>
    * context : ConnectionContext
    -> Task<'a>

static member ExecuteScalar
    : cmd : Command<#IScalar<'a>>
    * conn : DbConnection
    * token : CancellationToken
    -> Task<'a>

static member ExecuteScalar
    : cmd : Command<#IScalar<'a>>
    * context : ConnectionContext
    * token : CancellationToken
    -> Task<'a>
```