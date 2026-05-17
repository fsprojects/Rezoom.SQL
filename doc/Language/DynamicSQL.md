<!-- nav-top -->
[Home](../../README.md) &gt; [Language](README.md) &gt; Dynamic SQL

[&larr; Postgres](Quirks/PostgresQuirks.md) | [What's missing? &rarr;](MissingFeatures.md)
<!-- /nav-top -->

# Dynamic SQL

Occasionally it is necessary to generate SQL dynamically.

Rezoom.SQL lets you inject SQL expressions at runtime into an otherwise-static
query, via the `unsafe_inject_raw` erased function. The injected fragments do
_not_ go through the RZSQL typechecker and translator, so they should be
written directly in the dialect of SQL you are targeting with your
[backend](../Configuration/Json.md#backend).

You should avoid dynamic SQL whenever possible, since it is easy to make
mistakes. In particular, you must be careful to avoid SQL injection.

Future additions to Rezoom.SQL are planned to reduce the situations in which
dynamic SQL is necessary and add safer APIs for common use cases like dynamic
filtering and sorting on the output columns of a query.

## Injecting dynamic SQL into a static query

The static query carries the result shape and the parts of the SQL that are
known at compile time. The dynamic fragment slots into a single hole flagged by
`unsafe_inject_raw`. A fragment is an array of `CommandFragment` values,
typically built from the `sql` and `arg` helpers in `Rezoom.SQL.Raw`.

```fsharp
open Rezoom.SQL
open Rezoom.SQL.Raw

type MyMostlyStaticQuery = SQL<"""
    SELECT Id, Name FROM USERS
    WHERE unsafe_inject_raw(@dynSql)
""">

let exampleCommand (id : int) (nameSearch : string) =
    let predicate =
        [|  sql "Id = "
            arg id
            sql " OR Name LIKE "
            arg ("%" + nameSearch + "%")
        |]
    MyMostlyStaticQuery.Command(dynSql = predicate)
```

The injected fragment is written in your backend's actual syntax, so use
SQLite/T-SQL/Postgres operators and functions directly. Rezoom.SQL won't
translate them.

## Avoiding SQL injection

**NEVER** pass inputs from an untrusted source (e.g. an end user) to the `sql`
function. Doing so lets the user craft inputs that run whatever SQL they want.
This is called [SQL injection](https://www.google.com/search?q=sql+injection)
and is one of the worst vulnerabilities an application can have. It combines
potentially devastating consequences with easy exploitation.

To avoid SQL injection, **ALWAYS** pass user inputs as SQL parameters using the
`arg` or `argOfType` functions. `arg` will guess the `DbType` of the value from
its .NET type; `argOfType` has you specify it yourself.

### WRONG

```fsharp
// DO NOT do it this way!

let exampleCommand (nameSearch : string) =
    let predicate =
        [|  sql "Name LIKE "
            // BAD. DO NOT DO THIS!
            sql ("'%" + nameSearch + "%'") // <-- NO!
        |]
    MyMostlyStaticQuery.Command(dynSql = predicate)
```

### RIGHT

```fsharp
// Use a parameter to pass the untrusted user input.

// Notice that the percent signs aren't surrounded with single quotes anymore,
// because we no longer need to (badly) attempt to convert the input string to
// SQL source code.

let exampleCommand (nameSearch : string) =
    let predicate =
        [|  sql "Name LIKE "
            // GOOD: `arg` makes a bind-parameter at runtime.
            arg ("%" + nameSearch + "%")
        |]
    MyMostlyStaticQuery.Command(dynSql = predicate)
```

---
<!-- nav-bottom -->
[&larr; Postgres](Quirks/PostgresQuirks.md) | [What's missing? &rarr;](MissingFeatures.md)
<!-- /nav-bottom -->

