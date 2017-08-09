# Dynamic SQL

Occasionally it is necessary to generate SQL dynamically.

Rezoom.SQL permits running SQL statements specified fully at runtime, or adding
SQL expressions at runtime into an otherwise-static query. These dynamic SQL
expression do _not_ go through the RZSQL typechecker and translator, so they
should be written directly for the dialect of SQL you are targeting with your
[backend](../Configuration/Json.md#backend).

You should avoid using dynamic SQL whenever possible, since it is easy to make
mistakes. In particular, you must be careful to avoid SQL injection.

Future additions to Rezoom.SQL are planned to reduce the number of situations in
which dynamic SQL will be necessary and add safer APIs for common use cases like
dynamic filtering and sorting on the output columns of a query.

## How to use dynamic SQL

The simplest way is to create a fully dynamic `Command`. You do this with the
`dynamicCommand` function, which takes an array of "command fragments". You can
get a command fragment representing a raw SQL string using the `sql` function,
and a command fragment representing a parameter using `arg` or `argOfType`.

An example will illustrate it better than any explanation I could write:

```fsharp
open Rezoom.SQL
open Rezoom.SQL.Raw -- module with helpers for dynamic SQL

type ExampleQueryResult =
    {   Id : int
        Name : string
    }

let exampleCommand (id : int) (nameSearch : string) =
    dynamicCommand<ExampleQueryResult>
        [|  sql "SELECT Id, Name FROM USERS"
            sql " WHERE Id = "
            arg id
            sql " OR Name LIKE "
            arg ("%" + nameSearch + "%")
        |]
```

## Adding dynamic SQL expressions to static SQL queries

You can use the [erased function](Functions/README.md#erased-functions)
`unsafe_inject_raw` on a parameter to pass dynamic SQL via that parameter at
runtime. Again, an example will help:

```fsharp
open Rezoom.SQL
open Rezoom.SQL.Raw

type MyMostlyStaticQuery = SQL<"""
    SELECT Id, Name FROM USERS
    WHERE unsafe_inject_raw(@dynSql)
""">

let exampleCommand (id : int) (nameSearch : string) =
    let exampleSql =
        [|  sql "Id = "
            arg id
            sql " OR Name LIKE "
            arg ("%" + nameSearch + "%")
        |]
    MyMostlyStaticQuery.Command(dynSql = exampleSql)

```

Remember that dynamic SQL does not go through RZSQL translation so you'll need
to use your backend's actual syntax.

## Avoiding SQL injection in dynamic SQL

**NEVER** pass inputs from an untrusted source (e.g. an end user) to the `sql`
function. This allows that user to craft inputs that run whatever SQL they want.
This is called [SQL injection](https://www.google.com/search?q=sql+injection)
and is one of the worst vulnerabilities an application can have. It combines
potentially devastating consequences with easy exploitation.

To avoid SQL injection, **ALWAYS** pass user inputs as SQL parameters using the
`arg` or `argOfType` functions. `arg` will attempt to guess the `DbType` of the
given value based on its .NET type, while `argOfType` has you specify it
yourself.

### WRONG

```fsharp

// DO NOT do it this way!

let exampleCommand (id : int) (nameSearch : string) =
    dynamicCommand<ExampleQueryResult>
        [|  sql "SELECT Id, Name FROM USERS"
            sql " WHERE Name LIKE "

            // BAD. DO NOT DO THIS!
            sql ("'%" + nameSearch + "%'") // <-- NO!
        |]

```

### RIGHT

```fsharp

// Use a parameter to pass the untrusted user input.

// Notice that the percent signs aren't surrounded with single quotes anymore,
// because we no longer need to (badly) attempt to convert the input string to
// SQL source code.

let exampleCommand (id : int) (nameSearch : string) =
    dynamicCommand<ExampleQueryResult>
        [|  sql "SELECT Id, Name FROM USERS"
            sql " WHERE Name LIKE "

            // GOOD: use `arg` function to make a bind-parameter at runtime
            arg ("%" + nameSearch + "%")
        |]

```
