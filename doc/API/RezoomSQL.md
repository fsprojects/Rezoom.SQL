# Rezoom.SQL

This namespace contains the type providers defined in `Rezoom.SQL.Provider.dll`,
as well as some common types for representing SQL commands generically.

## The `SQL` provider

This type provider typechecks a command consisting of zero or more RZSQL
statements.

It takes two parameters:

1. The RZSQL command as a string.

2. **(optional)** The relative path to a folder containing your
   [rzsql.json](../Configuration/Json.md).

Most invocations of the type provider should leave the 2nd parameter
unspecified. It defaults to ".", that is, the folder of your F# project. If you
have multiple database models you're dealing with from within a single F#
project, then it makes sense to change this, but otherwise leave it alone.

It's a good idea to use F#'s triple-quoted strings for your SQL command strings.
These can be split across multiple lines and can contain `"` characters.

Example:

```fsharp
open Rezoom.SQL

type ExampleSQL = SQL<"""
    select * from Users where Name = @userName
""">
```

### The `Command` method

The provided type (`MyCommand` in the above example) has one method, `Command`.
This method takes the parameters used in the RZSQL statements. In this example,
it'll take a `userName` parameter of type `string`.

```fsharp
type ExampleSQL = SQL<"""
    select * from Users where Name = @userName
""">

let cmd = ExampleSQL.Command(userName = "example")
```

The parameters will always be in alphabetical order, so you can use positional
arguments if you really want, but for clarity's sake it is highly recommended
that you used named arguments as in the above example.

```fsharp
type ExamplePositionalArguments = SQL<"""
   select * from Groups where Id > @b and Id < @a
""">

// Since parameters are in alphabetical order: @a = 1, @b = 2
// However, this is not very readble! Please use named arguments!
let cmd = ExamplePositionalArguments.Command(1, 2)
```

### The `Command` type

The result of the `Command` method is a `Rezoom.SQL.Command<SomeType>`.
This means it is a SQL command you can execute to get a result of type `SomeType`.

`SomeType` will vary depending on the content of the command. Here are common
examples:

```fsharp
// command produces no data
type NoResultsSQL = SQL<"delete from Users">
let cmd : Command<unit> = NoResultsSQL.Command()

// command produces a list of provided row objects
type OneSQL = SQL<"select * from Users">
let cmd : Command<IReadOnlyList<OneSQL.Row>> = OneSQL.Command()

// command produces a single provided row object
// because it is a SELECT with no other clauses
type OneRowSQL = SQL<"select 1 as A, 2 as B">
let cmd : Command<OneRowSQL.Row> = OneRowSQL.Command()

// command produces multiple result sets
type TwoSQL = SQL<"select * from Users; select * from Groups">
let cmd : Command<ResultSets<IReadOnlyList<TwoSQL.Row1>, IReadOnlyList<TwoSQL.Row2>>> =
    TwoSQL.Command()
```

Each `Command` object has a `ConnectionName` property. This determines the
connection string name it will use (from App.config or Web.config) when
executed. You can call `myCommand.WithConnectionName(newName)` to get a new
`Command` object with a different `ConnectionName`. This is useful if you are
using the same provided SQL command on multiple databases within the same
running program.

In order to actually run the command and obtain its result sets, you should use
the extension methods defined in one of the following namespaces:

* [Rezoom.SQL.Synchronous](RezoomSQLSynchronous.md) to run the command
  immediately (`Command<'a> -> 'a`).

* [Rezoom.SQL.Asynchronous](RezoomSQLAsynchronous.md) to run the command as a
  task (`Command<'a> -> Task<'a>`).

* [Rezoom.SQL.Plans](RezoomSQLPlans.md) to run the command as a Rezoom plan
  (`Command<'a> -> Plan<'a>`).

## The `SQLModel` provider

This type provider gives you programmatic access to the database model resulting
from your migration scripts.

It takes a single optional parameter, which points it at the folder containing
your [rzsql.json](../Configuration/Json.md). This path is resolved relative to
your F# project folder. The default, ".", is correct for most projects. If you
have multiple database models you're dealing with from within a single F#
project, then it makes sense to change this, but otherwise leave it alone.

```fsharp
open Rezoom.SQL

type ExampleModel = SQLModel<".">
```

Since the parameter is optional, you can write the same thing like so. However,
this looks a bit weird and doesn't make it obvious that you're invoking a type
provider:

```fsharp
type ExampleModel = SQLModel
```

### The `Migrate` method

The main reason to use the `SQLModel` provider is to run migrations. The
provided type (`ExampleModel` here) has a static `Migrate` method which takes a
[MigrationConfig](RezoomSQLMigrations.md). For most applications using
`MigrationConfig.Default` from the [Rezoom.SQL.Migrations
namespace](RezoomSQLMigrations.md) is fine.

```fsharp
open Rezoom.SQL.Migrations

[<EntryPoint>]
let main argv =
    ExampleModel.Migrate(MigrationConfig.Default)
```

The exact details of the `Migrate` method vary depending on your database
backend. However, basically it will go through the following steps:

1. Open a database using the using the connection string in your App.config or
   Web.config named matching the `"connectionName"` configuration setting from
   [rzsql.json](../Configuration/Json.md).

2. If it is unable to connect because the database does not exists, attempt to
   create it, then reconnect.

3. Check for a table storing migration history (`__RZSQL_MIGRATIONS`).

4. If the table does not exist, create it.

5. For each migration script that has not been recorded in the migration history
   table, run the script in a transaction and insert a row into the migration
   history table for it.

There is an optional `connectionName` parameter to the `Migrate` method. If
provided, this parameter will be used instead of the "connectionName" from
[rzsql.json](../Configuration/Json.md) to look up the connection string in your
App.config.

