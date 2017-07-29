# rzsql.json

The RZSQL type provider is configured in a file called rzsql.json. If you're
using a Visual Studio project to build an F# assembly, this file must be present
in your project folder. If you're using an F# script file (.fsx), rzsql.json
should be in the same folder as your script.

Currently there are just a few configuration options to set in here. A complete
rzsql.json looks like this:

```javascript
{
  "backend": "tsql",
  "connectionname": "rzsql",
  "optionals":  "f#",
  "migrations": "."
}
```

All options are case-insensitive.

## Backend

_default: `"rzsql"`_

This is the most important option, and in fact for most projects this is the
only one you need to define. The defaults are fine for the others.

The backend setting tells RZSQL what database system you are going to use. This
determines:

* The syntax it'll translate to at compile time (e.g. converting `LIMIT 1` to `SELECT TOP 1`)
* The set of SQL functions (such as `SQRT`, `SUM`, etc.) available to your queries
* The [data types](../Language/DataTypes.md) supported
* The logic used for setting up the migration history table and running migrations

Currently there are four possible values for the `"backend"` setting:

| "backend"  | RDBMS                                     |
|------------|-------------------------------------------|
| "sqlite"   | [SQLite](https://www.sqlite.org/)         |
| "tsql"     | Microsoft SQL Server                      |
| "postgres" | [PostgreSQL](https://www.postgresql.org/) |
| "rzsql"    | None (no translation)                     |

The default "rzsql" backend is never what you want for a real application. It
outputs RZSQL's own syntax, although not necessarily exactly what was written
(for example, `*` wildcards will be expanded at compile time). It also does not
have any SQL functions.

Be aware that the `"tsql"` backend assumes you have SQL Server 2012 or newer,
because it uses the OFFSET/FETCH syntax to translate `LIMIT x OFFSET y` clauses.
If you don't use that clause, it may work on older versions of SQL Server, but
is not tested.

## ConnectionName

_default: `"rzsql"`_

This setting determines the connection string name RZSQL will use at runtime.
This should match the name used in the `<add name="..." .../>` element in the
`<connectionStrings>` section of your program's App.config or Web.config file.

There is usually no reason to change the default.

## Optionals

_default: `"f#"`_

This setting controls what .NET types the type provider will use to represent
values that could be null.

The default setting is to use F#'s [option
type](https://docs.microsoft.com/en-us/dotnet/articles/fsharp/language-reference/options)
for all nullables.

You can set this to `"c#"` to generate types more familiar to C# developers.
When using the `"c#"` setting, reference types like `string` will be left alone,
since they can already be null. Value types like `int` will be wrapped in
`System.Nullable<T>`, a.k.a. `int?` in C# syntax.

I recommend sticking to the default F# style, since in C# style, you cannot tell
whether a string parameter or result set column has been inferred to be nullable
or not. It's better to know when you have to handle the possible null case.

## Migrations

_default: `"."`_

This setting controls the folder where RZSQL will look for migration scripts.
Any file under this folder whose name matches the regex `@"V[0-9]+.*\.sql"`
(case-insensitive) is assumed to be a migration script.

This path is interpreted relative to the folder where rzsql.json is located.
This means that the default is to look for migration scripts in the same folder
as rzsql.json.

Since it is painful to use sub-folders in F# projects, the default setting is
recommended if you want to have your migration scripts be part of your project
file (which is also recommended!).
