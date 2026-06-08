<!-- nav-top -->
[Home](../../README.md) &gt; [Configuration](README.md) &gt; rzsql.json

[&larr; Configuration](README.md) | [Runtime configuration &rarr;](Configuration.md)
<!-- /nav-top -->

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
  "migrations": ".",
  "usertypes": ["Contoso.IdTypes", "Contoso.Primitives"]
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
By default this is resolved as `ConnectionStrings:{name}` in the host's
`IConfiguration` (e.g. your `appsettings.json`). See [Runtime
configuration](Configuration.md) for details.

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

## UserTypes

_default: `[]`_

This optional setting allows you to bring your own types into RZSQL's type system.

I like to use a lot of little wrapper types in my domain layer.

Instead of `string`, I might have an `EmailAddress` type. Instead of passing around `int` IDs, I like to have `UserId` and `GroupId` and `CompanyId` and so on.

This allows validation rules to live in the type's constructor, it makes methods self-documenting,
and it creates a compiler error if I accidentally call `service.AddUser(userId, companyId)` when that method is supposed to take `(companyId, userId)`.

By default though, RZSQL only understands the handful of built-in SQL primitives described in [Language/Data Types](../Language/DataTypes.md).

If we put our user type definitions in a separate assembly, reference that assembly from the project where our SQL
model+queries live, and add the assembly name to the `"usertypes"` list in rzsql.json, Rezoom SQL can use any user type that wraps a supported underlying primitive.

Reference the assembly full name in rzsql.json like so:

```javascript
  "usertypes": ["MyProduct.MyCustomTypesAssembly"]
```

You must also reference the MyProduct.MyCustomTypesAssembly project from your F# project where you're using Rezoom.SQL.Provider.

The type provider will search the listed assemblies for user types with mappings to primitive types.

A "primitive type" means any of the .NET types listed in the table at the top of [Language/Data Types](../Language/DataTypes.md).

A user type is:

* Any F# single-case union that wraps a primitive type, such as `type UserId = UserId of Guid`. `[<Struct>]` unions are also supported.
* Or, any type `T` for which we find a class with static `ToPrimitive` and `FromPrimitive` methods mapping `T` to and from a primitive type.
* Or, any type `T` for which we find F#-style extension methods `member this.ToPrimitive()` and `static member FromPrimitive(x)` mapping `T` to and from a primitive type.

In the latter two cases, it should be noted that `T` does not HAVE to be a type that you own.

For example, you can write ToPrimitive and FromPrimitive extension methods for `System.TimeOnly` in your UserTypes assembly, and then use `TimeOnly` in your SQL schema.

User-type code:

```fsharp
// simple DU
type UserId = UserId of System.Guid

// custom mapping for a type defined elsewhere
module TimeOnlyMapping =
    type System.TimeOnly with
        member this.ToPrimitive() =
            this.ToString("o")
        static member FromPrimitive(s : string) =
            System.TimeOnly.ParseExact(s, "o")
```

SQL schema:

```sql
create table Employees
( Id UserId primary key
, Name string(80)
, ShiftStarts TimeOnly
, ShiftEnds TimeOnly
);
```

The actual SQL that runs on your database will treat ShiftStarts and ShiftEnds as if you'd written `string`.

Selecting from this table will give you a `TimeOnly` property in your result set rows from the type provider.

Comparing to a parameter will cause that parameter to get a `TimeOnly` type inferred:


```fsharp
type MyQuery = SQL<"""
select * from Employees where ShiftStarts = @t
""">

let usage =
    MyQuery.Command(t = TimeOnly(0, 0, 0))
```

### Caveats and limitations

It is not supported to define a custom primitive that is backed by multiple columns. ToPrimitive and FromPrimitive must convert to a single primitive object! There are no plans to add multi-column primitives in the future.

It is not supported to define custom primitive mappings for a generic type. You cannot have `Id<User>` and `Id<Company>`. All custom-mapped types must be simple non-generic types.

When implementing ToPrimitive you should not return a null, and when implementing FromPrimitive you do not need to handle nulls.
You are defining the mapping for a non-null object. The mapping for a null / None object is always assumed to be null / None and cannot be overridden.

The ToPrimitive and FromPrimitive methods for any single UserType must be defined in the same class. You can't have
`ToPrimitive(x : Foo) : int` in one class and `FromPrimitive(x : int) : Foo` in another class and have it work -- the
assembly search will not detect `Foo` as a mapped UserType.

You must be aware of the underlying representation for usertypes when writing your SQL. For example, your custom type
may override comparison operators, but SQL doesn't know about that, and indeed doesn't know about your custom type at
all. It is erased to the underlying primitive at F# compile-time.

If you write `where ShiftStarts < @t and ShiftEnds > @t`, that comparison will be done on the underlying *string
representation*! With the above example ToPrimitive and FromPrimitive methods using the "o" format string this will
actually work fine, but a different representation might not hold up so well. So choose your underlying representation
carefully and be mindful of how your queries are actually working.

---
<!-- nav-bottom -->
[&larr; Configuration](README.md) | [Runtime configuration &rarr;](Configuration.md)
<!-- /nav-bottom -->

