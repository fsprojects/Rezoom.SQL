# UserTypes

The UserTypes feature allows you to bring custom .NET data types into RZSQL by pointing the type provider at your own assemblies.

This allows you to:

1. Model your domain better, getting columns typed as `EmailAddress` instead of `string`, `UserId` instead of `Guid`, and so on.
2. Make query result row types implement your interfaces. All your queries against the `User` table can return rows implementing an `IUser` interface you define, so you can write consumer code that works on all of them.
3. Remap built-in types to other storage formats. For example, RZSQL's default handling for DateTime in SQLite is to store an ISO8601 string. If you prefer to store it as an integer Unix time, you can do that with a UserType mapping.
4. Store and retrieve data from backend-specific column types RZSQL doesn't natively support, like Postgres `point` or TSQL `geography`.

## The layout

This is how a solution with UserTypes might look:

![](SolutionLayout.gv.svg)

The user types you wish to use MUST be in a separate assembly from your SQL queries, and must build first.

The type provider cannot "see" types defined in the same assembly it's trying to compile. They don't exist yet!

Referencing Rezoom.SQL.Annotations is optional. This is a lightweight package that only defines attribute classes.
Those attributes give you more control over how your custom-mapped UserTypes are translated to SQL.

## Mapping your own primitive types

STUB

## Mapping externally owned primitive types

STUB

## Row interfaces

STUB

## Mapping to vendor-specific database column types

STUB

## Annotation attributes reference

STUB

## Pitfalls to know about

STUB