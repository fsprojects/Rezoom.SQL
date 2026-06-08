# UserTypes

The UserTypes feature allows you to bring custom .NET data types into RZSQL by pointing the type provider at your own assemblies.

This allows you to:

1. Model your domain better, getting columns typed as `EmailAddress` instead of `string`, `UserId` instead of `Guid`, and so on.
2. Make query result row types implement your interfaces. All your queries against the `User` table can return rows implementing an `IUser` interface you define, so you can write consumer code that works on all of them.
3. Remap built-in types to other storage formats. For example, RZSQL's default handling for DateTime in SQLite is to store an ISO8601 string. If you prefer to store it as an integer Unix time, you can do that with a UserType mapping.
4. Store and retrieve data from backend-specific column types RZSQL doesn't natively support, like Postgres `point` or TSQL `geography`.

## The layout

This is how an example solution with UserTypes is arranged:

![Solution has YourProject.SQLQueries.fsproj referencing Rezoom.SQL.Provider and YourProject.UserTypes.fsproj referencing Rezoom.SQL.Annotations. There is a project reference from YourProject.SQLQueries to YourProject.UserTypes.](SolutionLayout.gv.svg)

Your UserTypes MUST be in a separate assembly from your SQL queries, and must build first.

The type provider cannot "see" types defined in the same assembly it's trying to compile. They don't exist yet!

The fsproj where you're using Rezoom.SQL.Provider must have a project reference to your UserType project(s). It must
**also** name those projects in [rzsql.json's](../../Configuration/Json.md) `"UserTypes"` list. This tells the type
provider to search the listed assemblies at design-time to find your custom types.

Referencing Rezoom.SQL.Annotations is optional. This is a lightweight package that only defines attributes.
Those attributes give you more control over how your custom-mapped UserTypes are translated to SQL.

## Mapping your own primitive types

It's a good practice to model your domain tightly with types. This helps make code self-documenting and allows the
compiler to catch errors where function arguments are passed out-of-order. For example, if you have a function in your domain:

```fsharp
let addUserToGroup (userId : int) (groupId : int) =
    // do stuff
```

It's very easy to accidentally call `addUserToGroup group.Id user.Id` and miss the mistake.

If you have wrapper types and your function signature changes to:

```fsharp
let addUserToGroup (userId : UserId) (groupId : GroupId) =
    // do stuff
```

Then you can't make that mixup without the compiler catching it.

However, implementing a domain model with those wrapper types on top of vanilla RZSQL would be frustrating. You'd
constantly have to convert the raw primitive `int` or `string` or `Guid` values that come out of your SQL query results
to your domain types, and unpack your domain types back to primitives to pass them in as query parameters.

With UserTypes you can solve this. A user-mapped primitive type can take either of the following forms:

### Single-case union

This is the simplest case. Any F# union type with a single case that wraps an underlying [built-in
primitive](../DataTypes.md) will automatically be detected as a valid UserType without needing further annotations or
methods.

```fsharp
// typical single-case DU wrapper pattern
type UserId = UserId of System.Guid

// struct DUs work fine too
[<Struct>]
type FileHash = FileHash of byte[]
```

### ToPrimitive/FromPrimitive static wrappers

This is a more advanced case. Perhaps your type is a little more complicated than a single-case DU wrapper. That's fine,
you can define the mapping directly.

```fsharp
type EmailAddress(rawEmail : string) =
    do
        if isNull rawEmail || not(rawEmail.Contains("@")) then
            invalidArg (nameof rawEmail) "Email must be non-null and contain @"

    override this.ToString() = rawEmail

    static member ToPrimitive(email : EmailAddress) : string = email.ToString()
    static member FromPrimitive(raw : string) : EmailAddress = EmailAddress(raw)
```

`EmailAddress` will be detected as a valid UserType because of the ToPrimitive and FromPrimitive methods mapping it to
string.

If you don't like having those static methods littering your domain, or you can't add them because the type you're
trying to map is from another library you can't edit, that's not a problem!

ToPrimitive and FromPrimitive **do not have to be** declared by the same type that they are mapping.

For example, you can map the BCL type `System.DateOnly` by declaring a static class:

```fsharp
type DateOnlyMapping() =
    static member ToPrimitive(date : DateOnly) : string = date.ToString("o")
    static member FromPrimitive(str : string) : DateOnly = DateOnly.ParseExact(str, "o")
```

Or even a module:

```fsharp
module DateOnlyMapping =
    let ToPrimitive (date : DateOnly) = date.ToString("o")
    let FromPrimitive (str : string) = DateOnly.ParseExact(str, "o")
```

Or my personal preference, F# extension methods:

```fsharp
module MyCustomMappings =
    type DateOnly with
        member this.ToPrimitive() = this.ToString("o")
        static member FromPrimitive(str : string) = DateOnly.ParseExact(str, "o")
```

You can have as many classes as you want defining static custom mappings. But you can't split the mapping for a *single
UserType* across multiple classes. `ToPrimitive : Foo -> string` has to be defined in the *same* class as `FromPrimitive
: string -> Foo` for the mapping to be valid.

## Using the mapped types

Once you've got your UserTypes assembly plugged in via [rzsql.json](../../Configuration/Json.md), you can use your
domain types in your database model. Instead of writing `create table Users(Id guid primary key)`, write `create table Users(Id UserId primary key)`.

**Note that while built-in types in RZSQL are case-insensitive, when you reference a UserType you *must* match its .NET type name exactly, case-sensitively!**

When you `select` from that table, you'll get the `Id` column back out in your F# code as a `UserId`, not just a plain `System.Guid`.

And when your query uses a parameter that you compare with the `Id` column, that parameter will be inferred as a `UserId` as well.

```fsharp
type MyQuery = SQL<"select * from Users where Id = @id">

let someGuid = Guid.Parse("6f626f4e-7964-6957-6c6c-526561644974")

plan {
    // command requires a UserId parameter
    let! row = MyQuery.Command(id = UserId someGuid).ExactlyOne()
    let id = row.Id // type is UserId
    let email = row.Email // type is EmailAddress
    return id, email
}
```

## Row interfaces

Another problem the UserTypes features solves is that RZSQL generates a new row type for *every* SQL query you write.

```fsharp
type QueryUserById = SQL<"select * from Users where Id = @id">
type QueryUserByEmail = SQL<"select * from Users where Email = @email">
```

The above two queries both select all columns from the `Users` table, but they have two different row types,
`QueryUserById.Row` and `QueryUserByEmail.Row`.

Those types are *structurally identical* but they are *nominally different*, so you can't easily write code that works on both.

Unfortunately, there is no good way for the provider to make these return the same row type. Each `SQL<...>` invocation
can only generate types *nested under* itself.

However, with UserTypes we can do the next best thing. We can make the generated types implement *the same interface*.

In your UserTypes assembly, write an interface matching the shape of the columns in the query:

```fsharp
type IUserRow =
    abstract member Id : UserId
    abstract member Email : EmailAddress
    // ... etc
```

Now in your queries, you can specify that you want the resulting row type to implement your `IUserRow` interface.
This is done by changing the `select` to `select<IUserRow>`.

```fsharp
type QueryUserById = SQL<"select<IUserRow> * from Users where Id = @id">
type QueryUserByEmail = SQL<"select<IUserRow> * from Users where Email = @email">
```

As long as the columns specified in the `IUserRow` interface are found in the result set, both `QueryUserById.Row` and
`QueryUserByEmail.Row` will implement `IUserRow`.

Now you can write downstream code to consume that interface, such as mapping `IUserRow` to a DTO type that your web API
returns to clients. You no longer have to deal with duplicating boilerplate mapping code on a bunch of different
basically-identical row types.

If the columns needed to implement the interface are *not* present, you'll get an error **at compile-time**.

You can also declare a query implements *multiple* interfaces by separating with commas:

```sql
select<IUserRow, ISoftDelete, IHasThumbnail, IHaveALotOfInterfaces> * from Users
```

## Controlling field lengths and storage type

In most SQL databases string and binary columns can (and should) have a max length specified.

But when you map a UserType to a `string` or a `byte[]`, by default it will come through without a length specifier.

This means the above examples like the `DateOnly` mapping or the `EmailAddress` mapping would be stored as
`nvarchar(max)` in TSQL.

You can override this by using the `SQLTypeLength` attribute from the `Rezoom.SQL.Annotations` NuGet package.
The attribute can go on the type being mapped...

```fsharp
open Rezoom.SQL.Annotations

[<SQLTypeLength(255)>] // store as nvarchar(255)
type EmailAddress(rawEmail : string) =
    ...

```

...Or on one of the methods doing the mapping:

```fsharp
module MyCustomMappings =
    type DateOnly with
        [<SQLTypeLength(10)>] // store as nvarchar(10)
        member this.ToPrimitive() = this.ToString("o")
        static member FromPrimitive(str : string) = DateOnly.ParseExact(str, "o")
```

A more heavy-handed alternative is to override the entire type name used on the backend.
For example, if you want more compact storage for the 10-char `DateOnly` type, you could make it a `char(10)` instead of `nvarchar`.
This is done with the `RawBackendSQLType` attribute.

```fsharp
    type DateOnly with
        [<RawBackendSQLType("char(10)")>]
        member this.ToPrimitive() = this.ToString("o")
        static member FromPrimitive(str : string) = DateOnly.ParseExact(str.Trim(), "o")
```

Note that `RawBackendSQLType` and `SQLTypeLength` cannot be specified on the same type, because the former completely
overrides the latter and makes it redundant.

The string passed to `RawBackendSQLType` is opaque to RZSQL and not type-checked. It is your responsibility to ensure
that it's syntactically valid and that it can store the data you're mapping into it.

## Re-mapping a builtin type

You can also use UserTypes to change how an already-supported RZSQL primitive type is stored.

This would primarily be useful on SQLite, where the database itself has very few types, so RZSQL made opinionated
decisions on storing GUIDs (as binary BLOBs) and DateTimes (as ISO8601 strings).

If you don't feel those decisions fit your project you can change them the same way you'd override any other UserType:

```fsharp
module ExampleOverrides =
    // change DateTime to store as a unix time instead of an ISO string
    let unixEpoch = DateTime(1970,1,1)
    type System.DateTime with
        member this.ToPrimitive() : int64 = int64 (this - unixEpoch).TotalSeconds
        static member FromPrimitive(i : int64) = unixEpoch + TimeSpan.FromSeconds(float i)

    // change Guid to store as a string instead of a byte[] blob
    type System.Guid with
        member this.ToPrimitive() : string = this.ToString()
        static member FromPrimitive(str : string) = Guid.Parse(str)
```

These overrides will apply everywhere your SQL queries reference the `datetime` and `guid` builtin types. Unlike most
UserTypes, when it's a builtin type you've re-mapped, you *don't* have to be case-sensitive to use it in your schema and
queries. That would just be far too confusing if typing `DateTime` applied your overridden methods but `datetime`
didn't!

### Supporting Decimal and DateTimeOffset on SQLite

Custom mapping can help you with the SQLite backend if you'd like to use `decimal` or `DateTimeOffset`. By default these
types will throw an exception if used with a SQLite backend because I couldn't think of an acceptable *default* way to
support them. For decimal, if we mapped to `REAL` you would lose the precision and base-10 math of `decimal`. The only
lossless way to store and retrieve a `decimal` value would be in a SQLite `BLOB` or `TEXT` column, but then mathematical
operators would break or silently decay to binary floating point.

Likewise with `DateTimeOffset`, the obvious choice would be to use `.ToString("o")` like we do with DateTime, but then
comparisons and equality would produce unexpected results. The below expression evaluates FALSE in SQLite using string
comparison, but should be TRUE comparing the actual moment in time two `DateTimeOffset` types represent. The UTC+0
one is a minute before the UTC-4 one.

`'2026-06-08T01:15:00.0000000+00:00' < '2026-06-07T21:16:00.0000000-04:00'`

If you understand the problem space and have chosen storage format where the tradeoffs work for *your needs*, mapping
these types can be the right call.

## Mapping to vendor-specific database column types

In addition to the aforementioned [built-in primitive](../DataTypes.md) datatypes, your `ToPrimitive` and
`FromPrimitive` methods can map a UserType to `System.Object`.

This allows you to store and retrieve *anything* your underlying ADO.NET provider can handle.

For example, you can map to the `point` type in `Postgres` like so:

```fsharp
[<RawBackendSQLType("point")>]
[<SQLParameterDbType("NpgsqlDbType", 15)>]
type Point2D =
    {   X : double
        Y : double
    }
    static member ToPrimitive(p : Point2D) : System.Object =
        box (NpgsqlTypes.NpgsqlPoint(p.X, p.Y))
    static member FromPrimitive(o : System.Object) : Point2D =
        let pt = o :?> NpgsqlTypes.NpgsqlPoint
        { X = pt.X; Y = pt.Y }
```

When mapping to `System.Object`, the `RawBackendSQLType` attribute is **required**.

Otherwise RZSQL would have no clue what underlying datatype to use on a `Point2D` column!

You'll also notice a new attribute on the above example, `[<SQLParameterDbType("NpgsqlDbType", 15)>]`.

This is used when you write a query that takes a `Point2D` as a *parameter*.

When the RZSQL runtime executes a query with UserType parameters, it first converts them to their underlying
representation via `ToPrimitive`. The output of that `ToPrimitive()` call becomes the
[dbParam.Value](https://learn.microsoft.com/en-us/dotnet/api/system.data.common.dbparameter.value?view=net-10.0).

By default,
[dbParam.DbType](https://learn.microsoft.com/en-us/dotnet/api/system.data.common.dbparameter.dbtype?view=net-10.0)
is set based on the underlying type being mapped to. For example, if you mapped to int, RZSQL will assume
`DbType.Int32` is appropriate.

Usually that is fine.

However, if you are mapping to `System.Object` to represent a custom type, RZSQL's guess of `DbType.Object` might not work with your ADO.NET provider.

In this case the correct thing to do, knowing that the `DbParameter` is specifically an instance of
[NpgsqlParameter](https://www.npgsql.org/doc/api/Npgsql.NpgsqlParameter.html), is to set `dbParam.NpgsqlDbType <- NpgsqlDbType.Point`.

The attribute here gives the runtime the information it needs to do that via reflection. The runtime doesn't carry an
Npgsql dependency and doesn't directly know about those data types, but it essentially does this:

```fsharp
let prop = dbParam.GetType().GetProperty(propName, BindingFlags.Instance|||BindingFlags.Public)
prop.SetValue(dbParam, Enum.ToObject(prop.PropertyType, intValue))
```

In the above snippet, `propName` and `intValue` come from the `[<SQLParameterDbType("NpgsqlDbType", 15)>]` attribute, 15
being the integer value of NpgsqlDbType.Point.

### Writing SQL dealing with backend-specific types

The above example helped you store a `point` and retrieve it, but you still can't do much with it in your database
queries. RZSQL doesn't know what operations `point` supports, and doesn't have type signatures for Postgres's geometric
functions, because they don't fit into its default backend-agnostic type hierarchy. For doing more than just CRUD
storage and retrieval, you'll want to get familiar with [VENDOR statements](../VendorStmts.md).

## Annotation attributes reference

### RawBackendSQLType

Usage: `[<RawBackendSQLType(sqlType : string)>]`

Specifies the literal type RZSQL should use for columns storing this UserType and in typename-carrying expressions like `CAST(x AS MyUserType)`.
This allows you to override the default storage format RZSQL would use for the underlying primitive type.

You SHOULD include the length specifier, if one is needed, in the string such as `"varchar(50)"`.

You SHOULD NOT include nullability information like `"varchar(50) NOT NULL"` in the string. RZSQL will already add
nullability annotations where appropriate so this would generate redundant, invalid syntax.

The `RawBackendSQLType` attribute is REQUIRED if the data type you map To/From is `System.Object`.

### SQLTypeLength

Usage: `[<SQLTypeLength(length : int)>]`

Specifies the maximum length for a UserType mapped to string (`nvarchar(N)`) or byte[] (`varbinary(N)`).

Not valid to combine this with `RawBackendSQLType`, since that already includes a length.

### SQLParameterDbType

Constructor 1: `[<SQLParameterDbType(dbType : System.Data.DbType)>]`

Constructor 2: `[<SQLParameterDbType(dbParameterPropertyName : string, value : int)>]`

Specifies the `DbType` to use when this UserType is passed into a query as a parameter.

You can change to a different `DbType` using the first constructor, like `[<SQLParameterDbType(DbType.Xml)>]`.

For advanced use cases where the standard `DbType` set is not sufficient and you need to set a different integer-valued
property on the ADO.NET provider's implementation of `DbParameter`, you can use the second constructor. The property
will be resolved by name at runtime and set to the specified integer value.

## Pitfalls and limitations

### Must map each specific type, not just a base type

If you have ToPrimitive and FromPrimitive defined on a base class, that does *not* automatically make all its subclasses valid UserTypes. Each one needs its own mapping.

### No chaining primitives

Your `ToPrimitive` and `FromPrimitive` must map *directly* to a builtin primitive type, not *another* wrapper type.

You can't have `Foo` mapped to underlying type `Bar`, `Bar` mapped to `Baz` and `Baz` mapped to `int`. Even though it
would be possible to follow that chain of wrappers and unwrappers to convert between `Foo` and `int`, RZSQL does not do
this. It would require additional error-checking to prevent cyclical paths and it would just make the mapping of `Foo`
harder to follow for any reader of your code.

### No generics

You cannot map a .NET generic type as a UserType. For example, maybe every entity in your domain has a Guid PK. You
might wish to write a single `type Id<'a> = Id of Guid` and then use `Id<User>`, `Id<Group>`, etc. instead of defining
individual types for each one. This is not supported. You'll have to use `type UserId = UserId of Guid` and `type
GroupId = GroupId of Guid` and so on.

### Changes affecting schema

When you change your UserTypes library, RZSQL has no way of knowing about the history.

Suppose for a long time you had `System.TimeOnly` mapped to a `string` (hh:mm:ss) and you have decided to change it to map
to an `int` (seconds since midnight). It's a small task to change your .NET assembly to replace the `ToPrimitive` and
`FromPrimitive` methods, but there is still data in your DB with the old string type.

As far as RZSQL is concerned, it has no idea. One of your migrations from a year ago said `create table Foo(TimeOfDay TimeOnly)`.
That migration created an `nvarchar` column in your SQL Server database and there's live data in there.

Now that you've changed the `TimeOnly` mapping, RZSQL thinks that old migration made an `int` column and always has. The
existence of the `nvarchar` column has been memory-holed: we have always been at war with Eastasia. Your queries will
fail at runtime because RZSQL's idea of your database model no longer matches reality.

The solution is to write a new migration and use a [VENDOR statement](../VendorStmts.md) to port data over from the old
format to the new. The vendor statement will allow you to bypass RZSQL's outdated conception of the data types and work
on the real data in the table. Something like:

```sql
// migration to handle changing storage format from string to int
VENDOR tsql {
    // create a new column
    ALTER TABLE dbo.Foo ADD [NewTime] INT;
    // port the data over from the old format
    UPDATE dbo.Foo SET [NewTime] = DATEDIFF(SECOND, 0, CAST([TimeOfDay] AS TIME));
    // drop the old column and swap in the new one
    ALTER TABLE dbo.Foo DROP COLUMN [TimeOfDay];
    EXEC sp_rename 'dbo.Foo.NewTime', 'TimeOfDay', 'COLUMN';
} IMAGINE {
    // nothing here, so the typechecker thinks nothing happened
}
```

The key thing to remember here is it is up to you to be disciplined about changing your storage representation!

