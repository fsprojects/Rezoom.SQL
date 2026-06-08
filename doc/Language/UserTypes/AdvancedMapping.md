---
title: Advanced primitive mapping
parent: UserTypes
grand_parent: Language
nav_order: 2
---

<!-- nav-top -->
[Home](../../../README.md) &gt; [Language](../README.md) &gt; [UserTypes](README.md) &gt; Advanced primitive mapping

[&larr; Field lengths and storage type](FieldLengthsAndStorage.md) | [Annotation attributes reference &rarr;](AttributesReference.md)
<!-- /nav-top -->

# Advanced primitive mapping

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
storage and retrieval, you'll want to get familiar with [VENDOR statements](../VendorStatements.md).

---
<!-- nav-bottom -->
[&larr; Field lengths and storage type](FieldLengthsAndStorage.md) | [Annotation attributes reference &rarr;](AttributesReference.md)
<!-- /nav-bottom -->

