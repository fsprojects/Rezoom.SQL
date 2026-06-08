---
title: Field lengths and storage type
parent: UserTypes
grand_parent: Language
nav_order: 1
---

<!-- nav-top -->
[Home](../../../README.md) &gt; [Language](../README.md) &gt; [UserTypes](README.md) &gt; Field lengths and storage type

[&larr; UserTypes](README.md) | [Advanced primitive mapping &rarr;](AdvancedMapping.md)
<!-- /nav-top -->

# Field lengths and storage type

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

---
<!-- nav-bottom -->
[&larr; UserTypes](README.md) | [Advanced primitive mapping &rarr;](AdvancedMapping.md)
<!-- /nav-bottom -->

