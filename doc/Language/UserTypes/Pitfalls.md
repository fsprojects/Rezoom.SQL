---
title: Pitfalls and limitations
parent: UserTypes
grand_parent: Language
nav_order: 4
---

<!-- nav-top -->
[Home](../../../README.md) &gt; [Language](../README.md) &gt; [UserTypes](README.md) &gt; Pitfalls and limitations

[&larr; Annotation attributes reference](AttributesReference.md) | [Functions &rarr;](../Functions/README.md)
<!-- /nav-top -->

# Pitfalls and limitations

## Must map each specific type, not just a base type

If you have ToPrimitive and FromPrimitive defined on a base class, that does *not* automatically make all its subclasses valid UserTypes. Each one needs its own mapping.

## No chaining primitives

Your `ToPrimitive` and `FromPrimitive` must map *directly* to a builtin primitive type, not *another* wrapper type.

You can't have `Foo` mapped to underlying type `Bar`, `Bar` mapped to `Baz` and `Baz` mapped to `int`. Even though it
would be possible to follow that chain of wrappers and unwrappers to convert between `Foo` and `int`, RZSQL does not do
this. It would require additional error-checking to prevent cyclical paths and it would just make the mapping of `Foo`
harder to follow for any reader of your code.

## No generics

You cannot map a .NET generic type as a UserType. For example, maybe every entity in your domain has a Guid PK. You
might wish to write a single `type Id<'a> = Id of Guid` and then use `Id<User>`, `Id<Group>`, etc. instead of defining
individual types for each one. This is not supported. You'll have to use
`type UserId = UserId of Guid` and `type GroupId = GroupId of Guid` and so on.

## Changes affecting schema

When you change your UserTypes library, RZSQL has no way of knowing about the history.

Suppose for a long time you had `System.TimeOnly` mapped to a `string` (hh:mm:ss) and you have decided to change it to map
to an `int` (seconds since midnight). It's a small task to change your .NET assembly to replace the `ToPrimitive` and
`FromPrimitive` methods, but there is still data in your DB with the old string type.

As far as RZSQL is concerned, it has no idea. One of your migrations from a year ago said `create table Foo(TimeOfDay TimeOnly)`.
That migration created an `nvarchar` column in your SQL Server database and there's live data in there.

Now that you've changed the `TimeOnly` mapping, RZSQL thinks that old migration made an `int` column and always has. The
existence of the `nvarchar` column has been memory-holed: we have always been at war with Eastasia. Your queries will
fail at runtime because RZSQL's idea of your database model no longer matches reality.

The solution is to write a new migration and use a [VENDOR statement](../VendorStatements.md) to port data over from the old
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

---
<!-- nav-bottom -->
[&larr; Annotation attributes reference](AttributesReference.md) | [Functions &rarr;](../Functions/README.md)
<!-- /nav-bottom -->

