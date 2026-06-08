---
title: Annotation attributes reference
parent: UserTypes
grand_parent: Language
nav_order: 3
---

<!-- nav-top -->
[Home](../../../README.md) &gt; [Language](../README.md) &gt; [UserTypes](README.md) &gt; Annotation attributes reference

[&larr; Advanced primitive mapping](AdvancedMapping.md) | [Pitfalls and limitations &rarr;](Pitfalls.md)
<!-- /nav-top -->

# Annotation attributes reference

## RawBackendSQLType

Usage: `[<RawBackendSQLType(sqlType : string)>]`

Specifies the literal type RZSQL should use for columns storing this UserType and in typename-carrying expressions like `CAST(x AS MyUserType)`.
This allows you to override the default storage format RZSQL would use for the underlying primitive type.

You SHOULD include the length specifier, if one is needed, in the string such as `"varchar(50)"`.

You SHOULD NOT include nullability information like `"varchar(50) NOT NULL"` in the string. RZSQL will already add
nullability annotations where appropriate so this would generate redundant, invalid syntax.

The `RawBackendSQLType` attribute is REQUIRED if the data type you map To/From is `System.Object`.

## SQLTypeLength

Usage: `[<SQLTypeLength(length : int)>]`

Specifies the maximum length for a UserType mapped to string (`nvarchar(N)`) or byte[] (`varbinary(N)`).

Not valid to combine this with `RawBackendSQLType`, since that already includes a length.

## SQLParameterDbType

Constructor 1: `[<SQLParameterDbType(dbType : System.Data.DbType)>]`

Constructor 2: `[<SQLParameterDbType(dbParameterPropertyName : string, value : int)>]`

Specifies the `DbType` to use when this UserType is passed into a query as a parameter.

You can change to a different `DbType` using the first constructor, like `[<SQLParameterDbType(DbType.Xml)>]`.

For advanced use cases where the standard `DbType` set is not sufficient and you need to set a different integer-valued
property on the ADO.NET provider's implementation of `DbParameter`, you can use the second constructor. The property
will be resolved by name at runtime and set to the specified integer value.

---
<!-- nav-bottom -->
[&larr; Advanced primitive mapping](AdvancedMapping.md) | [Pitfalls and limitations &rarr;](Pitfalls.md)
<!-- /nav-bottom -->

