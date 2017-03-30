# Data Types

Currently RZSQL supports the following data types. These are the types you can
use for columns in `CREATE TABLE` statements, in `CAST` expressions, and the
like.

| RZSQL type     | .NET type             | SQLite type | TSQL type       |
|----------------|-----------------------|-------------|-----------------|
| string(n)      | System.String         | varchar     | nvarchar(n)     |
| string         | System.String         | varchar     | nvarchar(max)   |
| binary(n)      | System.Byte[]         | blob        | varbinary(n)    |
| binary         | System.Byte[]         | blob        | varbinary(max)  |
| bool           | System.Boolean        | integer     | bit             |
| int, int32     | System.Int32          | integer     | int             |
| int16          | System.Int16          | integer     | smallint        |
| int8           | System.SByte          | integer     | tinyint         |
| int64          | System.Int64          | integer     | bigint          |
| float32        | System.Single         | float       | float(24)       |
| float64        | System.Double         | float       | float(53)       |
| decimal        | System.Decimal        | N/A         | numeric(38, 19) |
| datetime       | System.DateTime       | N/A         | datetime2       |
| datetimeoffset | System.DateTimeOffset | N/A         | datetimeoffset  |

The syntax to use when a type name is expected is obvious enough from the first
column from the above table, but is also documented in the following railroad
diagram:

### _type-name_

![railroad diagram](Diagrams/TypeName.svg)

# How types are inferred

RZSQL types are organized in a hierarchy in order to permit things like adding
an `int64` to an `int32`. When the typechecker encounters an expression such as
`a > b`, the types of `a` and `b` are "unified". This results in an error unless
`a` and `b` have the same types or one is an ancestor of the other's type.

A type variable such as `@x` may be unified with types in many places. The most
specific type always wins, so in the expression `@x >= someInt32 and @x <
someInt16`, @x is inferred to have type `int32`, because it is lower in the type
hierarchy than `int16`.

Here is the current type hierarchy. Notice that there are some types in the
hierarchy that do not appear in the above table. These exist just as constraints
so that we can model functions like TSQL's `datalength` that works on both
`binary` and `string` types. It is not possible to reference them in your own
RZSQL code, such as in `CAST` expressions.

![type hierarchy](TypeHierarchy.gv.svg)

# How nullability is inferred

RZSQL makes an effort to infer whether or not an expression's result can be
null. This is important since it generates types to represent the result sets of
SQL expressions, and must use an `option<'a>` instead of an `'a` if a null might
show up in the corresponding column.

Most expressions (like `a * b`) are assumed to be potentially null if _either_
of their inputs are potentially null. When an expression uses a function, the
nullability depends on the function. For example, in TSQL, the `power(base,
power)` function has "infectious" arguments in that if either of them is
nullable, the output is inferred to be nullable. However, the `coalesce`
function's output is only nullable if its last argument is nullable.

When a query is parameterized, RZSQL must also figure out which parameters
should be nullable. To this end, it assumes some expressions must be nullable.

In particular, expressions on both sides of the `IS` and `IS NOT` operators are
assumed to be nullable, because there is no point in using those operators over
`=` and `<>` other than for their behavior in the presence of nulls.

This means that if you write a query like this, `@name` will be inferred
non-nullable:

```sql
select * from Users where Name = @name
```

But if you use the `IS` operator instead, like so, `@name` is inferred to be
nullable, so you'll have to pass `None` or `Some "name"` to the query.

```sql
select * from Users where Name is @name
```

Some function arguments are also assumed to be nullable. All arguments but the
last to the `coalesce` function are assumed nullable. This also applies to some
backend-specific functions, like the SQLite `ifnull` function.

Finally, any expression inserted into a nullable table column is assumed to be
nullable.
