# T-SQL Quirks

Here are some surprises you may encounter when using RZSQL with T-SQL.

## RZSQL fakes the `IS` and `IS NOT` operators

RZSQL uses `IS` and `IS NOT` to represent equality comparisons that treat `NULL`
as equal to itself. The naming of these operators is borrowed from SQLite.

T-SQL does not have general purpose binary operators suitable for this. You
*can* write `expr IS NULL` or `expr IS NOT NULL` in T-SQL, but these forms only
work for comparison with a literal `NULL` -- you can't, for example, compare two
columns this way.

When faced with a usage of `IS` or `IS NOT` that doesn't have a literal `NULL`
as its right-hand side, RZSQL uses this idiom for `LeftSideExpr IS
RightSideExpr`:

```sql
EXISTS(SELECT LeftSideExpr INTERSECT SELECT RightSideExpr);
```

Surprisingly, SQL Server seems to recognize this idiom and generates a pretty
good query plan for it.

Play with the translation [here](http://rzsql.net/#570A98CDE9CA6837E1CCDA94E721E6B78219B226).

## RZSQL fakes the boolean data type

T-SQL does not have a boolean data type. It has a bit data type, which is
perfect for storing a boolean value, but does not **behave** as a boolean value.
For example, you cannot use a bit column un-adorned within a `WHERE` clause, as in:

```sql
-- this is not valid T-SQL
SELECT * FROM SomeTable WHERE SomeBitColumn
```

All SQL dialects must include boolean expressions, since SQL is near worthless
without its `WHERE` clause. But in T-SQL, those boolean expressions are **only
allowed within the clauses that require them**, such as `CASE`, `WHERE`, and `HAVING`.

You cannot evaluate a boolean expression and get a scalar value out:

```sql
-- this is not valid T-SQL either
SELECT (Column1 < Column2) AS IsLessThan FROM SomeTable
```

RZSQL has a boolean data type and for consistency's sake it's nice to be able to
pretend it works normally, regardless of backend. To facilitate this, the T-SQL
backend for RZSQL fakes the existence of a boolean data type by inserting `<> 0`
whenever a "fake boolean" is used in a boolean clause:

```sql
SELECT * FROM SomeTable WHERE (SomeBitColumn<>0)
```

Conversely, it adds a `CASE` expression whenever a boolean expression such as `x
< y` is used where a scalar value is needed:

```sql
SELECT
    (CAST((CASE WHEN (Column1 < Column2) THEN 1 ELSE 0 END) AS BIT)) AS IsLessThan
FROM SomeTable
```

See the translation for yourself [here](http://rzsql.net/#951399F415CA9780B94B5273B0BF490F153F5EBA).

## LIMIT/OFFSET is not the same as SELECT TOP

If you're familiar with T-SQL, you probably recognize RZSQL's `LIMIT` clause as
being like T-SQL's `SELECT TOP`:

```sql
-- RZSQL
SELECT * FROM USERS LIMIT 1;

-- T-SQL
SELECT TOP 1 * FROM Users;
```

In simple cases this is true. They are equivalent. However, the `LIMIT` clause
has an optional `OFFSET`, which `TOP` does not. If an `OFFSET` is supplied,
`LIMIT` translates to a less-used, newer piece of T-SQL syntax:


```sql
-- RZSQL
SELECT * FROM Example LIMIT 1 OFFSET 1;

-- T-SQL
SELECT * FROM Example OFFSET 1 ROWS FETCH NEXT 1 ROWS ONLY;
```

Is that syntax beautiful or what? You can almost hear the robotic voice.

Sometimes, the OFFSET/FETCH syntax must be used even if there is no `LIMIT` clause.

This is because of a different in the scope of T-SQL's `SELECT TOP` and RZSQL's `LIMIT`.

When tables are smushed together via a
[_compound-expr_](../SelectStmd.md/#compound-expr), `LIMIT` cuts down the total
result set. `SELECT TOP` only trims down the rows from the `SELECT` clause it
was applied to.

So, a query that combines multiple queries with e.g. `UNION ALL` before limiting
the reuslts will always be translated to use an OFFSET/FETCH clause in T-SQL.

You can explore how the `LIMIT` clause is translated
[here](http://rzsql.net/#2FB736BEE2E4F19E40FE4891BB50C69390ABF4A2).

## Must drop default before dropping column

T-SQL models default values as constraints, and refuses to let you drop a column
while it has constraints referencing it, even if the constraints will become
completely pointless once the column is gone.

Therefore, you must `ALTER TABLE DROP DEFAULT FOR ColumnName` before you `ALTER
TABLE DROP COLUMN ColumnName`, if the column has a default value.

You'll get informed of this at compile time.

## No ALTER TABLE RENAME TO

You'll get an exception at compile time if you use this statement.

This clause could perhaps be translated to `sp_rename`, but currently is just
prohibited outright. If you need it, try using a vendor statement with an
`IMAGINE` clause that claims to drop and recreate the table (while actually
running sp_rename and whatever else you need).

## No bitwise shift operators

T-SQL supports bitwise `&` and `|`, but no left or right shifts. You'll get an
exception at compile time if you use the `<<` or `>>` operators.

