# Select Statements

A **select statement** is used to retrieve data from the database and return it
to the application as a **result set**.

Every select statement produces exactly one result set.

If you have any familiarity with SQL, you have probably encountered select
statements before. An example of a select statement is below:

```sql
select usr.Id, usr.Name, com.CommentText
from Users as usr
join Comments as com on com.UserId = usr.Id
where usr.Id = @id
```

The syntax for a select statement is fairly complicated in total, but breaks out
nicely into understandable pieces. The best place to start is with the syntactic
element called _select-core_, which actually contains the keyword `SELECT`.

### _select-core_

{% include "Diagrams/SelectCore.svg" %}

## The `SELECT` clause

The first thing you must specify in a select statement is a list of columns it
should output. This is an unfortunate decision in SQL's design. However, RZSQL
does not rock the boat when it comes to unfortunate SQL design decisions.

Each element in the select list is called a _select-property_ and may be one of
the following:

* `*`, meaning all columns available from the `FROM` clause. RZSQL expands this
  at compile time to an explicit list of columns.

* `tableName.*`, meaning all columns in the table `tableName` from the `FROM`
  clause. RZSQL expands this at compile time to an explicit list of columns.

* An [expression](Expr.md), optionally including a column name to use in the
  result set. The column name is implicit if the expression is itself a
  _column-name_.

* A [navigation property](NavigationProperties.md).

### _select-property_

{% include "Diagrams/SelectProperty.svg" %}

## The `FROM` clause and _table-expr_

Most select statements are written to retrieve data from table(s). These tables
are specified in the `FROM` clause as a _table-expr_.

### _table-expr_

{% include "Diagrams/TableExpr.svg" %}

The simplest case for a _table-expr_ is right along the top of this diagram. It
is an [_object-name_](Name.md) referring to a table or view from which to select
columns, with an optional alias name. If an alias name is supplied, you can use
it when referring to the table's columns from within expressions, as in the
example from the top of this page.

Notice that you can also use a subquery wrapped in parentheses instead of a
table or view name. A subquery can be a full _select-stmt_, which is a little
more complex than a _select-core_. We'll get to that shortly.

#### Joins

A _table-expr_ can also represent a
[join](https://en.wikipedia.org/wiki/Join_(SQL)) between tables.

RZSQL supports just two types of joins.

* Inner joins (written as `JOIN`, `INNER JOIN`, or just `,`)
* Left outer joins (written as  `LEFT JOIN` or `LEFT OUTER JOIN`)

A join optionally (but usually) includes a condition, written in the `ON`
clause. This should be a boolean [expression](Expr.md) involving columns from
the tables being joined.

Typical usage of an inner join is to join a child table to its parent:

```sql
select *
from Employees e
join Department d on d.Id = e.DepartmentId
```

Typical usage of a left outer join is to join a parent table to its children.
The difference here is that the first query will not return any rows for
departments with no assigned employees, while the second query will return a
single row for each such department, with a NULL value in all the employee
columns.

```sql
select *
from Departments d
left join Employees e on d.Id = e.DepartmentId
```

## The `WHERE` clause

Tables tend to have a lot of data, so you had best filter it down to just what
you need before returning it as a result set. The `WHERE` clause serves this
purpose.

The where clause is simply a boolean [expression](Expr.md), which can refer to
columns from any of the tables named in the `FROM` clause.

Example:

```sql
select *
from Departments d
left join Employees e on d.Id = e.DepartmentId
where d.Name like '%resources%'
```

## The `GROUP BY` clause

The `GROUP BY` clause exists to be used in conjunction with aggregate functions
to answer questions like:

> How many employees are in each department?

We can easily get the total number of employees in a single department like so:

```sql
select count(*) as NumberOfEmployees
from Employees
where DepartmentId = @deptId
```

One way to answer this question would be to first get a list of department IDs,
then run the above query once for each department. However, this isn't the most
efficient approach.

Using `GROUP BY` we can get the answer in a single query:

```sql
select d.Id, d.Name, count(*) as NumberOfEmployees
from Departments d
left join Employees e on d.Id = e.DepartmentId
group by d.Id, d.Name
```

When you use `GROUP BY expr1, expr2`, you will get one result row for each
distinct tuple of values found for the expressions you listed.

You cannot reference any other columns directly in your selected expressions.
For example, this query is invalid:

```sql
select
    d.Id
    , d.Name
    , e.Name -- can't reference e.Name because it is not part of the GROUP BY
from Departments d
left join Employees e on d.Id = e.DepartmentId
group by d.Id, d.Name
```

However, you *can* reference columns from outside the GROUP BY within the
context of an aggregate function, because the aggregate function can combine
*all* the rows in the group:

```sql
select
    d.Id
    , d.Name
    , max(e.Salary)
from Departments d
left join Employees e on d.Id = e.DepartmentId
group by d.Id, d.Name
```

RZSQL attempts to validate this rule at compile time so that you can fix broken
queries before trying to run them on a real database. However, its rules may not
match those of the backend exactly, so it may reject some queries your database
would actually consider legitimate. For example, SQLite enforces no rules around
selecting columns not found in the `GROUP BY` and will happily pull values from
an arbitrary row in the group. RZSQL does not let you do this even if you have
specified that your backend is SQLite.

## The `HAVING` clause

The `GROUP BY` clause has an optional `HAVING` sub-clause. This is essentially
the same thing as the `WHERE` clause, except it runs *after* the `GROUP BY`,
while the `WHERE` clause runs before it.

This is confusing, but boils down to these rules:

* A `WHERE` clause can refer to columns that aren't part of the `GROUP BY`
  list, but can't use aggregate functions at all.

* A `HAVING` clause can use aggregate functions, but can't refer to columns that
  aren't part of the `GROUP BY` clause unless it does so within an aggregate
  function.

The following example retrieves the department name and max employee salary
*only for departments whose highest paid employee makes at least 65K/yr*:

```sql
select
    d.Id
    , d.Name
    , max(e.Salary)
from Departments d
left join Employees e on d.Id = e.DepartmentId
group by d.Id, d.Name
having max(e.Salary) >= 65000
```

## Compound expressions

If you have encountered SQL in the wild, you may have noticed some pieces
missing so far. "Where," you might ask, "is the ORDER BY clause? What about
limiting the number of rows in the result set?"

We're getting to that. However, both of those clauses apply to the total result
set, which we might get from gluing together multiple queries into a
_compound-expr_ using **compound operators**.

### _compound-expr_

{% include "Diagrams/CompoundExpr.svg" %}

## The compound operators

The four compound operators supported by RZSQL are:

* `UNION ALL`: the result set contains all rows from both compound terms.

* `UNION`: the result set contains all **distinct** rows from both compound
  terms.

* `INTERSECT`: the result set contains all rows from the left compound term
  **also found in** the right compound term.

* `EXCEPT`: the result set contains all rows from the left compound term **not
  found in** the right compound term.

Compound operators are all left-associative, meaning that `x except y union all
z` groups like `(x except y) union all z`. The parentheses here are purely for
illustration. In RZSQL, you _cannot_ use parentheses around compound exprs to
override their associativity. If you need an associativity other than
left-to-right, you'll need to use subqueries instead.

## The `VALUES` clause

Notice that as an alterantive to a _select-core_, you can write literal rows of
data with a `VALUES` clause. The main use case for this is in the [insert
statement](InsertStmt.md). There, a full _select-stmt_ is permitted, but usually
the data is provided by the client program in a `VALUES` clause rather than
pulled from other tables.

## The complete select statement

### _select-stmt_

{% include "Diagrams/SelectStmt.svg" %}

A select statement always begins with a _compound-expr_. After the whole
compound expression come clauses that control the ordering and truncation of the
total result set.

## The `ORDER BY` clause

This clause is simply a list of [expressions](Expr.md) to sort the results by.
Each can specify whether to sort in ascending or descending order. The default
is ascending.

Be warned that there are some constraints here that RZSQL cannot yet validate at
compile time, which may result in errors at runtime on your database backend if
you don't code carefully.

In particular, if your _compound-expr_ contains compound operators like `UNION`,
you can only order by columns found in the result set. If your _compound-expr_
is a single _select-core_, you can normally order by arbitrary expressions that
are in scope from the `FROM` clause.

## The `LIMIT` clause

This optional clause specifies a maximum number of rows to truncate the result
set to, as an integer [expression](Expr.md). It has its own optional sub-clause
`OFFSET`, which specifies a number of rows to skip *before* applying the limit.
The default `OFFSET` is zero.

Together, these can be used to implement paging. If you want to show 25 rows per
page, the first page of results can be returned with `LIMIT 25 OFFSET 0`, the
second page with `LIMIT 25 OFFSET 25`, the third page with `LIMIT 25 OFFSET 50`,
and so on.

## Special case: SELECT without other clauses

Occasionally you'll write a `SELECT` statement that has no compound operators
and no `FROM`, `WHERE`, `GROUP BY`, or `LIMIT` clause.

The main reason to do this is to return the result of a scalar function. For
example, after inserting a record into a SQLite table, you may want to return
its ID to your program, [like so](http://rzsql.net/#2411C55565AE200FE3895D3AB82B031184A2425B):

```sql
insert into MyTable(x, y) values ('example', 'data');
select last_insert_rowid() as InsertedId;
```

When you write a `SELECT` without any other clauses, RZSQL knows that it will
always return exactly one row, so it generates a command type returning a single
row instead of an `IReadOnlyList` of rows.

Additionally, if the row type only has one column, it will automatically
implement `IScalar<'columnType>`. This allows you to use extension methods like
`ExecuteScalar` on the generated command type for convenience.
