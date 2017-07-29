# Postgres Quirks

Here are some surprises you may encounter when using RZSQL with Postgres.

## `DateTimeOffset` is half-supported

There is no data type in Postgres that stores a date, time, and timezone offset.

You would think that `TIMESTAMP WITH TIME ZONE` would do that, but it does not.
It stores a UTC timestamp, and annoyingly converts it to different local times
in various situations such as during conversion to `TIMESTAMP WITHOUT TIME
ZONE`. It is baffling to me why anybody would ever want a database to be aware
of anybody's local time, but I'm not the database guy.

When using RZSQL, both the `DateTime` and `DateTimeOffset` types are mapped to
`TIMESTAMP WITH TIME ZONE`. With both types, querying the DB will give you a UTC
value. You should know that **if you put a .NET DateTimeOffset into the
database, only the UTC value will be stored, not the original timezone offset**.

Why is `DateTimeOffset` supported at all by RZSQL on Postgres? Well, it's still
the only type built into the .NET framework that _always_ unambiguously
represents a moment in time (`DateTime` has a pesky `Kind` field that makes it
ambiguous sometimes). So, if you are really trying to represent UTC times and
want to use DateTimeOffset just to avoid the whole `Kind` debacle, I don't want
to stop you. But you should be well aware that on Postgres, you won't actually
store the timezone offset.

## You cannot declare a DESC primary key as part of a column-def

You can specify a `PRIMARY KEY` constraint as part of the definition of a single
column, like so:

```sql
CREATE TABLE Foo(Id int primary key);
```

On Postgres, specifying descending order on this primary key is not supported.

It *is* supported if you add a primary key constraint after the fact, but then
you can't specify the `AUTOINCREMENT` clause. So it's one or the other.

In practice this is not expected to be a problem since ascending/descending
index order doesn't matter for a single-column index.

## REGEXP and MATCH operators are supported

SQLite has a couple operators comparable to the `LIKE` operator, which can be
set up to run some C functions if you want. RZSQL borrowed a lot of SQLite's
syntax, including these operators, `REGEXP` and `MATCH`:

```
SELECT * FROM SomeTable WHERE SomeColumn REGEXP '...';
```

On Postgres, these operators translate like so:

| RZSQL operator | Postgres operator |
|----------------|-------------------|
| REGEXP         | ~                 |
| NOT REGEXP     | !~                |
| MATCH          | SIMILAR TO        |
| NOT MATCH      | NOT SIMILAR TO    |
