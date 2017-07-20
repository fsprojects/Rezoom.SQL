# Insert Statements

An **insert statement** adds rows to a table. There are two syntactic forms this
type of statement can take in RZSQL.

### _insert-stmt_

{% include "Diagrams/InsertStmt.svg" %}

In the first form, which is common to most SQL databases, you specify the column
names you'll be supplying data for, then give the data using a select statement.
This select statement is commonly a `VALUES(...)` clause, when the data is
constant or is coming from the calling process via bind-parameters.

Example:

```sql
insert into Users
( Email
, Name
)
values ('user1@example.com', 'user 1')
    , ('user2@example.com', 'user 2');
```

Sometimes it is useful to copy data between tables though, in which case using a
real query is handy. Refer to [select-stmt](SelectStmt.md) for further information.

## The `ROW` clause

When there are a more than a couple of columns involved, it can be hard to see
which datum in a `VALUES(...)` clause corresponds to which column name in the
table.

To make this easier, RZSQL supports a special syntax for inserting a single row
of data, in which the column names are paired with the values being inserted.

```sql
insert into Users
row
    Email = 'user4@example.com',
    Name = 'user 4';
```

This compiles to a `VALUES(...)` clause.

You can experiment with the various insert statement types
[here](http://rzsql.net/#63DD33DE821DA57BC8E3268EB8FB013441B48B46).