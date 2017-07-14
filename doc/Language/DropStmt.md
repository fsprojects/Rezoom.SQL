# Drop object statements

A drop statement removes an object (which could be a table, view, or index) from
the database schema. Drop statements are not for removing rows from tables --
for that, refer to the documentation on [delete statements](DeleteStmt.md).

### _drop-stmt_

{% include "Diagrams/DropStmt.svg" %}

You must specify the type of object you are removing with a drop statement. If
you specify it incorrectly, RZSQL will yell at you during compilation. For
example, if you write:

```sql
drop table Foo
```

This is valid if and only if `Foo` is a table. If `Foo` is a view, this code
will not compile.

