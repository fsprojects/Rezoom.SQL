# Delete statements

An **delete statement** permanently removes rows from a table.

Its syntax typically looks like so. Note that rarely-used clauses have been
excluded from this diagram -- see the bottom of this section for a full diagram.

### _delete-stmt_

{% include "Diagrams/DeleteStmtSimple.svg" %}

All of the complexity of a delete statement goes in its `where` clause, which
determines which rows to delete from the table. Rows for which the `where`
expression evaluates to `true` will be deleted.

If no `where` clause is supplied, all rows in the table will be deleted, which
is usually **not** what you want.

## Limited deletes

Occasionally it is useful to delete only a limited number of rows.

Often you can do this with a `where Id in(select Id from ... limit x)`. In
fact, this is the most flexible approach.

However, it is also possible to put a `limit` clause on the `delete` statement
itself, which may perform better on some database backends.

This is the full syntax for the delete statement.

{% include "Diagrams/DeleteStmt.svg" %}
