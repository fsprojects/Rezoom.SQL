# Update Statements

An **update statement** edits column values within the rows of a table.

Its syntax typically looks like so. Note that rarely-used clauses have been
excluded from this diagram -- see the bottom of this section for a full diagram.

### _update-stmt_

{% include "Diagrams/UpdateStmtSimple.svg" %}

As you can see, you must specify the table to be updated, then one or more
column setter expressions. The expressions can refer to bind parameters or to
the existing column values within the row.

If the statement has a `where` clause, only rows for which the expression
evaluates to `true` will be updated. It rarely makes sense to update every row
in the table so you should usually have this clause!

Unlike that of some database engines, the update syntax in RZSQL does not
support using a "from" clause with joins. For complex update statements you may
need to use subqueries or temp tables.

## Limited updates

Occasionally it is useful to update only a limited number of rows, for example,
updating the top 10 highest performing salespeople to give them a salary
increase.

Often you can do this with a `where Id in(select Id from ... limit 10)`. In
fact, this is the most flexible approach.

However, it is also possible to put a `limit` clause on the `update` statement
itself, which may perform better on some database backends.

This is the full syntax for the update statement.

{% include "Diagrams/UpdateStmt.svg" %}
