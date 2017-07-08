# Alter Table Statements

An **alter table** statement is used to edit the schema of a table.

Currently, the `ALTER TABLE` support matches that of SQLite. This is a polite
way of saying it's incomplete. Only the following forms are supported:

### _alter-table_

{% include "Diagrams/AlterTable.svg" %}

You can rename a table with:

```sql
ALTER TABLE MyTableOldName
RENAME TO MyTableNewName
```

Or add columns, like:

```sql
ALTER TABLE MyTable
ADD COLUMN MyNewColumn int null references SomeOtherTable(SomeOtherColumn)
```

And that's it, for now.

# What if I need more?

Quite reasonably, you might want other forms such as:

```sql
ALTER TABLE MyTable DROP COLUMN ObsoleteOldColumn
```

Or:

```sql
ALTER TABLE MyTable ALTER COLUMN OldIntColumn int64
```

Or:

```sql
ALTER TABLE MyTable DROP CONSTRAINT SomeConstraintName
```

Or:

```sql
ALTER TABLE MyTable ADD CONSTRAINT UNIQUE(Column1, Column2)
```

These don't exist in RZSQL largely because they don't exist in SQLite, which was
the starting point for RZSQL's syntax. I plan to add them eventually.

In the meantime, a workaround is to use [vendor statements](VendorStatements.md)
in your migrations, with an `IMAGINE` clause that drops and recreates the table.
The vendor-specific code you write within the `VENDOR` block will run
unadulterated on your database backend, and the `IMAGINE` clause will tell the
typechecker what shape your schema ends up in.
