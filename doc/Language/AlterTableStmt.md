# Alter Table Statements

An **alter table** statement is used to edit the schema of a table.

### _alter-table_

{% include "Diagrams/AlterTable.svg" %}

## Core features

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

**On SQLite, that's all you can do**. This is just a fact of life: SQLite
doesn't support dropping columns, adding or removing constraints for existing
columns, or editing column type affinity. For all of those things, in SQLite you
must create a new table and copy your data over.

But on other database backends like `tsql` and `postgres`, you have more
options.

## Constraints

You can add UNIQUE, PRIMARY KEY, FOREIGN KEY, or CHECK constraints to a table:

```sql
ALTER TABLE MyTable
ADD UNIQUE(Column1, Column2);

ALTER TABLE MyTable
ADD CONSTRAINT MyCheckStuff CHECK(Column1 > 0);
```

You can also remove these constraints.

```sql
ALTER TABLE MyTable
DROP CONSTRAINT MyTable_Column1_Column2_UNIQUE;

ALTER TABLE MyTable
DROP CONSTRAINT MyCheckStuff;
```

Note that when a constraint is not explicitly named when it is created, RZSQL
automatically generates a name for it. This allows you to reliably predict the
name regardless of your backend.

The below table shows how constraint names are generated for different
constraint types:

| Constraint                                    | Name                            | Notes                          |
|-----------------------------------------------|---------------------------------|--------------------------------|
| primary key(col1, col2)                       | TableName_col1_col2_PK          | ASC/DESC don't affect the name |
| unique(col1, col2)                            | TableName_col1_col2_UNIQUE      | ASC/DESC don't affect the name |
| check(expr)                                   | TableName_CHECK                 | `expr` doesn't affect the name |
| foreign key(a, b) references OtherTable(c, d) | TableName_a_b_FK_OtherTable_c_d |                                |

## Default values

You can add a default value for a column like so:

```sql
ALTER TABLE MyTable
ADD DEFAULT FOR MyColumn ('this is the default value');
```

You cannot add a default value for a column that already has one. If you want to
change the default value for a column, you should drop the current default value
like so, then add a new one:

```sql
ALTER TABLE MyTable
DROP DEFAULT FOR MyColumn;
```

Notice that, although default values are modeled as constraints on some backends,
it is not possible to specify a custom constraint name for a default value.

This also means you don't need to know the constraint name to drop the default
value, just the name of the column you want to affect.

## Column type, nullability, and collation

Different backends model attributes of columns in different ways. In T-SQL,
nullability is part of the column's type, while in Postgres, it's a special kind
of constraint.

In order to simplify working with different backends, RZSQL model's changes to
column type, nullability, and collation with separate statements. It is not
possible to change both type and nullability in a single `ALTER TABLE` statement
unless you use a [vendor statement](VendorStatements.md).

You change column type like so:

```sql
ALTER TABLE MyTable
    ALTER COLUMN MyColumn string(80);
```

You specify a [_type-name_](DataTypes.md#type-name) just like when adding a new
column, but you can't specify anything else. The nullability and collation the
column already had will be preserved.

To change the nullability of a column simply use `NULL` or `NOT NULL` instead of
a type name.

```sql
-- allow nulls for MyColumn
ALTER TABLE MyTable
    ALTER COLUMN MyColumn NULL;

-- whoops, changed my mind: don't allow nulls
ALTER TABLE MyTable
    ALTER COLUMN MyColumn NOT NULL;
```

Finally, to change the collation of a column, use a `COLLATE` clause instead of
a type name. The nullability and type of the column will be preserved.

It is up to you to be aware of the valid collation names for your database
backend. RZSQL **does not check collation names**.

```sql
ALTER TABLE MyTable
    ALTER COLUMN MyColumn COLLATE SQL_Latin1_General_CP1_CI_AS;
```

## What if I need more?

Your database likely supports more advanced `ALTER TABLE` scenarios than this.
By using [vendor statements](VendorStatements.md) you can run whatever
backend-specific code you want, and tell RZSQL to pretend you're doing something
simpler.

For example, in T-SQL, when you add a nullable column with a default value, you
can specify `WITH VALUES` to have the default value used instead of `NULL` for
all existing rows. RZSQL doesn't support this, so you can fake it with a vendor
statement:

```sql
vendor tsql {
    -- this is what actually runs on the DB...
    alter table Foo
        add NewColumn int
            constraint Foo_NewColumn_DEFAULT_CONSTRAINT
            default(0) with values;
} imagine {
    -- ...this is what RZSQL typechecks and uses to understand
    -- the change you're making to the model.
    alter table Foo
        add NewColumn int null default(0);
}
```