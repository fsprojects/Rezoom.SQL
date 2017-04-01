# Expressions

Expressions are the most complex part of RZSQL's syntax. An expression
represents a computation that produces a **scalar value**, that is, a value of
some [data type](DataTypes.md) that could be stored in a table column.

The full syntax diagram for `expr` is rather daunting, but may serve as a useful
reference.

### _expr_

{% include "Diagrams/Expr.svg" %}

## The simplest expressions

Sprinkled onto the spaghetti of the above diagram are some tasty meatballs of
simplicity. These are the best starting points for getting a handle on
expressions, so they're right at the top.

#### Literals

A [literal](Literal.md) is just a plain value like `1` or `'Jeff'`.

#### Bind parameters

A **bind parameter** lets you pass a value from your program into your SQL
command. Bind parameters are like any other [names](Name.md), but are prefixed
with an `@` sign.

The [data type](DataTypes.md) of a bind parameter is inferred from its usage,
much like F# variable types are inferred.

#### Column names

Within some contexts, such as a [select statement](SelectStmt.md), you can
access table columns by [name](Name.md) from within an expression.

