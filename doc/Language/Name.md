# Names

Every programming language needs a way to name things like functions and
variables, and every programming language has a slightly different set of rules
for what characters may be used in names.

In RZSQL, the main rules are as follows:

* The initial character must be either an underscore or an alphabet character (A
  through Z, no Unicode nonsense here).

* The following characters, if any, may also be numeric (0 through 9) or dollar signs.

Names that break these rules are permitted as long as they are quoted. RZSQL
supports the quoting syntaxes of most major SQL databases, using backticks,
brackets, or double quotes to wrap names.

### _name_

{% include "Diagrams/Name.svg" %}

Regardless of which name syntax you use in your RZSQL source code, the compiled
code will quote all names using the syntax native to your chosen database
backend. For example, if your [rzsql.json](../Configuration/Json.md) uses the
`"tsql"` backend, all names in your source will end up wrapped in `[brackets]`.

## Object names

An **object name** is the name of a table, view, or index.

Object names are the same as other kinds of names, but can be qualified with a
schema name, as in `temp.MyTable`. Currently RZSQL does not support schema
creation statements, so the only possible schema names are the built-in `main`
and `temp`.

### _object-name_

{% include "Diagrams/ObjectName.svg" %}

## Column names

Much as table names can be qualified by schema names, column names used in
[expressions](Expr.md) can be qualified by table names.

Since table names are object names, this means that a column could include 3
dot-separated parts: `schema.table.column`. However, in practice usually the
table name is just a short alias defined within the `FROM` clause of a select
statement.

### _column-name_

{% include "Diagrams/ColumnName.svg" %}