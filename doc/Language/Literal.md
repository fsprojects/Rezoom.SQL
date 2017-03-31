# Literals

A **literal** is the source code representation of a value. Different [data
types](DataTypes.md) typically have different literal representations.

Examples of literals in F# code include:

```fsharp
"string literals"
123 // integer literals
3.1415 // floating point literals
```

Examples of literals in RZSQL include:

```sql
null
true
false

0x100
256
3.1415

'string literals'
```

This is the complete syntax for literals:

### _literal_

{% include "Diagrams/Literal.svg" %}

Most of this is standard fare for programming languages. However, make note of
the following quirks:

* Binary literals are the same as string literals except that they are prefixed
  with an `x` and can only contain pairs of hexadecimal digits between the
  quotes. An example of a binary literal is `x'DEADBEEF'`.

* There is no provision for escaping arbitrary characters in string literals.
  Single quotes can be escaped by doubling them within the string, but there is
  no backslash syntax to encode control characters like `'\n'`.

* RZSQL has literals for true and false. This is extremely common in programming
  languages at large, but slightly less common among SQL dialects. For instance,
  neither SQL Server nor SQLite have boolean literals (the former has no boolean
  data type at all, and the latter uses integers to represent booleans). RZSQL
  automatically translates these to the appropriate backend representation.

* RZSQL has literals for date and time values. Essentially these just understand
  the [ISO 8601](https://en.wikipedia.org/wiki/ISO_8601) format. An example of a
  `DATETIMEOFFSET` literal is `2017-03-30T00:32:57-04:00`.




