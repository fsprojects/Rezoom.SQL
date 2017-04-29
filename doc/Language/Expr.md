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

## Parentheses

Parentheses serve two roles in expressions. As in most languages, they can be
used to override the default operator precedence and control the order of
operations. For example, here parentheses are used to force `x` and `y` to be
added before multiplying in `z`:

```sql
(x + y) * z
```

However, parentheses also allow you use a sub-query expected to return a single
value [in an expression](http://rzsql.net/#ED91DE78755E4619957993765695D94BCD058E61):

```sql
select (select Email from Users limit 1) as FirstEmail
```

These are called _scalar subqueries_. Scalar subqueries should always have only
one column, and RZSQL enforces this at compile time. They should also return no
more than one row, but RZSQL cannot verify this, so it is up to you to ensure it.

If a scalar subquery produces multiple rows, the outcome is dependent on your
SQL backend. It may use the first row, or throw an error, or do something
completely unexpected.

## The CAST expression

The `CAST` expression is used to convert values from one [type](DataTypes.md) to
another. For example:

```sql
select CAST('123' as int64) as IntegerColumn
```

As far as RZSQL's type system is concerned, a value of any type can be converted
to any other type using `CAST`. This may fail at runtime depending on your
database backend.

## Operators

RZSQL supports a variety of operators. Operators can be:

* Unary, meaning they are written preceding their single operand as in `NOT x`
* Binary, meaning they are written between their operands, as in `x + y`
* Ternary, meaning they are written with three operands, as in `x between y and z`

All operators are left-associative, meaning that if operator `A` and `B` have
**the same precedence**, then `x A y B z` groups like `(x A y) B z`.

The following table briefly describes each operator in order of precedence, from
highest to lowest.

| Operator   | Type    | Precedence | Example                | Notes                                         |
|------------|---------|------------|------------------------|-----------------------------------------------|
| `COLLATE`  | Binary  |         10 | `'str' COLLATE NOCASE` | Right side is a collation name.               |
| `NOT`      | Unary   |          9 | `NOT true`             | True if operand is false.                     |
| `~`        | Unary   |          9 | `~0xff`                | Integer bitwise complement.                   |
| `-`        | Unary   |          9 | `-2`                   | -x is like x * -1.                            |
| <code>&#124;&#124;</code> | Binary | 8 | <code>abc &#124;&#124; def</code> | String concatenation.         |
| `*`        | Binary  |          7 | `5 * 3`                | Numeric multiplication.                       |
| `/`        | Binary  |          7 | `10 / 2`               | Numeric division.                             |
| `%`        | Binary  |          7 | `5 % 3`                | Integer modulo.                               |
| `+`        | Binary  |          6 | `5 + 3`                | Numeric addition.                             |
| `-`        | Binary  |          6 | `5 - 3`                | Numeric subtraction.                          |
| `<<`       | Binary  |          5 | `32 << 1`              | Integer bitwise left-shift.                   |
| `>>`       | Binary  |          5 | `64 >> 1`              | Integer bitwise right-shift.                  |
| `&`        | Binary  |          5 | `123456 & 0xff`        | Integer bitwise AND.                          |
| <code>&#124;</code> | Binary |  5 | <code>0x00ff &#124; 0xff00</code> | Integer bitwise OR.                |
| `>=`       | Binary  |          4 | `4 >= 4`               | Greater-than-or-equal comparison.             |
| `<=`       | Binary  |          4 | `4 <= 4`               | Less-than-or-equal comparison.                |
| `>`        | Binary  |          4 | `4 > 3`                | Greater-than comparison.                      |
| `<`        | Binary  |          4 | `3 < 4`                | Less-than comparison.                         |
| `=`        | Binary  |          3 | `1 = 1`                | Equality comparison.                          |
| `<>`       | Binary  |          3 | `1 <> 0`               | Inequality comparison.                        |
| `IS`       | Binary  |          3 | `NULL IS NULL`         | Equality comparison (including null).         |
| `IS NOT`   | Binary  |          3 | `1 IS NOT NULL`        | Inequality comparison (including null).       |
| `LIKE`     | Binary  |          3 | `'abc' LIKE '%b%'`     | String glob pattern test.                     |
| `NOT LIKE` | Binary  |          3 | `'a' NOT LIKE '%b%'`   | String glob pattern test (negated).           |
| `IN`       | Binary  |          3 | `1 IN (1,2,3)`         | Membership test. See details below.           |
| `NOT IN`   | Binary  |          3 | `1 NOT IN (2,3)`       | Membership test (negated). See details below. |
| `BETWEEN`  | Ternary |          3 | `5 BETWEEN 1 AND 7`    | Inclusive range test.                         |
| `AND`      | Binary  |          2 | `true AND NOT false`   | Logical AND.                                  |
| `OR`       | Binary  |          1 | `false OR true`        | Logical OR.                                   |

#### IS versus =

The `IS` and `IS NOT` operators are almost the same as `=` and `<>`. However,
there is an important distinction. SQL dialects traditionally implement
[three-valued logic](https://en.wikipedia.org/wiki/Null_(SQL)). This means that
the following expression is **false**:

```sql
NULL = NULL
OR NULL <> NULL
OR 1 <> NULL
```

The `IS` and `IS NOT` operators work the way equality comparisons do in normal
programming languages, so the following expression is **true**:

```sql
NULL IS NULL
AND NOT (NULL IS NOT NULL)
AND 1 IS NOT NULL
```

#### Special operators: `LIKE`

Some operators are actually a bit more complex than described above.

One such operator is `LIKE`, which has a couple of quirks. One is that is
actually a ternary operator whose third operand is optional. The `LIKE` operator
takes an optional `ESCAPE` clause specifying the character to use to escape
special characters in the pattern. The behavior of this clause is dependent on
your SQL backend, but an example usage for SQLite is:

```sql
'string with a % sign' LIKE '%\%%' ESCAPE '\'
```

Additionally, the verbs `MATCH` or `REGEXP` can be used instead of `LIKE`, as
in:

```sql
'test' REGEXP '^te.*'
```

These operators are not supported on all SQL backends. On SQLite they behave as
documented [here](https://sqlite.org/lang_expr.html#like).

#### Special operators: `IN`

Another special operator is `IN`. Its right side, which determines the source of
elements to test for membership in, can be one of three things:

* A table name.
* A subquery (wrapped in parentheses).
* A list of expresssions (wrapped in parentheses and separated by commas).
* A bind parameter.

If it is a bind parameter, that parameter's type will be inferred as an array.
At runtime, RZSQL will handle converting the given array of values to multiple
bind parameters, and the SQL query sent to the server will look something like
`... x IN (@p0, @p1, @p2... @pN) ...`.

#### Ternary operator associativity

Ternary operators are also left-associative. Hopefully there are no realistic
situations where this would come up, but this means that the following
expression:

```sql
1 BETWEEN 2 BETWEEN 3 AND 4 AND 5 BETWEEN 6 AND 7
```

Is parsed as:

```sql
(1 BETWEEN (2 BETWEEN 3 AND 4) AND 5) BETWEEN 6 AND 7
```

## Function invocation

Your SQL backend probably has some functions, like [these for
SQLite](https://sqlite.org/lang_corefunc.html).

RZSQL tries to know about those functions so it can correctly typecheck queries
using them. You can find the list of supported functions for your backend under
the [functions](Functions/README.md) section.

## The CASE expression

The CASE expression lets you write conditionals. This works pretty much the same
way on every SQL backend. There are two forms of CASE expressions.

The most generally useful form looks like this:

```sql
CASE
    WHEN cond1 THEN result1
    WHEN cond2 THEN result2
    ELSE result3
END
```

This is essentially the same thing as the F# expression:

```sql
if cond1 then result1
elif cond2 then result2
else result3
```

The other form of CASE expression has an expression between the CASE keyword and
the first WHEN keyword. In this form, each WHEN clause is implicitly compared to
the initial expression.

So, this expression:

```sql
CASE x
    WHEN value1 THEN result1
    WHEN value2 THEN result2
    ELSE result3
END
```

Is equivalent in effect to this expression:

```sql
CASE
    WHEN x = value1 THEN result1
    WHEN x = value2 THEN result2
    ELSE result3
END
```

## The EXISTS expression

The EXISTS expression takes a subquery on its right side. It returns true if the
subquery yields any rows, false otherwise.