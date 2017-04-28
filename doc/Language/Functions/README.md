# Functions

This section is dedicated to documenting RZSQL's support for functions.
Currently only scalar functions are supported, meaning they take expressions as
their arguments and return a single value, not a table.

## Type signatures

A function type signature is documented as:

    name(arg1, arg2... argN) -> ret

`arg1` through `argN` and `ret` describe the types of the function's parameters
and return value. These are best explained through examples:

    INT

A value of the INT [data type](../DataTypes.md).

    <numeric>

A value of any data type under the `<numeric>` part of the [data types
tree](../DataTypes.md).

    <numeric>^

Same as above, but the caret indicates that if this argument is inferred to be
nullable, the function's return value will also be inferred to be nullable.

    <numeric>?

The expression given for this argument will be assumed to be nullable. This can
cause bind parameters to be made nullable.

    a

A type variable. All occurences of the same type variable in the function type
signature must be the exact same type. So for example, this function can take
two INTs or two FLOAT64s but not one INT and one FLOAT64:

    example(a, a) -> BOOL

Type variables can also be constrained to a subset of data types, like so:

    <numeric> a

Finally, one of the function's parameters can possibly be repeated. The number
of allowed repetitions is denoted as follows:

   INT{2..}

or with a maximum number of repetitions:

   INT{2..5}

In total, a complex signature could look like this:

   complex_fun(INT, <numeric> a?, a^{1..3}, STRING) -> a

This function takes an int, then a nullable value of some numeric type, then
between 1 and 3 values of the _same_ numeric type, then a string.

It returns a value of the same numeric type used for its second and third (and
fourth, and fifth) arguments. If the third (or fourth, or fifth) argument is
inferred to be nullable, then the returned expression will also be inferred to
be nullable.

Hopefully this gets the idea across well enough to decipher the actual function
signatures.

## Erased functions

If a function is "erased", that means that it will not appear in the SQL code
translated to your backend. Its single argument will be translated and the
surrounding function call removed.

For example, `unsafe_coerce(123)` translates to just `123`.

These functions are only used for their effects on RZSQL's type inference. The
above `unsafe_coerce` function can be used to ignore typechecking on an
expression, because it permits any scalar value as its input and returns a value
of any other scalar type.

Another erased function is `nullable`. This function simply tells the
typechecker that its argument is nullable. An example of where this can be
useful is in the query:

```sql
select * from Table where X is @a + @b
```

The usage of the `is` operator in this query forces its right side to be
nullable. That expression, `@a + @b`, is nullable only if _either_ `@a` or `@b`
is nullable. Since nothing else in the query tells us which one is nullable,
RZSQL will assume both are nullable.

To avoid this, you can use the `nullable` function on one of the bind
parameters, thus explicitly satisfying the requirement that at least one is
nullable.

```sql
select * from Table where X is @a + nullable(@b)
```

This way, only `@b` will be nullable.

