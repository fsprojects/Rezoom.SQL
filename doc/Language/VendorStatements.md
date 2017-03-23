# Vendor Statements

Chances are, you'll eventually want to use a feature of your database that Rezoom.SQL doesn't understand yet.
Vendor statements exist for exactly this situation.

## How they work

With a vendor statement, you write your real SQL in one block of code, and it goes directly to your backend.

You have to specify the name of your backend on the vendor block. This is so that if you change backends, you'll get
compiler errors on all your vendor statements written for the old backend.

Finally, you can optionally specify one or more statements in a second block of code, in Rezoom.SQL's dialect. This code
will not be executed, in fact, it will not even appear in your compiled program. Its purpose is only to inform the type
checker of any result sets, model changes, or cache invalidation effects produced by the real code.

```sql
vendor tsql {

    -- This is T-SQL. Rezoom.SQL cannot understand this,
    -- does not look at it and does not care.

    -- This is the code that will actually run on your database.

    exec sp_tables

} imagine {

    -- This is Rezoom SQL. Rezoom.SQL parses and typechecks this
    -- and assumes that the T-SQL above has the same side effects
    -- and result sets.

    select
        '' as TABLE_QUALIFIER
        , '' as TABLE_OWNER
        , '' as TABLE_NAME
        , '' as TABLE_TYPE
        , COALESCE(null, '') as REMARKS
    -- This is here because a SELECT with no FROM, WHERE, or LIMIT clause
    -- would be assumed to return a single row.
    where true
}
```

## Using parameters

Because Rezoom.SQL does not attempt to understand the code in the vendor block, it can't tell if you're using
query parameters in there. You must wrap references to query parameters with extra braces, like so:

```sql
vendor tsql {
    exec sp_tables @table_name = {@table} -- notice extra braces
} imagine {
    select
        '' as TABLE_QUALIFIER
        , '' as TABLE_OWNER
        , '' as TABLE_NAME
        , '' as TABLE_TYPE
        , COALESCE(null, '') as REMARKS
    -- compare the parameter with a string in the imagine clause:
    -- this is just to inform the typechecker that it's a string
    where @table = ''
}
```

## The IMAGINE clause

The `IMAGINE` clause of the `VENDOR` statement is optional. You can write code like this:

```sql
vendor sqlite {
    update ExampleTable set ExampleColumn = 1;
}
```

However, if you leave it off, you will suffer in the following ways:

* You will not be able to access the result sets of the statements from your F# code.

* The statements will be assumed to have no effect on your database model. If you create a table in a vendor statement,
  Rezoom.SQL won't know about it unless you tell it to `IMAGINE` it.

* If you are using the commands in `plan` blocks with Rezoom, they will be assumed to not be cacheable and to invalidate
  the cached results for all other queries.

For this reason it is encouraged to use the `IMAGINE` clause in most cases. Exceptions to this rule include things like
creating triggers, for which no equivalent exists in Rezoom.SQL.

## The curly braces

Curly braces are a good choice for `VENDOR` statements, because they aren't found in most SQL dialects.
However, suppose you have to write a string literal containing a `}` in your vendor statement.

```sql
vendor tsql {
    delete top(10) from Comments where Text = '}' -- uh-oh
}
```

Rezoom.SQL does not attempt to parse or even lex the vendor statement body, it just reads text up until the closing
brace. **Therefore, the above example will fail**.

To dodge this problem, Rezoom.SQL allows you to use any sequence of punctuation characters following the keyword
`VENDOR`. The closing delimiter is the backwards version of the punctuation characters used.

For example, here are some possible pairs of opening and closing delimiters:

| Opening delimiter   | Closing delimiter   |
|---------------------|---------------------|
| `{`                 | `}`                 |
| `{?`                | `?}`                |
| `[`                 | `]`                 |
| `<#`                | `#>`                |

So, you could rewrite the above example like so to avoid the delimiter-usurping closing brace:

```sql
vendor tsql <#
    delete top(10) from Comments where Text = '}' -- all good since } isn't our closing delimiter
#>
```

Be aware that this also affects the delimiters you'll use for query parameters and the IMAGINE clause.

```sql
vendor tsql <#
    delete top(<# @count #>) from Comments where Text = '}'
#> imagine <#
    delete from Comments where @count = 123 -- help typechecker figure out @count's type
#>
```
