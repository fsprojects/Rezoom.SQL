# Common table expressions

Sometimes it is necessary to use the same subquery multiple times within a
complicated query.

A **common table expression**, or CTE for short, is the tool of choice for these
situations.

### _common-table-expression_

{% include "Diagrams/CommonTableExpression.svg" %}

Here's a trivial example:

```sql
with MyCte(x, y)
as (select 1, 2),
MyOtherCte(z)
as (select 3)
select * from MyCte join MyOtherCte on y + 1 = z
```

## Recursive CTEs

You can also write _recursive_ CTEs, like this example [adapted from the SQLite
documentation](https://sqlite.org/lang_with.html):

```sql
WITH RECURSIVE
  xaxis(x) AS (VALUES(-2.0) UNION ALL SELECT x+0.05 FROM xaxis WHERE x<1.2),
  yaxis(y) AS (VALUES(-1.0) UNION ALL SELECT y+0.1 FROM yaxis WHERE y<1.0),
  m(iter, cx, cy, x, y) AS (
    SELECT 0, x, y, 0.0, 0.0 FROM xaxis, yaxis
    UNION ALL
    SELECT iter+1, cx, cy, x*x-y*y + cx, 2.0*x*y + cy FROM m
    WHERE (x*x + y*y) < 4.0 AND iter<28
  ),
  m2(iter, cx, cy) AS (
    SELECT max(iter), cx, cy FROM m GROUP BY cx, cy
  ),
  a(t) AS (
    SELECT group_concat( substr(' .+*#', 1+min(iter/7,4), 1), '')
    FROM m2 GROUP BY cy
  )
SELECT group_concat(rtrim(t),'CRLF') as x FROM a
```

See it [typecheck
here](http://rzsql.net/#7B68353227CCB1A78FEA0172676CE5F3A208C1FA).