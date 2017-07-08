# Create View Statements

A **view** is just a query with a name. To create one, you use the following
syntax:

### _create-view_

{% include "Diagrams/CreateView.svg" %}

## An example

A common use case for views is "soft delete", where instead of deleting rows,
you set a flag to indicate that the rows should be ignored. This lets you easily
restore the data if it turns out it shouldn't have been deleted after all.

Most of your application should reference the data through a view that filters
out the "deleted" data. [Here's how you could define such a
view](http://rzsql.net/#682C8AEF818A258EE80820307ED3E805716EEDB7)

```sql
create table AllFoos
( Id int primary key autoincrement
, Name string(80)
, IsDeleted bool
);

create view Foos as
    select * from AllFoos
    where not IsDeleted;
```

Be advised that since RZSQL expands the `select *` column wildcard at compile
time, views using `select *` [will not automatically
update](http://rzsql.net/#1CA327B77EAB5C3F3A737F698915B447D433650B) when you add
columns to the table.

You'll have to explicitly drop and recreate the view to update it.


