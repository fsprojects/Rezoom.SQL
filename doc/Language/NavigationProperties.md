# Navigation Properties

Navigation properties are a small extension to the SQL language that help you
control how your SQL result sets will be turned into .NET objects.

While SQL result sets are always flat tables, they can represent hierarchies of
objects. When you perform a join in SQL across a one-to-many or many-to-many
relationship, you'll get data for the parent entity duplicated for each of its
children. For example, if your query looks like this:

```sql
select
    c.Id as ProductCategoryId
    , c.Name as ProductCategoryName
    , p.Id as ProductId
    , p.Name as ProductName
from ProductCategories c
join Products p on p.ProductCategoryId = c.Id
```

You may get a result set like this. Notice the duplicated data for the
categories "Components" and "Accessories".

| ProductCategoryId | ProductCategoryName | ProductId | ProductName      |
|-------------------|---------------------|-----------|------------------|
|                 1 | Components          |         1 | Red Bike Frame   |
|                 1 | Components          |         2 | Blue Bike Frame  |
|                 1 | Components          |         3 | Green Bike Frame |
|                 2 | Accessories         |         4 | Red Helmet       |
|                 2 | Accessories         |         5 | Gray Helmet      |

When you write code that processes this result set, often you don't want to deal
with this flat list of five rows. It is frequently more useful to have a list of
two categories, each having its own nested list of products.

You can write code that post-processes a result set to form it into the desired
shape, typically using `Seq.groupBy`. However, it is easier to let RZSQL write
this code for you.

To do this, you annotate the column list of your top-level `SELECT` statement
with **navigation properties**. A navigation property wraps one or more of the
selected columns, and specifies:

1. A cardinality: ONE, OPTIONAL, or MANY
2. A property name for the nested list or object reference

Here's how this looks when applied to the above query:

```sql
select
    c.Id as ProductCategoryId
    , c.Name as ProductCategoryName
    , many Products
        ( p.Id as ProductId
        , p.Name as ProductName
        )
from ProductCategories c
join Products p on p.ProductCategoryId = c.Id
```

Now when you run that query, you'll get two rows in your program instead of
five. The rows won't have `row.ProductId` and `row.ProductName` properties.
They'll have `row.Products`, and you'll find the product properties within those
lists.

Of course, in order to de-duplicate the categories, RZSQL must be able to tell
which rows refer to the same category. By default it does this by looking at
which columns are selected from part of the primary keys of a table. So in this
case, it will use `c.Id as ProductCategoryId` as the key to deduplicate on.

If you rewrote the query so that the product ID was also included outside of the
`MANY` clause, the deduplication key would include that column as well, so you'd
be back to having 5 rows (with each row having a nested list with one object).

```sql
select
    c.Id as ProductCategoryId
    , c.Name as ProductCategoryName
    -- having this PK column here makes it part of the deduplication key
    , p.Id as ExampleBadKey
    , many Products
        ( p.Id as ProductId
        , p.Name as ProductName
        )
from ProductCategories c
join Products p on p.ProductCategoryId = c.Id
```

This automatic deduplication key behavior is designed to work well when you use
table wildcards to generate your select lists, as in this example:

```sql
select c.*, many Products(p.*)
from ProductCategories c
join Products p on p.ProductCategoryId = c.Id
```

A future version of RZSQL may add the ability to manually specify which columns
should be used as deduplication keys.

Note that you can nest multiple layers of these annotations. For example, if you
were querying biological data by [taxonomic
rank](https://en.wikipedia.org/wiki/Taxonomic_rank), you could write code like
this, with lots of nesting.

```fsharp
type GetClasses = SQL<"""
    select
        one ParentPhylum(p.*, one ParentKingdom(k.*))
        , c.*
        , many ChildOrders
            ( o.*
            , many ChildFamilies
                ( f.*
                , many ChildGenera
                    ( g.*
                    , many ChildSpecies(s.*))))
    from Classes c
    join Phyla p on p.Id = c.PhylumId
    join Kingdoms k on k.Id = p.KingdomId
    left join Orders o on o.ClassId = c.Id
    left join Families f on f.OrderId = o.Id
    left join Genera g on g.FamilyId = f.Id
    left join Species s on s.GenusId = g.Id
""">

let showClasses() =
    use context = new ConnectionContext()
    let classes = GetClasses.Command().Execute(context)
    printfn "There are %d classes." classes.Count
    for c in classes do
        printfn "Class %s is in kingdom %s" c.Name c.ParentPhylum.ParentKingdom.Name
        for order in c.ChildOrders do
            for family in order.ChildFamilies do
                for genus in family.ChildGenera do
                    for spec in genus.ChildSpecies do
                        printfn "Wow, these are nested a lot! %s/%s/%s/%s"
                            order.Name family.Name genus.Name spec.Name
```
