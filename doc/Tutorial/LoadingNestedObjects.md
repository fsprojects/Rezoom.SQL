# Loading Nested Objects

One of the annoying things about writing code that uses SQL directly (as opposed to via LINQ) is that SQL
can only produce flat tabular result sets. For example, if you join users to comments, you might get a result set like
this:

| UserId | UserName | CommentId | Comment                       |
|--------|----------|-----------|-------------------------------|
| 1      | Alice    | 1         | Hello, world.                 |
| 1      | Alice    | 2         | This tutorial is fun!         |
| 2      | Bob      | 3         | I'm not having that much fun. |
| 2      | Bob      | 4         | Spam spam spam.               |

This table has 4 rows. But when you work with this data in a program, you probably don't want a flat list of
4 objects. Usually it'd be better to get a list of 2 `User` objects (Alice and Bob), each of which has its own list of
2 `Comment` objects.

Of course, you can write your own code to manually convert the flat list to the nested structure you desire,
de-duplicating user objects by their IDs. However, because this is such a common task, Rezoom.SQL will do it for you.

You just have to tell it which columns should be in nested lists by wrapping them in `many <property name>(...)`. Try
playing around with this code:

```fsharp
type GetUsersWithComments = SQL<"""
    select
        u.Id as UserId, u.Name as UserName,
        many Comments(c.Id as CommentId, c.Comment)
    from Users u
    join Comments c on c.AuthorId = u.Id
""">

let showUsersWithComments() =
    use context = new ConnectionContext()
    let users = GetUsersWithComments.Command().Execute(context)
    printfn "There are %d users." users.Count
    for user in users do
        printfn "User ID %d's name is %A:" user.UserId user.UserName
        printfn "They have written %d comments." user.Comments.Count
        for comment in user.Comments do
            printfn "    Comment: %s" comment.Comment
```

Notice that in the above example, Rezoom.SQL automatically de-duplicates the users. Behind the scenes, it is getting
the same old flat result set from SQL, but it processes it into a nested collection of objects in memory.
In order to do this, it must have some way to de-duplicate the repeated user information. By default, this is done by
comparing all the columns at the user level of the query that are selected from primary key columns -- in this case,
just `u.Id as UserId`.

Try editing the query to no longer select the user ID. You'll get an error from the type provider, saying that it has
no columns to use as keys.

When you use `many Xs(column1, column2, ...)`, the wrapped columns will be in an `IReadOnlyList` called `.Xs`.
This is suitable for one-to-many relationships. For many-to-one or many-to-[0,1] relationship, you can use
`one X(...)` or `optional X(...)`.

You can nest multiple layers of these annotations. For example, if you were querying biological data by
[taxonomic rank](https://en.wikipedia.org/wiki/Taxonomic_rank), you could write code like this, with lots of
nesting.

```fsharp
type GetClasses = SQL<"""
    select
        one ParentPhylum
            ( p.Id
            , p.Name
            , one ParentKingdom
                ( k.Id
                , k.Name))
        , c.Id
        , c.Name
        , many ChildOrders
            ( o.Id
            , o.Name
            , many ChildFamilies
                ( f.Id
                , f.Name
                , many ChildGenera
                    ( g.Id
                    , g.Name
                    , many ChildSpecies
                        ( s.Id
                        , s.Name
                        ))))
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
