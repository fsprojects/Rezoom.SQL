(this page is part of [the Rezoom.SQL tutorial](README.md))

# Adding Migrations

If you're picky about your contrived examples, you may take issue with the model defined in `V1.model.sql`.
After all, in most applications, users don't just post comments into the void. They post comments *on* something
else. Let's say they're commenting on articles posted by other users.

If you want to change the model, you could delete your database file and edit `V1.model.sql`. But since you've already
got some data, how about writing a migration script instead?

Add a file in your project called `V2.articles.sql`. Put the following SQL in there:

```sql
create table Articles
	( Id int primary key autoincrement
	, AuthorId int references Users(Id)
	, Title string(128)
	, Content string(2048)
	);

alter table Comments
	add column ArticleId int null references Articles(Id);
```

Now you can add code to your program referencing the new `Articles` table, and the new `ArticleId` column on
`Comments`.

```fsharp
type GetArticles = SQL<"""
    select
        a.*
        , one Author(aa.*)
        , many Comments
            ( c.*
            , one Author(ca.*))
    from Articles a
    join Users aa on aa.Id = a.AuthorId
    left join Comments c on c.ArticleId = a.Id
    left join Users ca on ca.Id = a.AuthorId
""">

let getArticles() =
    use context = new ConnectionContext()
    let articles = GetArticles.Command().Execute(context)
    for article in articles do
        printfn "%s (by %s)" article.Title article.Author.Email
        for comment in article.Comments do
            printfn "    %s said: %s" comment.Author.Email comment.Comment
```

It's as simple as that. Next time you run your program, you'll notice it runs
`V2.articles.sql` as a migration.

As you might imagine, you could write your next migration in `V3.something.sql`.
This linear ordering of migrations is simple and easy, but can be frustrating
when you work on features in separate branches, or on a team with other
developers. Rezoom.SQL has a feature called [migration
trees](../Configuration/MigrationTrees.md) to help with those situations. It's
beyond the scope of this tutorial, but check it out if you're curious.
