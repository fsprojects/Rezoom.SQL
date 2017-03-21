(*** hide ***)

#r "../packages/Rezoom.0.2.1/lib/net46/Rezoom.dll"
#r "../packages/Rezoom.SQL.Provider.0.2.0/lib/net46/Rezoom.SQL.Compiler.dll"
#r "../packages/Rezoom.SQL.Provider.0.2.0/lib/net46/Rezoom.SQL.Mapping.dll"
#r "../packages/Rezoom.SQL.Provider.0.2.0/lib/net46/Rezoom.SQL.Provider.dll"
#nowarn "193"

(**

# Tutorial

This tutorial will get you up and running with [Rezoom.SQL](https://github.com/rspeele/Rezoom.SQL).
In just a few minutes, you'll be writing statically typed SQL in your program and running it on a SQLite database.

You'll need [Visual Studio 2017](https://www.visualstudio.com/downloads/) -- the free
Community Edition is fine.

Make sure you check the box for "F# language support" in the installer.
If you've already installed VS2017, you can re-run the installer and modify
your installation to include F# language support.

## Creating a new F# project for the tutorial

In Visual Studio, click File -> New -> Project.
On the left side, select Templates -> Other Languages -> Visual F#, and select Console Application.
Make sure you target .NET framework 4.6 or newer.

![screenshot of new project dialog](CreateNewProject.png)

## Installing Rezoom.SQL from NuGet

In your new project, open up the package manager console window (View -> Other Windows -> Package Manager Console)
and type:

```
Install-Package Rezoom.SQL.Provider.SQLite
```

This will add Rezoom.SQL and its dependencies to your project, along with a few files to help you get started.

## Setting up your database model

One of the files automatically added to your project is `V1.model.sql`. Take a look inside and you'll see
some SQL code like this. Give it a quick read so you understand the example model.

*Note: you might want to toggle off SQL -> Intellisense Enabled because it's designed for T-SQL syntax.*

```sql
create table Users
    ( Id int primary key autoincrement
    , Email string(254) unique
    , Name string(64) null
    );

create table Comments
    ( Id int primary key autoincrement
    , AuthorId int references Users(Id)
    , Comment string(512)
    );

create index IX_Comments_AuthorId on Comments
    (AuthorId);
```

Now open up `Program.fs`, the main module of your program. The first thing you'll need to do is create the database
and run that migration script to create your tables. Fortunately, this doesn't take much code.

*)

open Rezoom.SQL
open Rezoom.SQL.Migrations

type MyModel = SQLModel<"."> // find migrations in the project folder, "."

let migrate() =
    // customize the default migration config so that it outputs a message after running a migration
    let config =
        { MigrationConfig.Default with
            LogMigrationRan = fun m -> printfn "Ran migration: %s" m.FileName
        }
    // run the migrations, creating the database if it doesn't exist
    MyModel.Migrate(config)

[<EntryPoint>]
let main argv =
    migrate()
    // return 0 status code
    0

(**

Go ahead and run this program with ctrl+F5. You should see that it outputs V1.model.sql. If you run it again,
it won't output anything, since the database already exists and there's no need to run migrations.

You can find the database it created, named `rzsql.db`, in the `bin/Debug` folder of your project.
If you'd like to take a look around it and see the tables, I recommend using
[DB Browser for SQLite](http://sqlitebrowser.org/).

## Adding some data

You add data to your tables using plain old SQL `INSERT` statements. Try adding this code to your program:

*)

open Rezoom.SQL.Synchronous // extension methods for running commands synchronously

// define a SQL command for inserting a user and getting their ID
type InsertUser = SQL<"""
    insert into Users(Name, Email) values (@name, @email);
    select last_insert_rowid() as id
""">

// define a SQL command for inserting a comment 
type InsertComment = SQL<"insert into Comments(AuthorId, Comment) values (@authorId, @comment)">

let addExampleUser name email =
    // open a context in which to run queries
    use context = new ConnectionContext()
    // insert a user and get their ID
    let userId : int64 =
        InsertUser.Command(email = email, name = Some name).ExecuteScalar(context)
    // add a couple comments by the user
    InsertComment.Command(authorId = int userId, comment = "Comment 1").Execute(context)
    InsertComment.Command(authorId = int userId, comment = "Comment 2").Execute(context)

// edit your main function to call the new insert code
[<EntryPoint>]
let main argv =
    migrate()
    addExampleUser "Test Person" "test@example.com"
    // return 0 status code
    0

(**

You may notice that if you run this program twice, you'll get an error the second time.
**This is because of the unique constraint** on `User.Email`. If you change the email address
in the program, you can run it again. Try editing it to get an email address at runtime from `Console.ReadLine()`.

## Querying for data

So far you've already run a query once: the `select last_insert_rowid() as id` following inserting a user.
But you can use the `SQL<...>` provided type to run all sorts of SQL queries. Let's try getting the list of users.

*)

type ListUsers = SQL<"""
    select * from Users
""">

let showUsers() =
    use context = new ConnectionContext()
    let users = ListUsers.Command().Execute(context)
    printfn "There are %d users." users.Count
    for user in users do
        printfn "User ID %d's email is %s..." user.Id user.Email
        match user.Name with
        | None -> printfn "  and they don't have a name."
        | Some name -> printfn "  and their name is %s." name

// edit your main function to call the new query code
[<EntryPoint>]
let main argv =
    migrate()
    showUsers()
    // return 0 status code
    0

(**

## Using more complex queries

You can use SQL joins, group by, etc. in your queries. The SQL syntax is not yet documented, but is closely based
on [that of SQLite](https://www.sqlite.org/lang.html).

For example, we can join the `Users` table to the `Comments` table to get a list of comments *and* data
about the users who wrote them. This also demonstrates using query parameters. Notice that Rezoom.SQL infers
that the parameter `@name` is a string from the fact that the query concatenates it with other strings.

*)

// find comments by users whose names contain the given substring
type ListCommentsByUser = SQL<"""
    select u.Id, u.Email, c.Id as CommentId, c.Comment
    from Users u
    join Comments c on c.AuthorId = u.Id
    where u.Name like '%' || @name || '%'
""">

let showComments name =
    use context = new ConnectionContext()
    let comments = ListCommentsByUser.Command(name).Execute(context)
    printfn "There are %d comments by users matching the name `%s`." comments.Count name
    for comment in comments do
        printfn "User ID %d:" comment.Id
        printfn " Email: %s" comment.Email
        printfn " Comment ID: %d" comment.CommentId
        printfn " Comment: %s" comment.Comment

// edit your main function to call the new query code
[<EntryPoint>]
let main argv =
    migrate()
    showComments "Test"
    // return 0 status code
    0

(**

## Loading nested data structures

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

*)

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

(**

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

*)

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

(**

## Making changes to your database model

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

*)

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

(**

It's as simple as that. Next time you run your program, you'll notice it runs `V2.articles.sql` as a migration.

As you might imagine, you could write your next migration in `V3.something.sql`. This linear ordering of migrations is
simple and easy, but can be frustrating when you work on features in separate branches, or on a team with other
developers. Rezoom.SQL has a feature called [migration trees](MigrationTrees.html) to help with those situations.
It's beyond the scope of this tutorial, but check it out if you're curious.

## Switching from SQLite to SQL Server

Rezoom.SQL translates its own dialect of SQL to different "backends". Currently only [SQLite](https://www.sqlite.org/)
and MS SQL Server (T-SQL) are supported, with [PostgreSQL](https://www.postgresql.org/) support coming soon.

So far, this tutorial has stuck to SQLite. However, most apps in the .NET ecosystem store their data in SQL Server,
so they use T-SQL. If you're starting a fresh project and want to target T-SQL, it's as easy as installing
[Rezoom.SQL.Provider.TSQL](https://www.nuget.org/packages/Rezoom.SQL.Provider.TSQL/) instead of
[Rezoom.SQL.Provider.SQLite](https://www.nuget.org/packages/Rezoom.SQL.Provider.SQLite/). However, both packages
are just thin wrappers around the [base library](https://www.nuget.org/packages/Rezoom.SQL.Provider/). They don't
actually have any code, they just bundle some default config files and the initial `V1.model.sql`.

You can easily change the config yourself to target a different database backend. Here's how.

There's a file in your project called `rzsql.json`. Open it up and you'll see this:

```javascript
{
  "backend": "sqlite",
  "optionals":  "f#",
  "migrations": ".",
  "connectionname": "rzsql"
}
```

Just change the `"backend"` setting from `"sqlite"` to `"tsql"`. Then rebuild your project.

You may get build errors if you have queries using the `last_insert_rowid()` function. This is because that
is a SQLite function, and doesn't exist in T-SQL. Rezoom.SQL unifies the syntax of SQL queries, but it's not a complete
compatibility layer: the functions available are still determined by the backend. In this case, the T-SQL equivalent
function is `scope_identity()`.

At this point your project should build, but you're not done yet. To be able to actually run the code, you'll need
to edit your `App.config` with connection settings for SQL Server. This part isn't actually Rezoom-specific,
it's standard .NET connection string stuff. However, nobody can remember the details, so here they are.
Open up `App.config`, and you'll find something like this:


```xml
<configuration>
<connectionStrings>
 <add
  name="rzsql" providerName="System.Data.SQLite"
  connectionString="Data Source=rzsql.db"
 />
</connectionStrings>
<system.data>
 <DbProviderFactories>
  <remove invariant="System.Data.SQLite" />
  <add
   name="SQLite Data Provider"
   invariant="System.Data.SQLite"
   description=".NET Framework Data Provider for SQLite"
   type="System.Data.SQLite.SQLiteFactory, System.Data.SQLite"
   />
 </DbProviderFactories>
</system.data>
</configuration>
```

Change the `<connectionStrings>` and `<DbProviderFactories>` sections like so:

```xml
<configuration>
<connectionStrings>
 <add
  name="rzsql" providerName="System.Data.SqlClient"
  connectionString="Data Source=.\SQLEXPRESS;Integrated Security=SSPI;Initial Catalog=rzsql"
 />
</connectionStrings>
<system.data>
 <DbProviderFactories>
  <remove invariant="System.Data.SqlClient" />
  <add
   name="SqlClient Data Provider"
   invariant="System.Data.SqlClient"
   description=".Net Framework Data Provider for SqlServer"
   type="System.Data.SqlClient.SqlClientFactory, System.Data"
  />
 </DbProviderFactories>
</system.data>
</configuration>
```

In the above configuration I am assuming your SQL server is located at .\SQLEXPRESS. If it isn't, change the
`connectionString` attribute accordingly.

*)