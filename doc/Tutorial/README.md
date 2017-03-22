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

```powershell
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

```fsharp
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
```

Go ahead and run this program with ctrl+F5. You should see that it outputs V1.model.sql. If you run it again,
it won't output anything, since the database already exists and there's no need to run migrations.

You can find the database it created, named `rzsql.db`, in the `bin/Debug` folder of your project.
If you'd like to take a look around it and see the tables, I recommend using
[DB Browser for SQLite](http://sqlitebrowser.org/).

## Adding some data

You add data to your tables using plain old SQL `INSERT` statements. Try adding this code to your program:

```fsharp
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
```

You may notice that if you run this program twice, you'll get an error the second time.
**This is because of the unique constraint** on `User.Email`. If you change the email address
in the program, you can run it again. Try editing it to get an email address at runtime from `Console.ReadLine()`.

## Querying for data

So far you've already run a query once: the `select last_insert_rowid() as id` following inserting a user.
But you can use the `SQL<...>` provided type to run all sorts of SQL queries. Let's try getting the list of users.

```fsharp
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
```

## Using more complex queries

You can use SQL joins, group by, etc. in your queries. The SQL syntax is not yet documented, but is closely based
on [that of SQLite](https://www.sqlite.org/lang.html).

For example, we can join the `Users` table to the `Comments` table to get a list of comments *and* data
about the users who wrote them. This also demonstrates using query parameters. Notice that Rezoom.SQL infers
that the parameter `@name` is a string from the fact that the query concatenates it with other strings.

```fsharp
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
```




