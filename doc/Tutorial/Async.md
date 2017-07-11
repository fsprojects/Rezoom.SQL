(this page is part of [the Rezoom.SQL tutorial](README.md))

# Asynchronous programming

So far we have covered only synchronous invocation of SQL commands. That is, in
the following code example, `command.Execute(context)` blocks the calling thread
until the results are returned from the database:


```fsharp
open Rezoom.SQL
open Rezoom.SQL.Synchronous

type MyQuery = SQL<"select * from MyTable">

let runMyQuery () : IReadOnlyList<MyQuery.Row> =
    use context = new ConnectionContext()
    let command = ListCommentsByUser.Command(name)
    command.Execute(context)
```

This is easy to understand, and perfectly adequate (even ideal) for many
applications.

However, it's a little wasteful. After all, the code is only responsible for
sending the query to your SQL server and processing the results when they
arrive. In between, there's no useful work being done here -- it's just waiting
around.

If you're writing a web application which you expect to get a lot of traffic,
you may want to free that thread up to work on other things while your database
is running the query.

In modern .NET (since 4.5) this is most commonly done with the
`System.Threading.Tasks.Task<'a>` type, which represents asynchronous work that
will eventually result in a value of type `'a`.

You can run a SQL command as a `Task` using the extension methods from
`Rezoom.SQL.Asynchronous`, like so:

```fsharp
open Rezoom.SQL
open Rezoom.SQL.Asynchronous // <-- notice the namespace difference

type MyQuery = SQL<"select * from MyTable">

                  // v-- notice the type difference
let runMyQuery () : Task<IReadOnlyList<MyQuery.Row>> =
    use context = new ConnectionContext()
    let command = ListCommentsByUser.Command(name)
    command.Execute(context)
```

## The task computation expression

Note that when you have a value of type `Task<'a>` and want to do something with
its result (of type `'a`), you **should not** use the `.Result` property. This
will block the calling thread while waiting for the task to finish, defeating
the entire point of using tasks in the first place!

Instead, you can use [the task computation
expression](https://github.com/rspeele/TaskBuilder.fs) that comes with Rezoom,
like so:

```fsharp
open Rezoom.SQL
open Rezoom.SQL.Asynchronous

type MyQuery = SQL<"select * from MyTable">

let runMyQuery () : Task<int> =
    task {
        use context = new ConnectionContext()
        let command = ListCommentsByUser.Command(name)
        // notice that this variable is bound with `let!`
        let! results = command.Execute(context)
        return results.[0].Id
    }
```

## Interop with F# async

F# comes with another type somewhat similar to `Task<'a>` for representing
asynchronous computations. It is called `Async<'a>`.

Rezoom does not come with wrappers for this, but you can convert between the two
types using `Async.AwaitTask` and `Async.StartAsTask`.

## Consider using Rezoom

If you're already going to go to the trouble of writing asynchronous code, it's
worth checking out the [Rezoom](../Rezoom/README.md) tutorial. Rezoom's `Plan`
type is a bit like `Async` or `Task`, but is smart about data dependencies. It
can save you from having to write complex or tightly coupled code to avoid
excessive database round trips.
