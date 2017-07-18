# Introduction to Rezoom

Although Rezoom.SQL can be used by itself, it is designed to work best with its
parent library, Rezoom. Rezoom is a general purpose library for composing tasks
that involve remote data access.

## A motivating example

Consider the following simple example.

```fsharp
open Rezoom
open Rezoom.SQL
open Rezoom.SQL.Synchronous

type GetPermissions = SQL<"select * from UserPermissions where UserId = @userId limit 1">
type DeleteDocument = SQL<"update Documents set DeletedByUserId = @userId where Id = @docId">

let deleteDocument (userId : int) (documentId : int) (conn : ConnnectionContext) =
    let permissions = GetPermissions.Command(userId).ExecuteExactlyOne(conn)
    if not permissions.CanDelete then
        failwith "User does not have permission to delete documents"
    else
        DeleteDocument.Command(docId = documentId, userId = userId).Execute(conn)
```

This code might be OK by itself. It reads pretty clearly. Of course, in a real
system you'd probably put an abstraction around reading the permissions, but
that's just a matter of moving that code behind an interface.

The problem I want to address with this code is what happens when you try to use
it in a perfectly reasonable way:

```fsharp
let deleteManyDocuments userId documentIds conn =
    for documentId in documentIds do
        deleteDocument userId documentId conn
```

This function is very bad! If we pass in 500 document IDs, we'll run 1000 SQL
batches in total -- half of them pointlessly re-querying for the user's
permissions.

One solution would be to move the _real_ implementation into
`deleteManyDocuments`, and make `deleteDocument` the wrapper, instead of the
other way around. This is an easy change to make here, but has its downsides
when you try to use it as the universal solution to this type of problem:

* All callers must also be coded to batch up their document IDs to pass into
  `deleteManyDocuments`, instead of calling `deleteDocument` immediately
  whereever needed.

* The permissions check still isn't shared outside this scope: if higher-level
  code uses, say `deleteManyDocuments` and `deleteManyFoos`, they'll each do
  their own permission check.

* Sometimes the batching logic is harder, obscuring the business logic and
  opening the door to bugs. For example, suppose `deleteManyDocuments` needed to
  accept a list of arbitrary `(userId, docId)` pairs. It would need to group by
  the user ID to avoid duplicate permission queries. This gets more frustrating
  with trickier requirements, such as checks that can be bypassed depending on
  other factors like the status of the document.

Another solution is to say that it's not `deleteDocument`'s responsibility to
check permissions. The caller should check permissions before calling it! This
has an obvious downside of cluttering caller code and being easy to forget. At
*some* level of your system, you'll really want to have a function that combines
the permission check and the action it permitted, and then you're back to this
problem.

Finally, you could write a caching layer for obtaining permissions. This is
probably the best approach so far, but can be a lot of work, especially making
sure the cache gets invalidated correctly when permissions change. It also won't
help at all with the 500 separate `update` commands.

## Solving it with Rezoom

Using Rezoom, you can define `deleteDocument` as a `Plan`. When you see
`Plan<'a>`, think: "some work that will eventually return an `'a`, possibly
dependent on requests to external services".

```fsharp
open Rezoom.SQL.Plans

let deleteDocument (userId : int) (documentId : int) : Plan<unit> =
    plan {
        let! permissions = GetPermissions.Command(userId).ExactlyOne()
        if not permissions.CanDelete then
            failwith "User does not have permission to delete documents"
        else
            do! DeleteDocument.Command(docId = documentId, userId = userId).Plan()
    }

let deleteManyDocuments (userId : int) (documentIds : seq<int>) : Plan<unit> =
    plan {
        for documentId in documentIds do
            do! deleteDocument userId documentId
    }
```

Notice that these functions do not take a connection context. This is because a
`Plan` is a _recipe for how_ to run something. A plan doesn't _do x_, it says
"if I only had a connection, I _could do x_".

Here's an example of how to actually run such a `Plan`:

```fsharp
open Rezoom.Execution
open System.Threading.Tasks

let example() =
    let plan : Plan<unit> = deleteManyDocuments 1 [1..500]
    let task : Task<unit> = execute ExecutionConfig.Default plan
    // Note: only use .Wait() if you want to wait synchronously for the task to finish
    task.Wait()
```

Executing this version of `deleteManyDocuments` will only query for permissions
**once**. This is thanks to the static analysis provided by Rezoom.SQL, which
tells Rezoom:

1. `GetPermissions` doesn't have side effects or use any nondeterministic
   functions like `random`, so its result can be cached for the rest of this
   transaction unless we update the data in the `UserPermissions` table.

2. `DeleteDocument` does have side effects, but doesn't touch the
   `UserPermissions` table, so it doesn't invalidate the cache for
   `GetPermissions`.

The above code will still make 500 round-trips to the database since it runs
the `DeleteDocument` commands one at a time. However, fixing this is also very
simple. You would just change:

```fsharp
for documentId in documentIds do
    ...
```

to:

```fsharp
for documentId in batch documentIds do
    ...
```

Now the function will execute with **two round-trips** to the database: one
containing the permissions query, another containing all the `update` statements
to delete the documents.

## Benefits

This automated caching and batching allows you to write very simple,
self-contained units of business logic which you can compose into much more
complex transactions without incurring massive performance costs.

This is a breath of fresh air compared to typical database work, where to get
acceptable efficiency you usually have to either write your logic in large
chunks with minimal abstraction, or pass around a lot of shared state
explicitly.

If you design carefully, you can end up with a rich domain layer (100s of
methods) that is completely ignorant of the SQL backend, and is built upon a
relatively small (30-50 methods) persistence API. Such a small persistence API
can be worth implementing twice: once with Rezoom.SQL for real world usage, and
once in-memory for integration testing the domain logic.

## Caching and you

If shivers ran down your spine when I mentioned caching, I don't blame you!
Cached answers can be _wrong_. The old adage goes:

> There are two hard things in computer science: cache invalidation, naming
> things, and off-by-one errors.

So it's natural to be suspicious of a "magic" cache, especially when we're
talking to an external database that could be updated by another thread or even
by a program running on another machine somewhere. The automatic cache
invalidation could be perfect for our own code, but how can it know about those
external changes to the data?

It doesn't know and it doesn't want to.

* A `Plan` in Rezoom represents a short-lived chunk of work, which by default
  will be executed within a transaction. It does not represent long-running,
  ongoing tasks like polling loops.

* The cache is local to each Plan's execution. When multiple plans are running
  at the same time (for example, servicing different web requests), they **do
  not** share any cached data.

This means that Rezoom's automatic caching is intended to **simulate you
explicitly passing already-loaded data** around between your functions,
**without cluttering your interfaces with brittle implementation details**.

In fact, at large scale, you might still want to use another layer of caching
for heavily-hit resources so that multiple web requests _can_ share some
caching. It's up to you!
