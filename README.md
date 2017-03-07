[![Build Status](https://travis-ci.org/rspeele/Rezoom.svg?branch=master)](https://travis-ci.org/rspeele/Rezoom)

# What's this?

Rezoom is a library intended to reduce the pain of dealing with data that lives across a latency boundary.

![Horrors of latency](https://raw.githubusercontent.com/rspeele/Rezoom/master/Documentation/Resources/Latency.png)

Common examples of this nightmare include SQL databases, NoSQL databases, and web APIs.

It tends to be hard to write abstractions over these data sources because the round-trip time dominates all other
performance concerns, and it's impossible to optimize without breaking your API.

If somebody is calling `GetUserDetails(userId)` in a loop for 500 different users, you really can't help them other
than by convincing them to switch to `GetMultipleUserDetails(all500userIds)`.

Rezoom lets you write little units of business logic called `Plan`s (like the aforementioned `GetUserDetails`),
which you can then glue together into larger plans. It'll handle converting the 500 independent `GetUserDetails` calls
into one `GetMultipleUserDetails` call. It also de-duplicates redundant requests: for example if multiple functions
need to query for the current user's permissions, only one such query will actually be executed.

# Show me an example so I know whether to care

Here's some contrived example code. You can write a Rezoom wrapper library for any data source, but it comes with
one called Rezoom.SQL that statically typechecks SQL and infers its caching behavior.

```fsharp
type GetPerson = SQL<"select * from People where Id = @id">
type GetCompany = SQL<"select * from Companies where Id = @id">

/// Gets a person and their employer. This implementation takes 2 round trips.
let getPersonAndEmployer (personId : int) : (GetPerson.Row * GetCompany.Row) Plan =
    plan {
        // One round trip.
        let! person = GetPerson.Command(id = personId).ExactlyOne()
        // Another round trip.
        let! employer = GetCompany.Command(id = person.EmployerId).ExactlyOne()
        return (person, employer)
    }

let example =
    plan {
        // Two round-trips: one to get both users, another to get both employers.
        // If the two users have the same employer, the 2nd round-trip will only include one SELECT statement.
        let! (user1, employer1), (user2, employer2) =
            getPersonAndEmployer 1, getPersonAndEmployer 2
        printfn "%s is employed by %, %s by %s"
            user1.Name, employer1.Name
            user2.Name, employer2.Name

        // Two more round trips: again, the user queries all share one, and the employer queries share the next.
        for id in batch [ 3; 4; 5; 6 ] do
            let! user, employer = getPersonAndEmployer id
            printfn "%s is employed by %s" user.Name, employer.Name
    }

```

Pretty neat, huh? How about this?

```fsharp
type UpdateCompanies = SQL<"update Companies set LastUpdatedUtc = SysUtcDateTime() where Id = 27">

let example2 =
    plan {
        // Two round-trips: one for the user, one for the employer.
        let! (user1, employer1) =
            getPersonAndEmployer 1

        // Zero round trips, because both query results are cached.
        let! (user1Again, employer1Again) =
            getPersonAndEmployer 1

        // Invalidates the cache for all queries dependent on the Companies table.
        // This is automatically deduced by the SQL<"query string"> type provider.
        do! UpdateCompanies.Command().Plan()

        // One round-trip: user is still cached, but we have to reload the employer.
        let! (user1Again, employer1Again) =
            getPersonAndEmployer 1

        return 0
    }
```

"Now, hang on a minute," you might be asking your computer screen.
"Where's this stuff being cached? What if some other code updates a table and now the cache is invalid?"

When you write a `plan` block nothing happens until you execute it with `Execution.execute(plan)`.
The cache is local to that execution, which is also implicitly wrapped in a transaction.
So, the cache is not for keeping information across, say, multiple requests to your website.

It's to eliminate the temptation to explicitly pass around loaded data, cluttering your API in an attempt
to save queries.

# I don't like magic. How's this plan stuff really work?

Rezoom uses (and gets its name from) something called a resumption monad.

Facebook's [Haxl](https://github.com/facebook/Haxl) uses the same concept, but is geared more towards reading data,
while Rezoom is designed to work well with code that updates and creates data, too. Also, Haxl is for the few people
who use Haskell, while Rezoom could potentially be used by dozens of F# developers.

Here's a greatly simplified version of the `Plan` type called `TrivialPlan`. To keep things simple, `TrivialPlan`
is specialized to only work with SQL queries.

```fsharp

// For this example, our queries are just SQL strings
type Query = string

// ... and our query results are sequences of untyped rows (obj arrays).
type QueryResponse = array<obj> seq

// A plan can that will eventually yield a result of type 'a can be one of two things:
type TrivialPlan<'a> =
    // Either it's already completed and carries a result...
    | Done of result : 'a
    // Or it's paused, waiting for a round-trip to run.
    | Pending of RoundTrip<'a>

    // In the latter case we have two things:
and RoundTrip<'a> =
    {   // The queries to run in the round-trip...
        Queries : Query array
        // And a function to get the next step in the plan, once we have the responses to our queries.
        Resume : QueryResponse array -> TrivialPlan<'a>
    }

```

You can probably imagine how you might write a function to execute a plan like this. Assuming you have a library
function to run SQL queries in a batch, it's tiny:

```fsharp
let rec exec (plan : 'a TrivialPlan) =
    match plan with
    | Done x -> x
    | Pending trip ->
        let responses = runBatchOfQueries trip.Queries
        exec (trip.Resume responses)
```

As an optimization you could add a cache for queries, so you don't run the same one more than once.

```fsharp
let rec execWithCache (cache : IDictionary<Query, QueryResult>) (plan : 'a TrivialPlan) =
    match plan with
    | Done x -> x
    | Pending trip ->
        let pending = trip.Queries |> Array.filter (not << cache.ContainsKey)
        let responses = runBatchOfQueries pending |> Array.zip Pending
        for query, response in responses do
            cache.[query] <- response
        let allResponses = trip.Queries |> Array.map (fun q -> cache.[q])
        execWithCache cache (trip.Resume allResponses)
```

Ok, so that covers how plan execution and caching could work.

In Rezoom, it's more complicated. Commands can specify that they aren't cacheable and even that they invalidate the
cached results of other commands. But the basic idea is there.

Now, how could you write the `TrivialPlan`s to feed into your `exec` function?
Well, it's easy to write one that just returns a value:

```fsharp
let ret (value : 'a) : 'a TrivialPlan = Done value
```

How about one that wraps a query?

```fsharp
let query (sql : Query) : QueryResult TrivialPlan =
    {   // We just have the one query to run.
        Queries = [| sql |]
        // Given our expected single query result, say we don't have a next step and we're done here.
        Resume = fun [| result |] -> Done result
    } |> Pending
```

To write real code though, we need to run multiple queries. We can sequence together actions by "binding" the result
of one plan as the input to generate the next plan.

```fsharp
let rec bind (first : 'a TrivialPlan) (next : 'a -> 'b TrivialPlan) =
    match first with
    // If the first plan is done, we can use its result to move onto the next.
    | Done x -> next x
    // Otherwise, we have to wrap the first plan's pending step, so that upon resuming execution,
    // we can check again to possibly proceed to the next step.
    | Pending step ->
        {   Queries = step.Queries
            Resume = fun responses -> bind (step.Resume responses) next
        } |> Pending
```

Now we can write fancy business logic like this:

```fsharp
bind (query "select * from Users where Id = 1")
    (fun users ->
        bind (query "select * from Groups where Id = 1")
            (fun groups ->
                printfn "Users: %A; Groups: %A" users groups
                ret (users, groups)
            )
    )
```

This ugly chain of nested functions is roughly what F# computation expressions translate to.
This means we could rewrite the above like so -- much more readable!

```fsharp
trivialplan {
    let! users = query "select * from Users where Id = 1"
    let! groups = query "select * from Groups where Id = 1"
    printfn "Users: %A; Groups: %A" users groups
    return (users, groups)
}
```

Now, so far, we don't have a way to share round-trips between plans. However, since a plan can carry
an array of queries, not just one, we can combine any two plans into one, which'll execute both side-by-side.

```fsharp
let rec sideBySide (a : 'a TrivialPlan) (b : 'b TrivialPlan) : ('a * 'b) TrivialPlan =
    match a, b with
    // If they're both done, tuple up the results.
    | Done gotA, Done gotB -> Done (gotA, gotB)
    // If only one is done, just keep working on the next one, but remember to
    // include the first result when the other finishes.
    | Done gotA, b -> bind b (fun gotB -> ret (gotA, gotB))
    | a, Done gotB -> bind a (fun gotA -> get (gotA, gotB))
    // If they're both pending...
    | Pending pendA, Pending pendB ->
        {   // Include the queries for both in a single round-trip.
            Queries = Array.append pendA.Queries pendB.Queries
            // When we get responses, resume both plans and keep on running them side-by-side.
            Resume =
                fun responses ->
                    let responsesA = Array.sub responses 0 pendA.Queries.Length
                    let responsesB = Array.sub pendA.Queries.Length pendB.Queries.Length
                    sideBySide (pendA.Resume responsesA) (pendB.Resume responsesB)
        } |> Pending
```

Notice that if the two plans have multiple steps, they'll take turns stepping forward, so the code of the second one
will be executing interleaved with the code of the first one. This means this approach is only suitable when the two
plans don't need to be run in any particular order.

With Rezoom, something like the above is used when you bind two variables at the same time, like in
`let! x, y = getX, getY`.

Everything gets more complicated in real life because we also have to support user-defined exception handlers,
`finally` blocks, loops, and beyond. But the simplified plan functions described here are close enough to use as a mental
model. Most importantly, they take the magic out of `plan` blocks and help you understand what they're capable of.