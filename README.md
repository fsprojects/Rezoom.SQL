[![Build Status](https://travis-ci.org/rspeele/Rezoom.svg?branch=master)](https://travis-ci.org/rspeele/Rezoom)

# What's this?

Rezoom is a library intended to reduce the pain of dealing with data that lives across a latency boundary.

![Horrors of latency](https://raw.githubusercontent.com/rspeele/Rezoom/master/Documentation/Resources/Latency.png)

Common examples of this nightmare include SQL databases, NoSQL databases, and web APIs.

It tends to be hard to write abstractions over these data sources because the round-trip time dominates all other
performance concerns, and it's impossible to optimize without breaking your API.

If somebody is calling `GetUserDetails(userId)` in a loop for 500 different users, you really can't help them other
than by convincing them to switch to `GetMultipleUserDetails(all500userIds)`.

Rezoom lets you write little units of business logic (like the aforementioned `GetUserDetails`), which you can then
glue together into larger functions, and it'll handle converting the 500 independent `GetUserDetails` calls into one
`GetMultipleUserDetails` call. It also de-duplicates redundant requests: for example if multiple functions need to query
for the current user's permissions, only one such query will actually be executed.

# How's that work exactly?

Rezoom uses something called a resumption monad, a classic functional programming technique.
Facebook's [Haxl](https://github.com/facebook/Haxl) uses the same concept, but is geared more towards reading data,
while Rezoom is designed to work well with code that updates and creates data, too. Also, Haxl is for the few people
who use Haskell, while Rezoom could potentially be used by dozens of F# developers.

With Rezoom, you write your code in `plan` blocks. If you're familiar with F#'s async workflows, `plan` blocks are
very similar. The trick is that when you bind variables in a single statement, like so, they are assumed to be
independent of each other and can share the same round-trips.

```
plan {
    let! x = planWith2RoundTrips()
    let! y = planWith3RoundTrips()
    // the above would take 5 total round trips
    // ...
    // but the below will only take 3, sharing the first 2 between the plans
    let! x, y = planWith2RoundTrips(), planWith3RoundTrips()
    return { X = x; Y = y }
}
```

You can do the same thing with loops.

```
plan {
    let! userIds = userIdsInGroup groupId
    // "batch userIds" means the iterations of this loop can run concurrently and
    // share round-trips, so the whole loop will only execute one database command
    for userId in batch userIds do
        let! details = getUserDetails userId
        printfn "User %s is in the group" details.UserName
}