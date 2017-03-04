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
`GetMultipleUserDetails` call. It also de-duplicates redundant requests, for example if multiple functions need to query
for the current user's permissions, only one such query will actually be executed.

# How's that work exactly?

TODO (but seriously it does work)