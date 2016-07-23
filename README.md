# Work in progress

This is a library implementing a resumption monad for .NET, which
allows for the following things to happen:

* Requests to the same data source that don't depend on each other can be automatically batched into a single round-trip (or equivalent expensive operation).

* Idempotent requests for the same data (e.g. current user) in the same execution context (e.g. web API request) can be automatically cached and de-duplicated.

This is similar in effect to Haskell's Haxl.

## What's done so far

* Core library Data.Resumption

  * The fundamental types `Plan<T>` and `IDataEnumerable<T>`.

  * Operations like monadic bind, apply, map, etc. on those types.

  * `ExecutionContext` for running `Plan<T>`s as `Task<T>`s with
    all the caching/deduplication goodness.

  * Mostly XML-commented, but no "big picture" documentation yet.

* F# library Data.Resumption.Workflows

  * F# idiomatic wrappers around the extension methods in Data.Resumption.

  * Computation expression builders `datatask` and `dataseq`. These
    are by far the best way to write `Plan<T>`s and
    `IDataEnumerable<T>`s.

* Test library Data.Resumption.Test

  * Unit tests against a hypothetical data source that just echos strings.

  * Tests assert that batching, caching, dedup work as intended, and
    that exception handling works in various scenarios.

  * Very few tests so far. Contributions in this area would of course
    be welcomed, even in this early stage of the project.

* Example integration: Data.Resumption.IPGeo

  * C# library demonstrating how to integrate an existing API
    (ip-api.com) with DataRes.

* Example integration test: Data.Resumption.IPGeo.Test

  * Demonstrates how the example integration can be used.

* Example integration: Data.Resumption.EF

  * C# library demonstrating how to integrate with Entity Framework.

  * Uses EntityFramework.Extended to support batching queries.

## What's still to come

* SQL integration with a micro-ORM

  * Should be comparable to Dapper/OrmLite/pals but work with DataRes
    for batching.

* Documentation

  * A real README.

  * A long-form blog post working through a real world example.

  * API reference.

* Example application

  * Something like a TODO list app that runs in Azure.

  * Ideally, would have two or three versions of the backend
    implementation:

    * A naive version (no batching)

    * A version with manually coded batching

    * A DataRes version
