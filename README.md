# Work in progress

This is a library implementing a resumption monad for .NET, which
allows for the following things to happen:

* Requests to the same data source that don't depend on each other can be automatically batched into a single round-trip (or equivalent expensive operation).

* Idempotent requests for the same data (e.g. current user) in the same execution context (e.g. web API request) can be automatically cached and de-duplicated.

This is similar in effect to Haskell's Haxl.

## What's done so far

* Core library Rezoom

  * The fundamental types `Plan<'a>` and `Errand<'a>`.

  * Operations like monadic bind, apply, map, etc. on those types.

  * Exception handling for plans.

  * The `plan` computation expression builder that allows writing plans like plain F# code.

  * Mostly XML-commented, but no "big picture" documentation yet.

  * Compatibility wrappers to help implement errands from C#.

  * Execution layer to run a `Plan` as a `System.Threading.Task` with batching/caching/deduplication.

  * Note: errand API is still unstable.

* Test library Rezoom.Test

  * Unit tests against a hypothetical data source that just echos strings.

  * Tests assert that batching, caching, dedup work as intended, and
    that exception handling works in various scenarios.

  * Very few tests so far. Contributions in this area would of course
    be welcomed, even in this early stage of the project.

* Example integration: Rezoom.IPGeo

  * C# library demonstrating how to integrate an existing API
    (ip-api.com) with Rezoom.

* Example integration test: Rezoom.IPGeo.Test

  * Demonstrates how the example integration can be used.

* Micro-ORM StaticQL.Mapping

  * Not dependent or inherently integrated with the rest of Rezoom, but designed to work well with it.

  * Automatically materializes the results of a SQL query (`IDataReader`) as CLR objects e.g. `User list`.

  * Works with F# record types and other immutable (constructor-initialized) types.

  * Generates IL for fast object construction.

  * Uses column naming convention to materialize nested structures, e.g. a list of Groups each with a nested list of Users.

## What's in progress

* SQL type provider StaticQL.Provider

  * Understands StaticQL, a "lowest common denominator" dialect of SQL (basically SQLite's syntax with a few extensions)

  * Can output it to various backends - SQLite, T-SQL, Postgres, etc.
    Goal is not to let you change backends transparently but just to
    let you use the same _syntax_ to write queries for any of them.
    For example, the built-in functions available to you will depend
    on which backend you select.

  * Reads migration scripts from a "model" folder to build a virtual
    model of your database. Validates queries against this model.

  * Infers the types of parameters used in your query.

  * Generates statically typed query objects with materialization handled by StaticQL.Mapping.

## What's on the horizon

* Documentation

  * A real README.

  * A long-form blog post working through a real world example.

  * API reference.

* Example application

  * Something like a TODO list app that runs in Azure.

  * Can demo with two execution implementations to show the difference
    in round-trips achieved by automatic batching/caching.

