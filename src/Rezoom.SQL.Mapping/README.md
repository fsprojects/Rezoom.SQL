# Rezoom.SQL.Mapping

Runtime code for executing SQL queries.

## Entity Readers

The bulk of this project is loader code that enables us to run a query and materialize the result sets as strongly typed objects.
Does reflection on the result row type it's going to load, then generates CIL to make a super fast loader from DbDataReader.

In other words, basically reimplements a lot of what Dapper does. But we need special features to handle our
MANY(...) construct and primitive type conversions, so that's why it's implemented in-house vs. pulling in Dapper to do this job.

## Batching and Rezoom integration

Additionally, this project defines how a Command<'a> can be turned into an Errand<'a> for use in a Rezoom `plan {...}` block.

This plugs into the Rezoom system and lets us support automatic batching and caching.

The actual batching code is in CommandBatch.fs, and the Rezoom integration that coordinates errand execution is in Plans.fs.

## Migrations

Defines the logic for figuring out a migration tree from a set of source files, and running the
migrations on a database.

## Side Note on Dependency Order

Rezoom.SQL.Compiler depends on this project, for types like CommandFragment and MigrationTree.

A slightly cleaner way to structure this would be a shared base library with those types, referenced by both
Rezoom.SQL.Compiler and Rezoom.SQL.Mapping. Then you wouldn't have to pull in whole runtime layer if you
really wanted just the compiler layer to do some analysis and translation on SQL queries.

But practically speaking it's fine this way.
