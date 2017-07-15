# Rezoom.SQL.Migrations

This namespace contains the migration configuration and some types representing
migrations within your data model.

## The `MigrationConfig` type

This is a record type with the following properties:

### AllowRetroactiveMigrations : bool

If true, permit running migrations that have not been run on the database, but
are older than other migrations that *have* been run. Typically this would only
make sense in development, after merging migrations written by another
developer.

See also the section on [migration trees](../Configuration/MigrationTrees.md).

### LogMigrationRan : Migration<string> -> unit

This is a function which will be called after a migration has been run. Often it
is nice to print the migration name to the console or perhaps to a text file.

## MigrationConfig defaults

The static member `MigrationConfig.Default` is suitable for most typical usages.
It is defined as:

```fsharp
{   AllowRetroactiveMigrations = false
    LogMigrationRan = fun _ -> ()
}
```

