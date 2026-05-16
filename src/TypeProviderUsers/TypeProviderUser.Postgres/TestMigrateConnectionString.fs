module TypeProviderUser.Postgres.TestMigrateConnectionString

open Microsoft.Extensions.Configuration
open NUnit.Framework
open Rezoom.SQL.Migrations

/// Exercises the connection-string Migrate overload against a live Postgres.
/// Idempotent on an already-migrated database. Migrate just checks the
/// migration history table and finds nothing to do.
[<Test>]
let ``migrate by connection string`` () =
    requirePostgres ()
    let cfg = services.GetService(typeof<IConfiguration>) :?> IConfiguration
    let connectionString = cfg.["ConnectionStrings:rzsql"]
    TestModel.Migrate(MigrationConfig.Default, connectionString)
