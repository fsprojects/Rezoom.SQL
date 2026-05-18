module TypeProviderUser.TSQL.TestMigrateConnectionString

open Microsoft.Extensions.Configuration
open NUnit.Framework
open Rezoom.SQL.Migrations

/// Exercises the connection-string Migrate overload against a live DB.
[<Test>]
let ``migrate by connection string`` () =
    let cfg = services.GetService(typeof<IConfiguration>) :?> IConfiguration
    let connectionString = "Data Source=(localdb)\\MSSQLLocalDB;Integrated Security=true;Initial Catalog=rzsql2"
    TestModel.Migrate(MigrationConfig.Default, connectionString)
