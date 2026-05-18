module TypeProviderUser.SQLite.TestMigrateConnectionString

open System.IO
open NUnit.Framework
open Rezoom.SQL.Migrations

/// Exercises the connection-string Migrate overload. This is the one a standalone
/// migrator tool would use without any DI / IConfiguration setup.
[<Test>]
let ``migrate by connection string`` () =
    let dbFile = "tpu-connstr-test.db"
    Microsoft.Data.Sqlite.SqliteConnection.ClearAllPools()
    if File.Exists(dbFile) then File.Delete(dbFile)
    try
        TestModel.Migrate(MigrationConfig.Default, sprintf "Data Source=%s" dbFile)
        Assert.IsTrue(File.Exists(dbFile))
    finally
        Microsoft.Data.Sqlite.SqliteConnection.ClearAllPools()
        if File.Exists(dbFile) then File.Delete(dbFile)
