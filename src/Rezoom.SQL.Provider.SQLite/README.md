# Rezoom.SQL.Provider.SQLite

Meta-package: brings in [Rezoom.SQL.Provider](https://www.nuget.org/packages/Rezoom.SQL.Provider/)
plus [Microsoft.Data.Sqlite](https://www.nuget.org/packages/Microsoft.Data.Sqlite/)
as the runtime ADO.NET driver for SQLite.

Install this if you're using Rezoom.SQL, targeting SQLite, and want a single PackageReference that includes the necessary driver.

```sh
dotnet add package Rezoom.SQL.Provider.SQLite
```

Set `"backend": "sqlite"` in your `rzsql.json`. See the
[main Rezoom.SQL repository](https://github.com/rspeele/Rezoom.SQL) for the
tutorial, language reference, and configuration docs.
