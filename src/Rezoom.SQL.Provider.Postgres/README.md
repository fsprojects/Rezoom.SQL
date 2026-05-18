# Rezoom.SQL.Provider.Postgres

Meta-package: brings in [Rezoom.SQL.Provider](https://www.nuget.org/packages/Rezoom.SQL.Provider/)
plus [Npgsql](https://www.nuget.org/packages/Npgsql/) as the runtime ADO.NET
driver for PostgreSQL.

Install this if you're using Rezoom.SQL, targeting Postgres, and want a single PackageReference that includes the necessary driver.

```sh
dotnet add package Rezoom.SQL.Provider.Postgres
```

Set `"backend": "postgres"` in your `rzsql.json`. See the
[main Rezoom.SQL repository](https://github.com/rspeele/Rezoom.SQL) for the
tutorial, language reference, and configuration docs.
