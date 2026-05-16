# Rezoom.SQL.Provider.TSQL

Meta-package: brings in [Rezoom.SQL.Provider](https://www.nuget.org/packages/Rezoom.SQL.Provider/)
plus [Microsoft.Data.SqlClient](https://www.nuget.org/packages/Microsoft.Data.SqlClient/)
as the runtime ADO.NET driver for Microsoft SQL Server.

Install this if you're using Rezoom.SQL, targeting T-SQL, and want a single PackageReference that includes the necessary driver.
```sh
dotnet add package Rezoom.SQL.Provider.TSQL
```

Set `"backend": "tsql"` in your `rzsql.json`. See the
[main Rezoom.SQL repository](https://github.com/rspeele/Rezoom.SQL) for the
tutorial, language reference, and configuration docs.
