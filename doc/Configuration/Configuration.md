# Runtime configuration

Two pieces of runtime configuration are needed: the connection string for each
named connection, and the ADO.NET provider invariant (e.g.
`Microsoft.Data.SqlClient`) used to open it.

By default, Rezoom.SQL reads both from your host's `IConfiguration` — the
standard configuration abstraction used by ASP.NET Core and the .NET generic
host. No App.config, no `ConfigurationManager`.

## appsettings.json

```json
{
  "ConnectionStrings": {
    "rzsql": "Data Source=.\\SQLEXPRESS;Initial Catalog=rzsql;Integrated Security=SSPI;TrustServerCertificate=true"
  },
  "RezoomSQL": {
    "Providers": {
      "rzsql": "Microsoft.Data.SqlClient"
    }
  }
}
```

The connection name (`rzsql` here) must match the `connectionName` setting in
your [rzsql.json](Json.md).

## ASP.NET Core / generic host

If your host already wires up `IConfiguration` (which ASP.NET Core's
`WebApplication.CreateBuilder` does by default), there is **nothing else to
register**. Rezoom.SQL constructs a `ConfigurationConnectionProvider` lazily
the first time something asks for a connection.

```csharp
var builder = WebApplication.CreateBuilder(args);
builder.Services.AddScoped<PlanExecutor>();
var app = builder.Build();
// ...
FiddleModel.Migrate(MigrationConfig.Default, app.Services);
```

## Plain console app

Build your own `ServiceProvider`, register `IConfiguration`, pass it to
`Migrate` and to `PlanExecutor`.

```fsharp
let configuration =
    ConfigurationBuilder()
        .AddJsonFile("appsettings.json", optional = false)
        .AddEnvironmentVariables()
        .Build() :> IConfiguration

let collection = ServiceCollection()
collection.AddSingleton<IConfiguration>(configuration) |> ignore
use provider = collection.BuildServiceProvider()
let services = provider :> IServiceProvider

MyModel.Migrate(MigrationConfig.Default, services)
```

## Sample connection strings

### SQLite (using [Microsoft.Data.Sqlite](https://www.nuget.org/packages/Microsoft.Data.Sqlite/))

```json
{
  "ConnectionStrings": {
    "rzsql": "Data Source=rzsql.db"
  },
  "RezoomSQL": {
    "Providers": { "rzsql": "Microsoft.Data.Sqlite" }
  }
}
```

### T-SQL (using [Microsoft.Data.SqlClient](https://www.nuget.org/packages/Microsoft.Data.SqlClient/))

```json
{
  "ConnectionStrings": {
    "rzsql": "Data Source=.\\SQLEXPRESS;Initial Catalog=rzsql;Integrated Security=SSPI;TrustServerCertificate=true"
  },
  "RezoomSQL": {
    "Providers": { "rzsql": "Microsoft.Data.SqlClient" }
  }
}
```

### Postgres (using [Npgsql](https://www.nuget.org/packages/Npgsql/))

```json
{
  "ConnectionStrings": {
    "rzsql": "Host=localhost;Database=rzsql;Username=your_user;Password=your_password"
  },
  "RezoomSQL": {
    "Providers": { "rzsql": "Npgsql" }
  }
}
```

## Custom ConnectionProvider

If you need to source the connection string from somewhere other than
`IConfiguration` — e.g. Azure Key Vault, AWS Secrets Manager, an in-process
secrets store — subclass `ConnectionProvider`:

```csharp
public sealed class KeyVaultConnectionProvider : ConnectionProvider
{
    public override ConnectionInfo GetConnectionString(string name) { /* ... */ }
    public override DbConnection Open(string name) { /* ... */ }
}
```

Register it in DI; the explicit registration takes precedence over the
`IConfiguration` fallback:

```csharp
services.AddSingleton<ConnectionProvider, KeyVaultConnectionProvider>();
```

## Automatic database creation

When you run migrations via `MyModel.Migrate(MigrationConfig.Default, services)`,
Rezoom.SQL will try to create the database described in your connection string
if it doesn't already exist.

### SQLite

If the filename in `Data Source=somefile.db` doesn't exist, Rezoom.SQL creates
an empty file by that name, which is sufficent for SQLite to "connect" to and
start creating tables.

### T-SQL

If the connection fails for any reason other than total inability to establish
communication, Rezoom.SQL reconnects with `Initial Catalog` set to `master`. If
that succeeds, it creates the database originally named as the initial catalog
and reconnects to it.

[Source](https://github.com/rspeele/Rezoom.SQL/blob/master/src/Rezoom.SQL.Compiler/TSQL.MigrationBackend.fs)

### Postgres

If the connection fails with error `3D000` (Invalid Catalog Name), Rezoom.SQL
reconnects with `Database=postgres`, creates the database originally named in
the connection string, and reconnects.

[Source](https://github.com/rspeele/Rezoom.SQL/blob/master/src/Rezoom.SQL.Compiler/Postgres.MigrationBackend.fs)
