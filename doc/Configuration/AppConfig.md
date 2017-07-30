# App.config

By default, Rezoom.SQL creates database connections using configuration from
your startup project's App.config file (or Web.config for ASP.NET websites).

Note well the following:

* App.config is only used at runtime. At compile time RZSQL is oblivious to this
  file's existence.

* Conversely, [rzsql.json](Json.md) is used to configure RZSQL at compile time,
  and not loaded at runtime.

* Only the App.config of the startup project (the exe you're running) matters.
  If you put an App.config in a library and reference that library from a
  console app, the App.config in the library makes no difference whatsoever.

There are two things you need to accomplish in your App.config:

1. In the `<connectionStrings>` section, add a connection string whose name
   matches the `ConnectionName` property from `rzsql.json`. (default: `"rzsql"`)

2. In the `<system.data><DbProviderFactories>` section, add an entry for the
   database provider you're using.

## Samples

In practice, nobody can remember how to write a connection string, let alone a
`DbProviderFactory` entry from scratch. The originals were found via brute force
search in a Microsoft Research project, and have been copied from StackOverflow
ever since.

An easy way to get a working App.config as a starting point is to use the helper
NuGet package for your backend, which will also install the appropriate ADO.NET
provider if necessary:

* [SQLite](https://www.nuget.org/packages/Rezoom.SQL.Provider.SQLite/)
* [T-SQL](https://www.nuget.org/packages/Rezoom.SQL.Provider.TSQL/)
* [Postgres](https://www.nuget.org/packages/Rezoom.SQL.Provider.Postgres/)

If you want to set it up yourself, here are sample App.configs for each backend:

### SQLite, using [System.Data.SQLite.Core](https://www.nuget.org/packages/System.Data.SQLite.Core/)

```xml
<?xml version="1.0" encoding="utf-8"?>
<configuration>
  <connectionStrings>
    <add name="rzsql" providerName="System.Data.SQLite" connectionString="Data Source=rzsql.db" />
  </connectionStrings>
  <system.data>
    <DbProviderFactories>
      <remove invariant="System.Data.SQLite" />
      <add name="SQLite Data Provider" invariant="System.Data.SQLite" description=".NET Framework Data Provider for SQLite" type="System.Data.SQLite.SQLiteFactory, System.Data.SQLite" />
    </DbProviderFactories>
  </system.data>
</configuration>
```

### T-SQL

```xml
<?xml version="1.0" encoding="utf-8"?>
<configuration>
  <connectionStrings>
    <add
      name="rzsql"
      providerName="System.Data.SqlClient"
      connectionString="Data Source=.\SQLEXPRESS;Integrated Security=SSPI;Initial Catalog=rzsql"
    />
  </connectionStrings>
  <system.data>
    <DbProviderFactories>
      <remove invariant="System.Data.SqlClient" />
      <add
        name="SqlClient Data Provider"
        invariant="System.Data.SqlClient"
        description=".Net Framework Data Provider for SqlServer"
        type="System.Data.SqlClient.SqlClientFactory, System.Data, Version=2.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089"
      />
    </DbProviderFactories>
  </system.data>
</configuration>
```

### Postgres, using [Npgsql](https://www.nuget.org/packages/Npgsql/)

```xml
<?xml version="1.0" encoding="utf-8"?>
<configuration>
  <connectionStrings>
    <add name="rzsql" providerName="Npgsql" connectionString="Host=localhost;Database=rzsql;Username=your_user_here;Password=your_password_here" />
  </connectionStrings>
  <system.data>
    <DbProviderFactories>
      <remove invariant="Npgsql" />
      <add name="Npgsql Data Provider" invariant="Npgsql" support="FF" description=".Net Framework Data Provider for Postgresql" type="Npgsql.NpgsqlFactory, Npgsql" />
    </DbProviderFactories>
  </system.data>
</configuration>
```

## Automatic database creation

When you run migrations (via `MyModel.Migrate(MigrationConfig.Default)`), RZSQL
will try to create the database described in your connection string if it does
not already exist. Here's how this happens:

### SQLite

If the filename specified in `Data Source=somefile.db` doesn't exist, RZSQL
creates an empty file by that name, which is sufficent for SQLite to "connect"
to and start creating tables.

### T-SQL

If the connection fails for any reason other than total inability to establish
communication, RZSQL attempts to reconnect with `Initial Catalog` set to
`master`. If it is able to connect successfully this way, it will attempt to
create the database originally named as the initial catalog, and, if successful,
reconnect to that new database.

[Source](https://github.com/rspeele/Rezoom.SQL/blob/master/src/Rezoom.SQL.Compiler/TSQL.MigrationBackend.fs)

### Postgres

If the connection fails with error "3D000" (Invalid Catalog Name), RZSQL
attempts to reconnect with `Database=postgres`. If it is able to connect
successfully this way, it will create the database originally named in the
connection string, and, if successful, reconnect to that new database.

[Source](https://github.com/rspeele/Rezoom.SQL/blob/master/src/Rezoom.SQL.Compiler/Postgres.MigrationBackend.fs)