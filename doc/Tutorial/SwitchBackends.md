(this page is part of [the Rezoom.SQL tutorial](README.md))

# Switching to SQL Server or PostgreSQL

Rezoom.SQL translates its own dialect of SQL to different "backends". Currently
[SQLite](https://www.sqlite.org/), MS SQL Server (T-SQL), and
[PostgreSQL](https://www.postgresql.org/) are supported.

So far, this tutorial has stuck to SQLite. However, most apps in the .NET
ecosystem store their data in SQL Server, so they use T-SQL.

## T-SQL

Note: if you want to use Postgres, just skip down the page. The Postgres section
repeats any information you need from from this part.

If you're starting a fresh project and want to target T-SQL, it's as easy as
installing
[Rezoom.SQL.Provider.TSQL](https://www.nuget.org/packages/Rezoom.SQL.Provider.TSQL/)
instead of
[Rezoom.SQL.Provider.SQLite](https://www.nuget.org/packages/Rezoom.SQL.Provider.SQLite/).
However, both packages are just thin wrappers around the [base
library](https://www.nuget.org/packages/Rezoom.SQL.Provider/). They don't
actually have any code, they just bundle some default config files and the
initial `V1.model.sql`.

**With your existing project, you can easily change the config yourself** to
  target a different database backend. Here's how.

There's a file in your project called `rzsql.json`. Open it up and you'll see this:

```javascript
{
  "backend": "sqlite",
  "optionals":  "f#",
  "migrations": ".",
  "connectionname": "rzsql"
}
```

Just change the `"backend"` setting from `"sqlite"` to `"tsql"`. Then rebuild your project.

You may get build errors if you have queries using the `last_insert_rowid()`
function. This is because that is a SQLite function, and doesn't exist in T-SQL.
Rezoom.SQL unifies the syntax of SQL queries, but it's not a complete
compatibility layer: the functions available are still determined by the
backend. **In this case, the T-SQL equivalent function is `scope_identity()`**.

At this point your project should build, but you're not done yet. To be able to
actually run the code, you'll need to edit your `App.config` with connection
settings for SQL Server. This part isn't actually Rezoom-specific, it's standard
.NET connection string stuff. However, nobody can remember the details, so here
they are. Open up `App.config`, and you'll find something like this:


```xml
<configuration>
<connectionStrings>
 <add
  name="rzsql" providerName="System.Data.SQLite"
  connectionString="Data Source=rzsql.db"
 />
</connectionStrings>
<system.data>
 <DbProviderFactories>
  <remove invariant="System.Data.SQLite" />
  <add
   name="SQLite Data Provider"
   invariant="System.Data.SQLite"
   description=".NET Framework Data Provider for SQLite"
   type="System.Data.SQLite.SQLiteFactory, System.Data.SQLite"
   />
 </DbProviderFactories>
</system.data>
</configuration>
```

Change the `<connectionStrings>` and `<DbProviderFactories>` sections like so:

```xml
<configuration>
<connectionStrings>
 <add
  name="rzsql" providerName="System.Data.SqlClient"
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
   type="System.Data.SqlClient.SqlClientFactory, System.Data"
  />
 </DbProviderFactories>
</system.data>
</configuration>
```

In the above configuration I am assuming your SQL server is located at
.\SQLEXPRESS. If it isn't, change the `connectionString` attribute accordingly.

## Postgres

If you're starting a new project from scratch, you can just install
[Rezoom.SQL.Provider.Postgres](https://www.nuget.org/packages/Rezoom.SQL.Provider.Postgres/)
instead of
[Rezoom.SQL.Provider.SQLite](https://www.nuget.org/packages/Rezoom.SQL.Provider.SQLite/).

But if you want to keep working on the project you already created, you can do that easily too:

There's a file in your project called `rzsql.json`. Open it up and you'll see this:

```javascript
{
  "backend": "sqlite",
  "optionals":  "f#",
  "migrations": ".",
  "connectionname": "rzsql"
}
```

Just change the `"backend"` setting from `"sqlite"` to `"postgres"`. Then rebuild your project.

You may get build errors if you have queries using the `last_insert_rowid()`
function. This is because that is a SQLite function, and doesn't exist in Postgres.
Rezoom.SQL unifies the syntax of SQL queries, but it's not a complete
compatibility layer: the functions available are still determined by the
backend. **In this case, the Postgres equivalent function is `lastval()`**.

At this point your project should build, but you're not done yet. To be able to
actually run the code, you'll need to edit your `App.config` with connection
settings for Postgres. This part isn't actually Rezoom-specific, it's standard
.NET connection string stuff. However, nobody can remember the details, so here
they are. Open up `App.config`, and you'll find something like this:


```xml
<configuration>
<connectionStrings>
 <add
  name="rzsql" providerName="System.Data.SQLite"
  connectionString="Data Source=rzsql.db"
 />
</connectionStrings>
<system.data>
 <DbProviderFactories>
  <remove invariant="System.Data.SQLite" />
  <add
   name="SQLite Data Provider"
   invariant="System.Data.SQLite"
   description=".NET Framework Data Provider for SQLite"
   type="System.Data.SQLite.SQLiteFactory, System.Data.SQLite"
   />
 </DbProviderFactories>
</system.data>
</configuration>
```

Change the `<connectionStrings>` and `<DbProviderFactories>` sections like so.
You'll need to fill in your own username and password in the connection string.

```xml
<configuration>
  <connectionStrings>
    <add
     name="rzsql" providerName="Npgsql"
     connectionString="Host=localhost;Database=rzsql;Username=your_user_here;Password=your_password_here"
    />
  </connectionStrings>
  <system.data>
    <DbProviderFactories>
      <remove invariant="Npgsql" />
      <add
       name="Npgsql Data Provider"
       invariant="Npgsql"
       support="FF"
       description=".Net Framework Data Provider for Postgresql"
       type="Npgsql.NpgsqlFactory, Npgsql"
      />
    </DbProviderFactories>
  </system.data>
</configuration>
```
