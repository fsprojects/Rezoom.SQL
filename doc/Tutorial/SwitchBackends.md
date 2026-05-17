<!-- nav-top -->
[Home](../../README.md) &gt; [Tutorial](README.md) &gt; Using TSQL or Postgres

[&larr; Adding migrations](AddingMigrations.md) | [Loading nested objects &rarr;](LoadingNestedObjects.md)
<!-- /nav-top -->

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
actually run the code, you'll need to create/edit your `AppSettings.json` with
connection settings for SQL Server. This is *runtime* configuration whereas `rzsql.json`
was *compile-time* information.

Here is an example AppSettings.json.

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

Change the `ConnectionStrings` and `RezoomSQL.Providers` sections like so:

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

In the above configuration I am assuming your SQL server is located at
.\SQLEXPRESS. If it isn't, change the connection string accordingly.

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
actually run the code, you'll need to create/edit your `AppSettings.json` with
connection settings for Postgres. This is *runtime* configuration whereas `rzsql.json`
was *compile-time* information.

Here is an example AppSettings.json.

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

Change the `ConnectionStrings` and `RezoomSQL.Providers` sections like so.
You'll need to fill in your own username and password in the connection string.

```json
{
  "ConnectionStrings": {
    "rzsql": "Host=localhost;Database=rzsql;Username=your_user_here;Password=your_password_here"
  },
  "RezoomSQL": {
    "Providers": { "rzsql": "Npgsql" }
  }
}
```

---

---
<!-- nav-bottom -->
[&larr; Adding migrations](AddingMigrations.md) | [Loading nested objects &rarr;](LoadingNestedObjects.md)
<!-- /nav-bottom -->

