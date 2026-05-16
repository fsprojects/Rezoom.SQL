using System.Configuration;
using System.Data.Common;
using Rezoom.SQL.Mapping;

namespace SQLFiddle.Website;

/// <summary>
/// Pulls connection settings from ASP.NET Core's <see cref="IConfiguration"/>:
/// the connection string from the standard <c>ConnectionStrings:{name}</c> slot,
/// and the provider invariant (e.g. <c>"Microsoft.Data.SqlClient"</c>) from
/// <c>RezoomSQL:Providers:{name}</c>, falling back to <c>Microsoft.Data.SqlClient</c>.
///
/// Wire it up in Program.cs as a singleton ConnectionProvider; controllers (and
/// the migration call) then take it via constructor injection like any other
/// service. No App.config required.
/// </summary>
public sealed class ConfigurationConnectionProvider(IConfiguration cfg) : ConnectionProvider
{
    public override ConnectionStringSettings GetConnectionString(string name)
    {
        var connectionString = cfg.GetConnectionString(name)
            ?? throw new InvalidOperationException(
                $"No connection string named '{name}' under ConnectionStrings:{name}");
        var providerName = cfg[$"RezoomSQL:Providers:{name}"] ?? "Microsoft.Data.SqlClient";
        return new ConnectionStringSettings(name, connectionString, providerName);
    }

    public override DbConnection Open(string name)
    {
        var settings = GetConnectionString(name);
        var factory = NetStandardHacks.DbProviderFactories.GetFactory(settings.ProviderName);
        var conn = factory.CreateConnection()
            ?? throw new InvalidOperationException(
                $"Provider '{settings.ProviderName}' returned a null DbConnection");
        conn.ConnectionString = settings.ConnectionString;
        conn.Open();
        return conn;
    }
}
