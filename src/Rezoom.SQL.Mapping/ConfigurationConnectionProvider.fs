namespace Rezoom.SQL.Mapping
open System
open Microsoft.Extensions.Configuration

/// <summary>
/// <see cref="ConnectionProvider"/> that resolves settings via Microsoft.Extensions.Configuration
/// (appsettings.json, environment variables, anything else wired into the host's
/// <see cref="IConfiguration"/>).
/// </summary>
/// <remarks>
/// Looks up:
/// <list type="bullet">
///   <item>Connection string at <c>ConnectionStrings:{name}</c></item>
///   <item>Provider invariant at <c>RezoomSQL:Providers:{name}</c>, defaulting to
///         <c>Microsoft.Data.SqlClient</c> when unset</item>
/// </list>
/// Typical ASP.NET Core registration:
/// <code>
///   services.AddSingleton&lt;ConnectionProvider, ConfigurationConnectionProvider&gt;();
/// </code>
/// </remarks>
type ConfigurationConnectionProvider(configuration : IConfiguration) =
    inherit ConnectionProvider()

    [<Literal>]
    static let DefaultProviderName = "Microsoft.Data.SqlClient"

    override __.GetConnectionString(name) =
        let connectionString = configuration.[sprintf "ConnectionStrings:%s" name]
        if String.IsNullOrEmpty(connectionString) then
            failwithf
                "No connection string named '%s' in configuration (looked for ConnectionStrings:%s)"
                name name
        let providerName =
            match configuration.[sprintf "RezoomSQL:Providers:%s" name] with
            | null | "" -> DefaultProviderName
            | v -> v
        {   Name = name
            ConnectionString = connectionString
            ProviderName = providerName
        }

    override this.Open(name) =
        let info = this.GetConnectionString(name)
        let factory = NetStandardHacks.DbProviderFactories.GetFactory(info.ProviderName)
        let conn = factory.CreateConnection()
        if isNull conn then
            failwithf "Provider '%s' returned a null DbConnection" info.ProviderName
        conn.ConnectionString <- info.ConnectionString
        conn.Open()
        if conn.GetType().Name = "SQLiteConnection" then
            // Encourage SQLite to put the R in RDBMS
            use cmd = conn.CreateCommand()
            cmd.CommandText <- "PRAGMA foreign_keys=ON;"
            cmd.ExecuteNonQuery() |> ignore
        conn
