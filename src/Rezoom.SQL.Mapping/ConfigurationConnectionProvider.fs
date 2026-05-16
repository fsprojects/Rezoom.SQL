namespace Rezoom.SQL.Mapping
open System
open System.Collections.Concurrent
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
///   <item>Provider invariant at <c>RezoomSQL:Providers:{name}</c>. If that's not
///         set, falls back to a backend default registered by the TP-generated
///         code for this connection name (e.g. <c>Microsoft.Data.Sqlite</c> for a
///         SQLite project). If nothing is registered either, falls back to
///         <c>Microsoft.Data.SqlClient</c>.</item>
/// </list>
/// Typical ASP.NET Core registration:
/// <code>
///   services.AddSingleton&lt;ConnectionProvider, ConfigurationConnectionProvider&gt;();
/// </code>
/// </remarks>
type ConfigurationConnectionProvider(configuration : IConfiguration) =
    inherit ConnectionProvider()

    [<Literal>]
    static let HardcodedFallback = "Microsoft.Data.SqlClient"

    static let backendDefaults = ConcurrentDictionary<string, string>(StringComparer.OrdinalIgnoreCase)

    /// Registers the canonical ADO.NET provider invariant for a given connection
    /// name. The TP-generated Migrate and Command code calls this at first touch
    /// so users don't have to write a RezoomSQL:Providers:{name} section in
    /// appsettings.json for the 99% case where one project targets one backend.
    /// Idempotent. Last writer wins if multiple registrations collide on the same
    /// connection name; an explicit RezoomSQL:Providers config entry overrides
    /// this registry entirely.
    static member RegisterBackendDefault(connectionName : string, providerInvariant : string) =
        backendDefaults.[connectionName] <- providerInvariant

    override __.GetConnectionString(name) =
        let connectionString = configuration.[sprintf "ConnectionStrings:%s" name]
        if String.IsNullOrEmpty(connectionString) then
            failwithf
                "No connection string named '%s' in configuration (looked for ConnectionStrings:%s)"
                name name
        let providerName =
            match configuration.[sprintf "RezoomSQL:Providers:%s" name] with
            | null | "" ->
                let registered, value = backendDefaults.TryGetValue(name)
                if registered then value else HardcodedFallback
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

/// Augments ConnectionProvider with a static factory that resolves from any host's
/// IServiceProvider. It tries an explicitly-registered ConnectionProvider first,
/// then falls back to constructing a ConfigurationConnectionProvider from a
/// registered IConfiguration. This is what plan execution and TP-generated Migrate
/// calls use, so any host with a normal ASP.NET Core / generic-host setup gets
/// connection-string resolution for free without registering anything Rezoom-specific.
[<AutoOpen>]
module ConnectionProviderExtensions =
    type ConnectionProvider with
        static member ResolveFrom(services : IServiceProvider) : ConnectionProvider =
            match services.GetService(typeof<ConnectionProvider>) with
            | :? ConnectionProvider as provider -> provider
            | _ ->
                match services.GetService(typeof<IConfiguration>) with
                | :? IConfiguration as cfg ->
                    ConfigurationConnectionProvider(cfg) :> ConnectionProvider
                | _ ->
                    failwith
                        "Rezoom.SQL needs either a ConnectionProvider or an IConfiguration registered \
                         in the IServiceProvider, but found neither. Register an IConfiguration \
                         (standard for ASP.NET Core / generic host) to get the default \
                         ConfigurationConnectionProvider, or supply your own with \
                         `services.AddSingleton<Rezoom.SQL.Mapping.ConnectionProvider, YourImpl>()`."
