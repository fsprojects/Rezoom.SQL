namespace Rezoom.SQL.Mapping
open System
open System.Collections.Generic
open Microsoft.Extensions.Configuration

/// Settings for a single named connection: connection string + ADO.NET provider invariant.
/// Built by <see cref="ConfigurationConnectionProvider"/> and consumed by dialect
/// migration backends. Not part of <see cref="ConnectionProvider"/>'s abstract
/// surface; it's an implementation detail of the configuration-driven path.
[<NoEquality; NoComparison>]
type ConnectionInfo =
    {   /// The logical name this connection is known by.
        Name : string
        ConnectionString : string
        /// Provider invariant. e.g. <c>"Microsoft.Data.SqlClient"</c>,
        /// <c>"Npgsql"</c>, <c>"Microsoft.Data.Sqlite"</c>.
        ProviderName : string
    }

/// <summary>
/// <see cref="ConnectionProvider"/> that resolves settings via Microsoft.Extensions.Configuration
/// (appsettings.json, environment variables, anything else wired into the host's
/// <see cref="IConfiguration"/>).
/// </summary>
/// <remarks>
/// Looks up:
/// <list type="bullet">
///   <item>Connection string at <c>ConnectionStrings:{name}</c></item>
///   <item>Provider invariant at <c>RezoomSQL:Providers:{name}</c>. If not set,
///         picks the canonical ADO.NET driver for the backend name passed by the
///         TP-generated code (e.g. <c>Microsoft.Data.Sqlite</c> for SQLite). Set
///         <c>RezoomSQL:Providers:{name}</c> to override for non-canonical drivers.</item>
/// </list>
/// </remarks>
type ConfigurationConnectionProvider(configuration : IConfiguration) =
    inherit ConnectionProvider()

    /// Backend name -> canonical ADO.NET provider invariant. Matches what the
    /// wrapper meta-packages (Rezoom.SQL.Provider.{SQLite,TSQL,Postgres}) ship.
    /// The "rzsql" no-op backend points at SqlClient as a placeholder; if you're
    /// actually running migrations on the Identity backend, register a custom
    /// ConnectionProvider or set RezoomSQL:Providers:{name} explicitly.
    static let canonicalDriver : IDictionary<string, string> =
        let d = Dictionary<string, string>(StringComparer.OrdinalIgnoreCase)
        d.["sqlite"] <- "Microsoft.Data.Sqlite"
        d.["tsql"] <- "Microsoft.Data.SqlClient"
        d.["postgres"] <- "Npgsql"
        d.["rzsql"] <- "Microsoft.Data.SqlClient"
        upcast d

    member __.GetConnectionInfo(name : string, backendName : string) : ConnectionInfo =
        let connectionString = configuration.[sprintf "ConnectionStrings:%s" name]
        if String.IsNullOrEmpty(connectionString) then
            failwithf
                "No connection string named '%s' in configuration (looked for ConnectionStrings:%s)"
                name name
        let providerName =
            match configuration.[sprintf "RezoomSQL:Providers:%s" name] with
            | null | "" ->
                let succ, v = canonicalDriver.TryGetValue(backendName)
                if succ then v
                else
                    failwithf
                        "Unknown backend '%s' for connection '%s' and no RezoomSQL:Providers:%s override in configuration"
                        backendName name name
            | v -> v
        {   Name = name
            ConnectionString = connectionString
            ProviderName = providerName
        }

    override this.Open(name, backendName) =
        let info = this.GetConnectionInfo(name, backendName)
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
/// registered IConfiguration.
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
