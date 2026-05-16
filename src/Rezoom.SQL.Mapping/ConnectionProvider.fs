namespace Rezoom.SQL.Mapping
open System
open System.Data.Common
open System.Reflection

/// Minimal abstraction over "give me a live DbConnection by name." Implementations
/// decide how to resolve a name to a connection (config file, secrets store,
/// custom routing, test fixture intercept, etc.). The backend name is the
/// compile-time-known dialect ("sqlite", "tsql", "postgres", "rzsql") from
/// rzsql.json; implementations are free to ignore it. ConfigurationConnectionProvider
/// uses it to pick a default ADO.NET driver when none is set in config.
[<AbstractClass>]
type ConnectionProvider() =
    abstract member Open : connectionName : string * backendName : string -> DbConnection
    abstract member BeginTransaction : DbConnection -> DbTransaction
    default __.BeginTransaction(conn) = conn.BeginTransaction()

module NetStandardHacks =
    let loadInstance (assemblyName : string) (typeName : string) =
        let asm = Assembly.Load(assemblyName)
        if isNull asm then failwithf "Couldn't load assembly %s" assemblyName
        let ty = asm.GetType(typeName)
        if isNull ty then failwithf "Couldn't load type %s from assembly %s" typeName assemblyName
        let instance = ty.GetProperty("Instance", BindingFlags.Public|||BindingFlags.NonPublic|||BindingFlags.Static)
        if isNull instance then
            let instance = ty.GetField("Instance", BindingFlags.Public|||BindingFlags.NonPublic|||BindingFlags.Static)
            if isNull instance then
                Activator.CreateInstance(ty) :?> DbProviderFactory
            else
                instance.GetValue(null) :?> DbProviderFactory
        else
            instance.GetValue(null) :?> DbProviderFactory
    type DbProviderFactories() =
        static member GetFactory(providerName : string) : DbProviderFactory =
            match providerName.ToLowerInvariant() with
            | "system.data.sqlclient" ->
                loadInstance "System.Data.SqlClient" "System.Data.SqlClient.SqlClientFactory"
            | "microsoft.data.sqlclient" ->
                loadInstance "Microsoft.Data.SqlClient" "Microsoft.Data.SqlClient.SqlClientFactory"
            | "system.data.sqlite" ->
                loadInstance "System.Data.SQLite" "System.Data.SQLite.SQLiteFactory"
            | "npgsql" ->
                loadInstance "Npgsql" "Npgsql.NpgsqlFactory"
            | "microsoft.data.sqlite" ->
                loadInstance "Microsoft.Data.Sqlite" "Microsoft.Data.Sqlite.SqliteFactory"
            | other ->
                failwithf "Tragically unsupported provider name ``%s``" other
