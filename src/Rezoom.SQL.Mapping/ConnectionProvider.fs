namespace Rezoom.SQL.Mapping
open System
open System.Data.Common
open System.Reflection

/// Settings for a single named connection: connection string + provider invariant.
/// Returned by <see cref="ConnectionProvider.GetConnectionString"/> and consumed by
/// dialect-specific migration backends.
[<NoEquality; NoComparison>]
type ConnectionInfo =
    {   /// The logical name this connection is known by (matches the name passed to
        /// <see cref="ConnectionProvider.GetConnectionString"/>).
        Name : string
        ConnectionString : string
        /// Provider invariant. e.g. <c>"Microsoft.Data.SqlClient"</c>,
        /// <c>"Npgsql"</c>, <c>"Microsoft.Data.Sqlite"</c>.
        ProviderName : string
    }

[<AbstractClass>]
type ConnectionProvider() =
    /// Open a live <see cref="DbConnection"/> for the named connection.
    abstract member Open : name : string -> DbConnection
    abstract member BeginTransaction : DbConnection -> DbTransaction
    default __.BeginTransaction(conn) = conn.BeginTransaction()
    /// Resolve a named connection to its <see cref="ConnectionInfo"/>. Used by
    /// migrations, where the result is passed to the dialect's migration backend
    /// constructor.
    abstract member GetConnectionString : name : string -> ConnectionInfo

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
