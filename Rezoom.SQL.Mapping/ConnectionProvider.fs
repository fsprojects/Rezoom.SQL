namespace Rezoom.SQL
open System.Configuration
open System.Data.Common

[<AbstractClass>]
type ConnectionProvider() =
    abstract member Open : name : string -> DbConnection
    abstract member BeginTransaction : DbConnection -> DbTransaction
    default __.BeginTransaction(conn) = conn.BeginTransaction()
    static member ResolveConnectionString(name : string) =
        let connectionStrings = ConfigurationManager.ConnectionStrings
        if isNull connectionStrings then
            failwith "No <connectionStrings> element in config"
        let connectionString = connectionStrings.[name]
        if isNull connectionString then
            failwith "No connection string by the expected name"
        else
            connectionString

type DefaultConnectionProvider() =
    inherit ConnectionProvider()
    static member Open(name) =
        let connectionString = ConnectionProvider.ResolveConnectionString(name)
        let provider = DbProviderFactories.GetFactory(connectionString.ProviderName)
        let conn = provider.CreateConnection()
        conn.ConnectionString <- connectionString.ConnectionString
        conn.Open()
        conn
    override __.Open(name) = DefaultConnectionProvider.Open(name)

