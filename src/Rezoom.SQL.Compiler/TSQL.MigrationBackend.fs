namespace Rezoom.SQL.Compiler.TSQL
open System
open System.Configuration
open System.Data.SqlClient
open System.Data.Common
open System.Text.RegularExpressions
open Rezoom.SQL.Compiler
open Rezoom.SQL.Compiler.BackendUtilities

type TSQLMigrationBackend(settings : ConnectionStringSettings) =
    inherit DefaultMigrationBackend(settings)
    let attemptCreateDatabase (conn : DbConnection) =
        let oldConnectionString = conn.ConnectionString
        let builder = SqlConnectionStringBuilder(settings.ConnectionString)
        let catalog = builder.InitialCatalog
        if String.IsNullOrEmpty(catalog) then
            false
        else
            builder.InitialCatalog <- "master"
            conn.ConnectionString <- builder.ConnectionString
            try
                conn.Open()
                use dbCmd = conn.CreateCommand()
                dbCmd.CommandText <-
                    // do we care about injection attacks here? probably not... it's our own connection string
                    sprintf
                        """
                            IF DB_ID('%s') IS NULL
                                CREATE DATABASE [%s];
                        """ catalog catalog
                ignore <| dbCmd.ExecuteNonQuery()
            finally
                conn.Close()
                conn.ConnectionString <- oldConnectionString
            SqlConnection.ClearAllPools()
            // Threading.Thread.Sleep(5000) // For some damn reason it doesn't work if we reconnect right away...
            conn.Open()
            true
    static member BatchSeparator = "RZSQL_DISTINCTIVE_BATCH_SEPARATOR"
    override this.Initialize() =
        let conn = this.Connection
        try
            conn.Open()
        with
        // Class 20 or higher means we couldn't connect at all.
        // https://msdn.microsoft.com/en-us/library/system.data.sqlclient.sqlerror.class(v=vs.110).aspx
        | :? SqlException as exn when exn.Class < 20uy ->
            // A possible source of this problem is that the initial catalog we specified does not yet exist.
            // We'll try to reconnect to the master catalog and create it. This won't work on e.g. Azure SQL,
            // but is very useful on local SQLEXPRESS instances.
            conn.Close()
            let created =
                try
                    attemptCreateDatabase conn
                with
                | innerExn -> 
                    raise <| AggregateException(exn, innerExn)
            if not created then
                reraise()
        // Now we have an open connection (or failed w/ an exception already) -- so proceed with our metadata tables.
        use cmd = conn.CreateCommand()
        cmd.CommandText <-
            """
                IF NOT EXISTS (
                    SELECT * FROM sys.tables t
                    JOIN sys.schemas s ON t.schema_id = s.schema_id
                    WHERE s.name = 'dbo' and t.name = '__RZSQL_MIGRATIONS'
                )
                CREATE TABLE __RZSQL_MIGRATIONS
                    ( MajorVersion int not null
                    , Name varchar(256) not null
                    , PRIMARY KEY (MajorVersion, Name)
                    );
            """
        ignore <| cmd.ExecuteNonQuery()
    override this.Batches(source) =
        Regex.Split(source, Regex.Escape(TSQLMigrationBackend.BatchSeparator))
        |> Seq.filter (not << String.IsNullOrWhiteSpace)