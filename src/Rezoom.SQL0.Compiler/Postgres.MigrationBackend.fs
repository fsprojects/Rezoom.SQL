namespace Rezoom.SQL.Compiler.Postgres
open System
open System.Collections
open System.Configuration
open System.Data.Common
open System.Text.RegularExpressions
open Rezoom.SQL.Compiler.BackendUtilities

type PostgresMigrationBackend(settings : ConnectionStringSettings) =
    inherit DefaultMigrationBackend(settings)
    static let tryGetCode (data : IDictionary) =
        if data.Contains("Code") then
            match data.["Code"] with
            | :? string as code -> Some code
            | _ -> None
        else None
    static let attemptCreateDatabase (conn : DbConnection) oldConnectionString =
        let oldCatalog =
            Regex.Replace(oldConnectionString, @".*Database *= *(\w+).*", "$1", RegexOptions.IgnoreCase)
        let newConnectionString =
            Regex.Replace(oldConnectionString, @"Database *= *\w+", "Database=postgres", RegexOptions.IgnoreCase)
        conn.ConnectionString <- newConnectionString
        try
            conn.Open()
            use dbCmd = conn.CreateCommand()
            dbCmd.CommandText <-
                // do we care about injection attacks here? probably not... it's our own connection string
                sprintf
                    """
                        CREATE DATABASE "%s";
                    """ (oldCatalog.Replace("\"", "\"\""))
            ignore <| dbCmd.ExecuteNonQuery()
        finally
            conn.Close()
            conn.ConnectionString <- oldConnectionString
        conn.Open()
    override this.Initialize() =
        let conn = this.Connection
        try
            conn.Open()
        with
        | :? DbException as exn when tryGetCode exn.Data = Some "3D000" -> // 3D000 Invalid Catalog Name
            conn.Close()
            try
                attemptCreateDatabase conn settings.ConnectionString
            with
            | innerExn -> 
                raise <| AggregateException(exn, innerExn)
        // Now we have an open connection (or failed w/ an exception already) -- so proceed with our metadata tables.
        use cmd = conn.CreateCommand()
        cmd.CommandText <-
            """
                CREATE TABLE IF NOT EXISTS __RZSQL_MIGRATIONS
                    ( MajorVersion int not null
                    , Name varchar(256) not null
                    , PRIMARY KEY (MajorVersion, Name)
                    );
            """
        ignore <| cmd.ExecuteNonQuery()