module Rezoom.SQL.BackendUtilities
open System
open System.Data
open System.Data.Common
open System.Text
open System.Globalization
open System.Collections.Generic
open Rezoom.SQL.Mapping
open Rezoom.SQL.Mapping.Migrations
open Rezoom.SQL

type Fragment = CommandFragment
type Fragments = Fragment seq

let simplifyFragments (fragments : Fragments) =
    seq {
        let mutable hasWhitespace = false
        let builder = StringBuilder()
        for fragment in fragments do
            match fragment with
            | CommandText text ->
                ignore <| builder.Append(text)
                hasWhitespace <- text.EndsWith(" ")
            | Whitespace ->
                if not hasWhitespace then ignore <| builder.Append(' ')
                hasWhitespace <- true
            | Parameter _
            | LocalName _ ->
                if builder.Length > 0 then
                    yield CommandText <| builder.ToString()
                    ignore <| builder.Clear()
                yield fragment
                hasWhitespace <- false
        if builder.Length > 0 then
            yield CommandText <| builder.ToString()
            ignore <| builder.Clear()
    }

let ws = Whitespace
let text str = CommandText str

let join separator (fragments : Fragments seq) =
    seq {
        let separator = CommandText separator
        let mutable first = true
        for element in fragments do
            if not first then yield separator
            else first <- false
            yield! element
    }

let join1 separator sequence = join separator (sequence |> Seq.map Seq.singleton)

type DbMigration(majorVersion : int, name : string) =
    member __.ToTuple() = (majorVersion, name)

type DefaultMigrationBackend(conn : DbConnection) =
    abstract member Initialize : unit -> unit
    abstract member GetMigrationsRun : unit -> (int * string) seq
    abstract member RunMigration : string Migration -> unit
    default __.Initialize() =
        use cmd = conn.CreateCommand()
        cmd.CommandText <-
            """
                CREATE TABLE IF NOT EXISTS __RZSQL_MIGRATIONS
                    ( MajorVersion int
                    , Name varchar(256)
                    , UNIQUE (MajorVersion, Name)
                    );
            """
        ignore <| cmd.ExecuteNonQuery()
    default __.GetMigrationsRun() =
        use cmd = conn.CreateCommand()
        cmd.CommandText <-
            """
                SELECT MajorVersion, Name
                FROM __RZSQL_MIGRATIONS
            """
        use reader = cmd.ExecuteReader()
        let entReader = CodeGeneration.ReaderTemplate<DbMigration array>.Template().CreateReader()
        entReader.ProcessColumns(DataReader.columnMap(reader))
        let row = DataReader.DataReaderRow(reader)
        while reader.Read() do
            entReader.Read(row)
        let migrationsRan = entReader.ToEntity()
        migrationsRan
        |> Seq.map (fun m -> m.ToTuple())
    default __.RunMigration(migration) =
        use tx = conn.BeginTransaction()
        do
            use cmd = conn.CreateCommand()
            cmd.CommandText <- migration.Source
            ignore <| cmd.ExecuteNonQuery()
        do
            use cmd = conn.CreateCommand()
            cmd.CommandText <-
                """
                    INSERT INTO __RZSQL_MIGRATIONS
                    VALUES (@major, @name)
                """
            do
                let major = cmd.CreateParameter()
                major.DbType <- DbType.Int32
                major.ParameterName <- "@major"
                major.Value <- box migration.MajorVersion
                ignore <| cmd.Parameters.Add(major)
            do
                let name = cmd.CreateParameter()
                name.DbType <- DbType.String
                name.ParameterName <- "@name"
                name.Value <- box migration.Name
                ignore <| cmd.Parameters.Add(name)
            ignore <| cmd.ExecuteNonQuery()
        tx.Commit()
    interface IMigrationBackend with
        member this.Initialize() = this.Initialize()
        member this.GetMigrationsRun() = this.GetMigrationsRun()
        member this.RunMigration(migration) = this.RunMigration(migration)
