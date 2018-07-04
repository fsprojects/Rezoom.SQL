module Rezoom.SQL.Compiler.BackendUtilities
open System
open System.Configuration
open System.Data
open System.Data.Common
open System.Text
open Rezoom.SQL.Mapping
open Rezoom.SQL.Migrations
open Rezoom.SQL.Compiler

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
            | LineBreak
            | Indent
            | Outdent
            | Parameter _
            | InlineParameter _
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
let tabin = Indent
let tabout = Outdent
let linebreak = LineBreak
let text str = CommandText str

let joinWith separator (fragments : Fragments seq) =
    seq {
        let mutable first = true
        for element in fragments do
            if not first then yield! separator
            else first <- false
            yield! element
    }

let joinLines separator fragments = joinWith [| linebreak; text separator; ws |] fragments
let joinLines1 separator sequence = joinLines separator (sequence |> Seq.map Seq.singleton)
let join separator fragments = joinWith (Seq.singleton <| text separator) fragments
let join1 separator sequence = join separator (sequence |> Seq.map Seq.singleton)
let indent fragments =
    seq {
        yield tabin
        yield! fragments
        yield tabout
    }
let parencols fragmentss =
    seq {
        let comma = text ","
        let mutable first = true
        yield text "("
        yield ws
        for fragments in fragmentss do
            if not first then
                yield comma
                yield ws
            else
                first <- false
            yield! fragments
            yield linebreak
        yield text ")"
    } |> indent

let parencols1 fragments = parencols (fragments |> Seq.map Seq.singleton)

type DbMigration(majorVersion : int, name : string) =
    [<BlueprintKey>]
    member __.MajorVersion = majorVersion
    [<BlueprintKey>]
    member __.Name = name
    member __.ToTuple() = (majorVersion, name)

type DefaultMigrationBackend(conn : DbConnection) =
    new(settings : ConnectionStringSettings) =
        let provider = Rezoom.SQL.Mapping.NetStandardHacks.DbProviderFactories.GetFactory(settings.ProviderName)
        let conn = provider.CreateConnection()
        conn.ConnectionString <- settings.ConnectionString
        new DefaultMigrationBackend(conn)
    member __.Connection = conn
    abstract member Initialize : unit -> unit
    abstract member GetMigrationsRun : unit -> (int * string) seq
    abstract member RunMigration : string Migration -> unit
    abstract member Batches : string -> string seq
    default __.Batches(source) = Seq.singleton source
    default __.Initialize() =
        conn.Open()
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
    default this.RunMigration(migration) =
        use tx = conn.BeginTransaction()
        for batch in this.Batches(migration.Source) do
            use cmd = conn.CreateCommand()
            cmd.Transaction <- tx
            cmd.CommandText <- batch
            ignore <| cmd.ExecuteNonQuery()
        do
            use cmd = conn.CreateCommand()
            cmd.Transaction <- tx
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
        member this.Dispose() = conn.Dispose()
