open System
open System.IO
open System.Data.SQLite
open Rezoom
open Rezoom.Execution
open Rezoom.SQL.Provider
open Rezoom.SQL.Mapping
open Rezoom.SQL.Mapping.Migrations

type DataModel = SQLModel

type GetChildTodos = SQL<"""
    select * from ActiveTodos where ParentId is @parentId
""">

type MakeChildTodo = SQL<"""
    insert into Todos(ParentId, Heading, Paragraph, DeactivatedUtc)
    values (null, @heading, @paragraph, null);
    select last_insert_rowid() as id;
""">

let cmdPlan (cmd : string) =
    plan {
        match cmd with
        | "ls" ->
            let! children = GetChildTodos.Command(None).ExecutePlan()
            printfn "%d children" children.Count
            for child in children do
                printfn "%s: %s" child.Heading (defaultArg child.Paragraph "None")
        | "mk" ->
            let! result = MakeChildTodo.Command("test heading", Some "test para").ExecutePlan()
            printfn "Created TODO %d" result.[0].id
        | "throw" ->
            failwith "unhandled exn"
        | _ ->
            printfn "Unrecognized command ``%s``" cmd
    }

let buildPlan (cmd : string) =
    plan {
        let cmds = cmd.Split(';')
        for cmd in batch cmds do
            do! cmdPlan (cmd.Trim())
    }

let migrate() =
    let dbname = "test.db"
    if not <| File.Exists(dbname) then
        SQLiteConnection.CreateFile(dbname)
    use conn = new SQLiteConnection("data source=" + dbname)
    conn.Open()
    let config =
        {   AllowMigrationsFromOlderMajorVersions = false
            LogMigrationRan = fun _ -> ()
        }
    DataModel.Migrate(config, conn)

[<EntryPoint>]
let main argv =
    migrate()
    let execute plan = Execution.execute ExecutionConfig.Default plan
    while true do
        let cmd = Console.ReadLine()
        try
            let plan = buildPlan cmd
            (execute plan).Wait()
        with
        | exn ->
            printfn "Plan failed with exn: %O" exn
    0 // return an integer exit code
