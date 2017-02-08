open System
open System.IO
open System.Data.SQLite
open Rezoom
open Rezoom.Execution
open Rezoom.SQL.Provider
open Rezoom.SQL.Mapping
open Rezoom.SQL.Mapping.Migrations

type DataModel = Model

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
            // TODO: infer nullable 2nd param
            let! result = MakeChildTodo.Command("test heading", "test para").ExecutePlan()
            printfn "Created TODO %d" result.[0].id
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
        let plan = buildPlan cmd
        (execute plan).Wait()
    0 // return an integer exit code
