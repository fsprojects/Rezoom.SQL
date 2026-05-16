open System
open System.Diagnostics
open System.Security
open Microsoft.Extensions.Configuration
open Microsoft.Extensions.DependencyInjection
open Rezoom
open FileSystem
open FileSystem.Execution

/// Encapsulates the demo's REPL state. The IServiceProvider is captured once in the
/// ctor and used to build per-call ExecutionConfigs (the demo's "round-trips saved"
/// counter requires a fresh ExecutionLog per Execute, so we can't use PlanExecutor).
type private Repl(services : IServiceProvider) =
    let mutable dumbMode = false

    let execute plan =
        if dumbMode then executeDumb services plan
        else executeSmart services plan

    let bench description (plan : 'a Plan) =
        let stopwatch = Stopwatch()
        stopwatch.Start()
        let result, batchCount = execute plan
        stopwatch.Stop()
        printfn "  -> Ran `%s` in %d ms with %d round trips."
            description stopwatch.ElapsedMilliseconds batchCount
        result

    let rec showHierarchy fromParentId =
        plan {
            let! hierarchies = Domain.getHierarchy fromParentId
            for h in hierarchies do
                printfn "%O" h
        }

    let rec addPermissionsToHierarchy userId hierarchy =
        plan {
            let getOwnPermissions =
                match hierarchy.Node with
                | File file -> Domain.getEffectivePermissions userId file.ParentId
                | Folder folder -> Domain.getEffectivePermissions userId folder.FolderId
            let getChildren =
                [ for child in hierarchy.Children ->
                    addPermissionsToHierarchy userId child
                ] |> Plan.concurrentList
            let! ownPermissions, children =
                getOwnPermissions, getChildren
            return
                {   Node = hierarchy.Node
                    Children = children
                    Info = ownPermissions
                }
        }

    let rec showHierarchyWithPermissions userId fromParentId =
        plan {
            let! hierarchies = Domain.getHierarchy fromParentId
            let! withPermissions =
                [ for hierarchy in hierarchies -> addPermissionsToHierarchy userId hierarchy
                ] |> Plan.concurrentList
            for h in withPermissions do
                printfn "%O" h
        }

    let testCache userId doubleUp =
        plan {
            let! folders = Domain.getHierarchy None
            for h in folders do printfn "%O" h
            if doubleUp then
                let! folders = Domain.getHierarchy None
                for h in folders do printfn "%O" h
            for root in batch folders do
                for child in batch root.Children do
                    match child.Node with
                    | File f -> do! Domain.recycleFile userId f.FileId
                    | _ -> ()
            if not doubleUp then
                let! folders = Domain.getHierarchy None
                for h in folders do printfn "%O" h
        }

    let parseFolderId (idStr : string) =
        let succ, id = Int32.TryParse(idStr)
        if not succ then
            eprintfn "Misformatted id %s" idStr
            None
        else
            Some (FolderId id)

    member this.Bench description plan = bench description plan

    member this.Run() =
        let mutable looping = true
        let mutable userId =
            bench
                (sprintf "Becoming default user %s" DemoSetup.defaultUserName)
                (Domain.getUserByName DemoSetup.defaultUserName |> Plan.map Option.get)
        let planCommand (cmd : string) args =
            match cmd, args with
            | "dumb", [||] ->
                dumbMode <- true
                Plan.zero
            | "smart", [||] ->
                dumbMode <- false
                Plan.zero
            | "reset", [||] ->
                plan {
                    do! DemoSetup.setUpDemoData
                    let! id = Domain.getUserByName DemoSetup.defaultUserName |> Plan.map Option.get
                    userId <- id
                }
            | "become", [| name |] ->
                plan {
                    let! resolved = Domain.getUserByName name
                    match resolved with
                    | None -> eprintfn "No such user %s" name
                    | Some id -> userId <- id
                }
            | "ls", [||] ->
                showHierarchy None
            | "ls", [| idStr |] ->
                match parseFolderId idStr with
                | None -> Plan.zero
                | Some id ->
                    showHierarchy (Some id)
            | "lsp", [||] ->
                showHierarchyWithPermissions userId None
            | "lsp", [| idStr |] ->
                match parseFolderId idStr with
                | None -> Plan.zero
                | Some id ->
                    showHierarchyWithPermissions userId (Some id)
            | "rmdir", [| idStr |] ->
                match parseFolderId idStr with
                | None -> Plan.zero
                | Some id ->
                    Domain.recycleFolder userId id
            | "testcache", [| arg |] ->
                testCache userId (arg = "doubleup")
            | _ ->
                eprintfn "Unrecognized command."
                Plan.ret ()
        while looping do
            Console.Write("> ")
            let input = Console.ReadLine()
            if input = "quit" then
                looping <- false
            else
                let cmd, args =
                    let split = input.Split([|' '|], StringSplitOptions.RemoveEmptyEntries)
                    split.[0], Array.skip 1 split
                let plan = planCommand cmd args
                try
                    bench input plan
                with
                | :? AggregateException as e when e.InnerExceptions.Count = 1 ->
                    match e.InnerException with
                    | :? SecurityException as s ->
                        eprintfn "Security exception: %s" s.Message
                    | exn ->
                        eprintfn "Exception: %O" exn
                | :? SecurityException as s ->
                    eprintfn "Security exception: %s" s.Message
                | exn ->
                    eprintfn "Exception: %O" exn

[<EntryPoint>]
let main argv =
    // Configuration: appsettings.json + environment variables (which override).
    // The connection string for "rzsql" comes from ConnectionStrings:rzsql.
    let configuration =
        ConfigurationBuilder()
            .AddJsonFile("appsettings.json", optional = false)
            .AddEnvironmentVariables()
            .Build() :> IConfiguration

    // DI setup: just register IConfiguration. Rezoom.SQL's ConnectionProvider falls
    // back to a ConfigurationConnectionProvider built from this when no explicit
    // ConnectionProvider is registered, so no per-app wire-up is needed.
    // (This demo doesn't use PlanExecutor — its "round trips saved" stat requires
    // a custom ExecutionLog per call, built inline in Execution.fs.)
    let collection = ServiceCollection()
    collection.AddSingleton<IConfiguration>(configuration) |> ignore
    use provider = collection.BuildServiceProvider()
    let services = provider :> IServiceProvider

    DemoSetup.migrate(services)

    let repl = Repl(services)
    repl.Bench "Set up demo data" DemoSetup.setUpDemoData |> ignore
    repl.Run()
    0 // return an integer exit code
