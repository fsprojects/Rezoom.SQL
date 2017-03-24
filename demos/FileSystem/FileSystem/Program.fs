open System
open System.Diagnostics
open Rezoom
open FileSystem
open FileSystem.Execution

let execute plan = executeSmart plan

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
        let! hierarchy = Domain.getHierarchy fromParentId
        for h in hierarchy do
            printfn "%O" h
    }

let mainLoop () =
    let mutable looping = true
    let mutable userId =
        bench
            (sprintf "Becoming default user %s" DemoSetup.defaultUserName)
            (Domain.getUserByName DemoSetup.defaultUserName |> Plan.map Option.get)
    let planCommand (cmd : string) args =
        match cmd, args with
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
            let succ, id = Int32.TryParse(idStr)
            if not succ then
                eprintfn "Misformatted id %s" idStr
                Plan.ret ()
            else
                showHierarchy (Some (FolderId id))
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
            bench input plan

[<EntryPoint>]
let main argv =
    DemoSetup.migrate()
    bench "Set up demo data" DemoSetup.setUpDemoData
    mainLoop()
    0 // return an integer exit code
