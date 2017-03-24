open System.Diagnostics
open Rezoom
open Rezoom.Execution
open FileSystem
open FileSystem.Domain
open FSharp.Control.Tasks.ContextInsensitive

type DemoExecutionLog() =
    inherit ExecutionLog()
    let mutable batchCount = 0
    let mutable anyErrands = false
    override __.OnPreparedErrand(_) = anyErrands <- true
    override __.OnEndStep() =
        if anyErrands then
            batchCount <- batchCount + 1
            anyErrands <- false
    member __.BatchCount = batchCount

let executeSmart (plan : 'a Plan) =
    task {
        let log = DemoExecutionLog()
        let config =
            { ExecutionConfig.Default with
                Log = log
            }
        let sw = Stopwatch()
        let! result = execute config plan
        return result, log.BatchCount
    } |> fun t -> t.Result

let executeDumb (plan : 'a Plan) =
    (DumbExecution.execute plan).Result

let execute plan = executeDumb plan

let bench description (plan : 'a Plan) =
    let stopwatch = Stopwatch()
    stopwatch.Start()
    let result, batchCount = execute plan
    stopwatch.Stop()
    printfn "Ran `%s` in %d ms with %d round trips"
        description stopwatch.ElapsedMilliseconds batchCount

[<EntryPoint>]
let main argv =
    DemoSetup.migrate()
    bench "Set up demo data" DemoSetup.setUpDemoData

    printfn "%A" argv
    0 // return an integer exit code
