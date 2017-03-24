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

let exec description (plan : 'a Plan) =
    task {
        let log = DemoExecutionLog()
        let config =
            { ExecutionConfig.Default with
                Log = log
            }
        let! result = execute config plan
        printfn "Ran `%s` in %d batches" description log.BatchCount
        return result
    } |> fun t -> t.Result

[<EntryPoint>]
let main argv =
    DemoSetup.migrate()
    exec "Set up demo data" DemoSetup.setUpDemoData

    printfn "%A" argv
    0 // return an integer exit code
