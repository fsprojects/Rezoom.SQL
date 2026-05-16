module FileSystem.Execution
open System
open System.Collections.Generic
open System.Threading
open System.Threading.Tasks
open Rezoom
open Rezoom.Execution

// The usual way of executing plans, more or less.
// Here we define a custom logger.

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

// Now we write a wrapper function around Rezoom.Execution.execute
// that uses our logger in the config and records the # of batches.

let executeSmart (plan : 'a Plan) =
    task {
        let log = DemoExecutionLog()
        let config =
            { ExecutionConfig.Default with
                Instance = fun () -> ExecutionInstance(log)
            }
        let! result = execute config plan
        return result, log.BatchCount
    } |> fun t -> t.Result

// The rest of this file is a manual re-implementation of Rezoom.Execution.execute
// which is dumb. That is, it does not batch commands and it does not cache anything.

// Normally this would not be in a project, it is only written here so we run plans in "dumb" mode
// and see how many round trips we saved in "smart" mode.

type private DumbPlanContext(services : IServiceProvider) =
    inherit PlanContext()
    let instances = Dictionary<Type, obj>()
    let globals = Stack<_>()
    override __.Services = services
    override this.GetPlanLocal<'f, 'a when 'f :> PlanLocal<'a> and 'f : (new : unit -> 'f)>() =
        let ty = typeof<'f>
        let succ, existing = instances.TryGetValue(ty)
        if succ then Unchecked.unbox existing else
        let factory = new 'f()
        let instance = factory.Create(this)
        match factory.Lifetime with
        | Lifetime.Execution ->
            instances.Add(ty, box instance)
            globals.Push(fun state ->
                factory.Dispose(state, instance)
                ignore <| instances.Remove(ty))
            instance
        | _ ->
            instance
    static member private ClearStack(stack : _ Stack, state) =
        let mutable exn = null
        while stack.Count > 0 do
            let disposer = stack.Pop()
            try
                disposer state
            with
            | e ->
                if isNull exn then exn <- e
                else exn <- AggregateException(exn, e)
        if not (isNull exn) then raise exn
    member __.ClearLocals(state) = ()
    member __.SetSuccessful() = ()
    member this.Dispose() =
        try
            this.ClearLocals(ExecutionSuccess)
        finally
            DumbPlanContext.ClearStack(globals, ExecutionSuccess)
    interface IDisposable with
        member this.Dispose() = this.Dispose()

let executeDumb (plan : 'a Plan) =
    task {
        let services =
            { new IServiceProvider with
                member __.GetService(_) = null }
        let token = CancellationToken.None
        use context = new DumbPlanContext(services)
        let mutable errandCount = 0
        let mutable plan = plan
        let mutable result = None
        while result = None do
            match plan with
            | Step(requests, resume) ->
                let tasks = ResizeArray()
                let pendingResponses =
                    requests.Map(fun e ->
                        let task = lazy (e.PrepareUntyped(context) token)
                        tasks.Add(task)
                        fun () -> task.Value)
                errandCount <- errandCount + tasks.Count
                for task in tasks do // run tasks in series
                    let! _ = task.Value
                    ()
                plan <- pendingResponses.Map(fun t -> t().Result |> RetrievalSuccess) |> resume
            | Result r ->
                result <- Some r
        return Option.get result, errandCount
    } |> fun t -> t.Result
