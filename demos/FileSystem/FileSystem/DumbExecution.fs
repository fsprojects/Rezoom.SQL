module FileSystem.DumbExecution
open System
open System.Collections.Generic
open System.Threading
open System.Threading.Tasks
open FSharp.Control.Tasks.ContextInsensitive
open Rezoom

type private DumbServiceContext(config : IServiceConfig) =
    inherit ServiceContext()
    let services = Dictionary<Type, obj>()
    let globals = Stack<_>()
    let mutable totalSuccess = false
    override __.Configuration = config
    override this.GetService<'f, 'a when 'f :> ServiceFactory<'a> and 'f : (new : unit -> 'f)>() =
        let ty = typeof<'f>
        let succ, service = services.TryGetValue(ty)
        if succ then Unchecked.unbox service else
        let factory = new 'f()
        let service = factory.CreateService(this)
        match factory.ServiceLifetime with
        | ServiceLifetime.ExecutionLocal ->
            services.Add(ty, box service)
            globals.Push(fun state ->
                factory.DisposeService(state, service)
                ignore <| services.Remove(ty))
            service
        | _ ->
            service
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
    member __.SetSuccessful() = totalSuccess <- true
    member this.Dispose() =
        let state = if totalSuccess then ExecutionSuccess else ExecutionFault
        try
            this.ClearLocals(state)
        finally
            DumbServiceContext.ClearStack(globals, state)
    interface IDisposable with
        member this.Dispose() = this.Dispose()

let execute (plan : 'a Plan) =
    task {
        let config = { new IServiceConfig with member __.TryGetConfig() = None }
        let token = CancellationToken.None
        use context = new DumbServiceContext(config)
        let mutable errandCount = 0
        let mutable plan = plan
        let mutable result = None
        while result = None do
            match plan with
            | Step(requests, resume) ->
                let tasks = ResizeArray()
                let pendingResponses =
                    requests.Map(fun e ->
                        let task = e.PrepareUntyped(context) token
                        tasks.Add(task)
                        task)
                errandCount <- errandCount + tasks.Count
                let! _ = Task.WhenAll(tasks)
                plan <- pendingResponses.Map(fun t -> RetrievalSuccess t.Result) |> resume
            | Result r ->
                result <- Some r
        return Option.get result, errandCount
    }


