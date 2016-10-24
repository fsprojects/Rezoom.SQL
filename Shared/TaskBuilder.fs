// TaskBuilder.fs - TPL task computation expressions for F#
//
// Written in 2016 by Robert Peele (humbobst@gmail.com)
//
// To the extent possible under law, the author(s) have dedicated all copyright and related and neighboring rights
// to this software to the public domain worldwide. This software is distributed without any warranty.
//
// You should have received a copy of the CC0 Public Domain Dedication along with this software.
// If not, see <http://creativecommons.org/publicdomain/zero/1.0/>.

namespace FSharp.Control.Tasks
open System
open System.Threading.Tasks
open System.Runtime.CompilerServices

// This module is not really obsolete, but it's not intended to be referenced directly from user code.
// However, it can't be private because it is used within inline functions that *are* user-visible.
// Marking it as obsolete is a workaround to hide it from auto-completion tools.
[<Obsolete>]
module TaskBuilder =
    /// Represents the state of a computation:
    /// either awaiting something with a continuation,
    /// or completed with a return value.
    type Step<'a> =
        | Await of ICriticalNotifyCompletion * (unit -> Step<'a>)
        | Return of 'a
    /// Implements the machinery of running a `Step<'m, 'm>` as a `Task<'m>`.
    and StepStateMachine<'a>(firstStep) as this =
        let methodBuilder = AsyncTaskMethodBuilder<'a>()
        /// The continuation we left off awaiting on our last MoveNext().
        let mutable continuation = fun () -> firstStep
        /// Return true if we should call `AwaitOnCompleted` on the current awaitable.
        let nextAwaitable() =
            try
                match continuation() with
                | Return r ->
                    methodBuilder.SetResult(r)
                    null
                | Await (await, next) ->
                    continuation <- next
                    await
            with
            | exn ->
                methodBuilder.SetException(exn)
                null
        let mutable self = this

        /// Start execution as a `Task<'m>`.
        member __.Run() =
            methodBuilder.Start(&self)
            methodBuilder.Task
    
        interface IAsyncStateMachine with
            /// Proceed to one of three states: result, failure, or awaiting.
            /// If awaiting, MoveNext() will be called again when the awaitable completes.
            member __.MoveNext() =
                let mutable await = nextAwaitable()
                if not (isNull await) then
                    // Tell the builder to call us again when this thing is done.
                    methodBuilder.AwaitUnsafeOnCompleted(&await, &self)    
            member __.SetStateMachine(_) = () // Doesn't really apply since we're a reference type.

    /// Used to represent no-ops like the implicit empty "else" branch of an "if" expression.
    let inline zero() = Return ()

    /// Used to return a value.
    let inline ret (x : 'a) = Return x

    // The following flavors of `bind` are for sequencing tasks with the continuations
    // that should run following them. They all follow pretty much the same formula.

    let inline bindTask (task : 'a Task) (continuation : 'a -> Step<'b>) =
        let taskAwaiter = task.GetAwaiter()
        if taskAwaiter.IsCompleted then // Proceed to the next step based on the result we already have.
            taskAwaiter.GetResult() |> continuation
        else // Await and continue later when a result is available.
            Await (taskAwaiter, (fun () -> taskAwaiter.GetResult() |> continuation))

    let inline bindVoidTask (task : Task) (continuation : unit -> Step<'b>) =
        let taskAwaiter = task.GetAwaiter()
        if taskAwaiter.IsCompleted then continuation() else
        Await (taskAwaiter, continuation)

    let inline bindConfiguredTask (task : 'a ConfiguredTaskAwaitable) (continuation : 'a -> Step<'b>) =
        let taskAwaiter = task.GetAwaiter()
        if taskAwaiter.IsCompleted then
            taskAwaiter.GetResult() |> continuation
        else
            Await (taskAwaiter, (fun () -> taskAwaiter.GetResult() |> continuation))

    let inline bindVoidConfiguredTask (task : ConfiguredTaskAwaitable) (continuation : unit -> Step<'b>) =
        let taskAwaiter = task.GetAwaiter()
        if taskAwaiter.IsCompleted then continuation() else
        Await (taskAwaiter, continuation)

    let inline
        bindGenericAwaitable< ^a, ^b, ^c when ^a : (member GetAwaiter : unit -> ^b) and ^b :> ICriticalNotifyCompletion >
        (awt : ^a) (continuation : unit -> Step< ^c >) =
        let taskAwaiter = (^a : (member GetAwaiter : unit -> ^b)(awt))
        Await (taskAwaiter, continuation)

    /// Chains together a step with its following step.
    /// Note that this requires that the first step has no result.
    /// This prevents constructs like `task { return 1; return 2; }`.
    let rec combine (step : Step<unit>) (continuation : unit -> Step<'b>) =
        match step with
        | Return _ -> continuation()
        | Await (awaitable, next) ->
            Await(awaitable, fun () -> combine (next()) continuation)

    /// Builds a step that executes the body while the condition predicate is true.
    let inline whileLoop (cond : unit -> bool) (body : unit -> Step<unit>) =
        if cond() then
            // Create a self-referencing closure to test whether to repeat the loop on future iterations.
            let rec repeat () =
                if cond() then combine (body()) repeat
                else zero()
            // Run the body the first time and chain it to the repeat logic.
            combine (body()) repeat
        else zero()

    /// Wraps a step in a try/with. This catches exceptions both in the evaluation of the function
    /// to retrieve the step, and in the continuation of the step (if any).
    let rec tryWith(step : unit -> Step<'a>) (catch : exn -> Step<'a>) =
        try
            match step() with
            | Return _ as i -> i
            | Await (awaitable, next) -> Await (awaitable, fun () -> tryWith next catch)
        with
        | exn -> catch exn

    /// Wraps a step in a try/finally. This catches exceptions both in the evaluation of the function
    /// to retrieve the step, and in the continuation of the step (if any).
    let rec tryFinally (step : unit -> Step<'a>) fin =
        let step =
            try step()
            // Important point: we use a try/with, not a try/finally, to implement tryFinally.
            // The reason for this is that if we're just building a continuation, we definitely *shouldn't*
            // execute the `fin()` part yet -- the actual execution of the asynchronous code hasn't completed!
            with
            | _ ->
                fin()
                reraise()
        match step with
        | Return _ as i ->
            fin()
            i
        | Await (awaitable, next) ->
            Await (awaitable, fun () -> tryFinally next fin)

    /// Implements a using statement that disposes `disp` after `body` has completed.
    let inline using (disp : #IDisposable) (body : _ -> Step<'a>) =
        // A using statement is just a try/finally with the finally block disposing if non-null.
        tryFinally
            (fun () -> body disp)
            (fun () -> if not (isNull (box disp)) then disp.Dispose())

    /// Implements a loop that runs `body` for each element in `sequence`.
    let forLoop (sequence : 'a seq) (body : 'a -> Step<unit>) =
        // A for loop is just a using statement on the sequence's enumerator...
        using (sequence.GetEnumerator())
            // ... and its body is a while loop that advances the enumerator and runs the body on each element.
            (fun e -> whileLoop e.MoveNext (fun () -> body e.Current))

    /// Runs a step as a task -- with a short-circuit for immediately completed steps.
    let inline run (firstStep : unit -> Step<'a>) =
        try
            match firstStep() with
            | Return x -> Task.FromResult(x)
            | Await _ as step ->
                StepStateMachine<'a>(step).Run()
        // Any exceptions should go on the task, rather than being thrown from this call.
        // This matches C# behavior where you won't see an exception until awaiting the task,
        // even if it failed before reaching the first "await".
        with
        | exn -> Task.FromException<_>(exn)

    type TaskBuilder() =
        // These methods are consistent between the two builders.
        // Unfortunately, inline members do not work with inheritance.
        member inline __.Delay(f : unit -> Step<_>) = f
        member inline __.Run(f : unit -> Step<'m>) = run f
        member inline __.Zero() = zero()
        member inline __.Return(x) = ret x
        member inline __.ReturnFrom(task) = bindConfiguredTask task ret
        member inline __.ReturnFrom(task) = bindVoidConfiguredTask task ret
        member inline __.ReturnFrom(yld : YieldAwaitable) = bindGenericAwaitable yld ret
        member inline __.Combine(step, continuation) = combine step continuation
        member inline __.Bind(task, continuation) = bindConfiguredTask task continuation
        member inline __.Bind(task, continuation) = bindVoidConfiguredTask task continuation
        member inline __.Bind(yld : YieldAwaitable, continuation) = bindGenericAwaitable yld continuation
        member inline __.While(condition, body) = whileLoop condition body
        member inline __.For(sequence, body) = forLoop sequence body
        member inline __.TryWith(body, catch) = tryWith body catch
        member inline __.TryFinally(body, fin) = tryFinally body fin
        member inline __.Using(disp, body) = using disp body
        // End of consistent methods -- the following methods are different between
        // `TaskBuilder` and `ContextInsensitiveTaskBuilder`!

        member inline __.ReturnFrom(task : _ Task) =
            bindTask task ret
        member inline __.ReturnFrom(task : Task) =
            bindVoidTask task ret
        member inline __.Bind(task : _ Task, continuation) =
            bindTask task continuation
        member inline __.Bind(task : Task, continuation) =
            bindVoidTask task continuation

    type ContextInsensitiveTaskBuilder() =
        // These methods are consistent between the two builders.
        // Unfortunately, inline members do not work with inheritance.
        member inline __.Delay(f : unit -> Step<_>) = f
        member inline __.Run(f : unit -> Step<'m>) = run f
        member inline __.Zero() = zero()
        member inline __.Return(x) = ret x
        member inline __.ReturnFrom(task) = bindConfiguredTask task ret
        member inline __.ReturnFrom(task) = bindVoidConfiguredTask task ret
        member inline __.ReturnFrom(yld : YieldAwaitable) = bindGenericAwaitable yld ret
        member inline __.Combine(step, continuation) = combine step continuation
        member inline __.Bind(task, continuation) = bindConfiguredTask task continuation
        member inline __.Bind(task, continuation) = bindVoidConfiguredTask task continuation
        member inline __.Bind(yld : YieldAwaitable, continuation) = bindGenericAwaitable yld continuation
        member inline __.While(condition, body) = whileLoop condition body
        member inline __.For(sequence, body) = forLoop sequence body
        member inline __.TryWith(body, catch) = tryWith body catch
        member inline __.TryFinally(body, fin) = tryFinally body fin
        member inline __.Using(disp, body) = using disp body
        // End of consistent methods -- the following methods are different between
        // `TaskBuilder` and `ContextInsensitiveTaskBuilder`!

        member inline __.ReturnFrom(task : _ Task) =
            bindConfiguredTask (task.ConfigureAwait(continueOnCapturedContext = false)) ret
        member inline __.ReturnFrom(task : Task) =
            bindVoidConfiguredTask (task.ConfigureAwait(continueOnCapturedContext = false)) ret
        member inline __.Bind(task : _ Task, continuation) =
            bindConfiguredTask (task.ConfigureAwait(continueOnCapturedContext = false)) continuation
        member inline __.Bind(task : Task, continuation) =
            bindVoidConfiguredTask (task.ConfigureAwait(continueOnCapturedContext = false)) continuation

// Don't warn about our use of the "obsolete" module we just defined (see notes at start of file).
#nowarn "44"

[<AutoOpen>]
module ContextSensitive =
    /// Builds a `System.Threading.Tasks.Task<'a>` similarly to a C# async/await method.
    /// Use this like `task { let! taskResult = someTask(); return taskResult.ToString(); }`.
    let task = TaskBuilder.TaskBuilder()

module ContextInsensitive =
    /// Builds a `System.Threading.Tasks.Task<'a>` similarly to a C# async/await method, but with
    /// all awaited tasks automatically configured *not* to resume on the captured context.
    /// This is often preferable when writing library code that is not context-aware, but undesirable when writing
    /// e.g. code that must interact with user interface controls on the same thread as its caller.
    let task = TaskBuilder.ContextInsensitiveTaskBuilder()

