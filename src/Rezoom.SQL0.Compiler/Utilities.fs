[<AutoOpen>]
module Rezoom.SQL.Compiler.Utilities
open System
open System.Collections
open System.Collections.Generic

let inline (|?) opt def = defaultArg opt def
let inline (|??) opt def = match opt with | Some _ -> opt | None -> def

let inline rmap (f : 'a -> 'b) (list : 'a array) = Array.map f list

let toReadOnlyList (values : 'a seq) =
    ResizeArray(values) :> IReadOnlyList<_>

let toDictionary (key : 'a -> 'k) (values : 'a seq) =
    let d = Dictionary()
    for value in values do
        d.[key value] <- value
    d

let srcMap f (w : 'a WithSource) = w.Map(f)
let srcValue (w : 'a WithSource) = w.Value

[<GeneralizableValue>]
let emptyDictionary<'k, 'v> =
    { new IReadOnlyDictionary<'k, 'v> with
        member __.ContainsKey(_) = false
        member __.Count = 0
        member __.GetEnumerator() : IEnumerator<KeyValuePair<'k, 'v>> = Seq.empty.GetEnumerator()
        member __.GetEnumerator() : IEnumerator = upcast Seq.empty.GetEnumerator()
        member __.Item with get(k) = raise <| KeyNotFoundException()
        member __.TryGetValue(k, v) = false
        member __.Keys = Seq.empty
        member __.Values = Seq.empty
    }

let inline bug msg = failwith msg

let inline fail msg =
    raise (SQLCompilerException(msg))

let inline failAt (source : SourceInfo) (msg : string) =
    raise (SourceInfoException(msg, source))

type NameResolution<'a> =
    | Found of 'a
    | NotFound of string
    | Ambiguous of string

type Result<'x, 'err> =
    | Ok of 'x
    | Error of 'err

type ResultBuilder() =
    member inline this.Zero() = Ok ()
    member inline this.Bind(result : Result<'x, 'err>, next : 'x -> Result<'y, 'err>) =
        match result with
        | Error err -> Error err
        | Ok x -> next x
    member inline this.Combine(first : Result<'x, 'err>, next : unit -> Result<'y, 'err>) =
        match first with
        | Error err -> Error err
        | Ok _ -> next()
    member inline this.Return(x) = Ok x
    member inline this.ReturnFrom(x : Result<_, _>) = x
    member inline __.Delay(x : unit -> 'x) = x
    member inline __.Run(x : unit -> 'x) = x()

let result = ResultBuilder()

let resultAt source result =
    match result with
    | Ok x -> x
    | Error err -> failAt source err

let resultOk source result = resultAt source result |> ignore

let appendLists (left : 'x IReadOnlyList) (right : 'x IReadOnlyList) =
    { new IReadOnlyList<'x> with
        member __.Count = left.Count + right.Count
        member __.GetEnumerator() : 'x IEnumerator = (Seq.append left right).GetEnumerator()
        member __.GetEnumerator() : IEnumerator = upcast (Seq.append left right).GetEnumerator()
        member __.Item
            with get (index) =
                let leftCount = left.Count
                if index >= leftCount then right.[index - leftCount]
                else left.[index]
    }

type AmbiguousKeyException(msg) =
    inherit Exception(msg)

let appendDicts (left : IReadOnlyDictionary<'k, 'v>) (right : IReadOnlyDictionary<'k, 'v>) =
    { new IReadOnlyDictionary<'k, 'v> with
        member __.ContainsKey(key) = left.ContainsKey(key) || right.ContainsKey(key)
        member __.Count = left.Count + right.Count
        member __.GetEnumerator() : IEnumerator<KeyValuePair<'k, 'v>> = (Seq.append left right).GetEnumerator()
        member __.GetEnumerator() : IEnumerator = upcast (Seq.append left right).GetEnumerator()
        member __.Item
            with get(k) =
                let lsucc, lv = left.TryGetValue(k)
                let rsucc, rv = right.TryGetValue(k)
                if lsucc && rsucc then
                    raise <| AmbiguousKeyException(sprintf "Key %O is ambiguous" k)
                else if lsucc then
                    lv
                else if rsucc then
                    rv
                else raise <| KeyNotFoundException()
        member __.TryGetValue(k, v) =
            let lsucc, lv = left.TryGetValue(k)
            let rsucc, rv = right.TryGetValue(k)
            if lsucc && rsucc then
                raise <| AmbiguousKeyException(sprintf "Key %O is ambiguous" k)
            else if lsucc then
                v <- lv
                true
            else if rsucc then
                v <- rv
                true
            else false
        member __.Keys = Seq.append left.Keys right.Keys
        member __.Values = Seq.append left.Values right.Values
    }

let rec private insertionsOf y xs =
    match xs with
    | [] -> Seq.singleton [y]
    | x :: rest ->
        seq {
            yield y :: xs
            for rest in insertionsOf y rest -> x :: rest
        }

let rec permutations (xs : 'a list) =
    match xs with
    | [] -> Seq.singleton []
    | x :: rest -> permutations rest |> Seq.collect (insertionsOf x)
 
/// Translates from FParsec's position type to our own.
let internal translatePosition (pos : FParsec.Position) =
    { Index = int pos.Index; Line = int pos.Line; Column = int pos.Column }

let mapBy keyFunction sequence =
    sequence |> Seq.map (fun x -> keyFunction x, x) |> Map.ofSeq

let inline (+@+) x y =
    let h1 = match box x with | null -> 0 | _ -> x.GetHashCode()
    let h2 = match box y with | null -> 0 | _ -> y.GetHashCode()
    ((h1 <<< 5) + h1) ^^^ h2

/// State monad. Useful for complicated transforms on immutable structures.
type State<'st, 'a> = 'st -> 'st * 'a

module State =
    let runForOutputState input stateful = stateful input |> fst
    let runForOuputValue input stateful = stateful input |> snd

    let inline get state = state, state
    let inline put (newState : 'st) (_ : 'st) = newState, ()
    let inline ret x state = state, x
    let inline zero state = state, ()

    let inline map f stateful state =
        let newState, x = stateful state
        newState, f x

    let inline delay (f : unit -> State<'st, 'a>) state =
        f () state

    let inline bind (previous : State<'st, 'a>) (next : 'a -> State<'st, 'b>) state =
        let currentState, x = previous state
        next x currentState

    let inline combine (previous : State<'st, 'a>) (next : State<'st, 'b>) =
        previous >> fst >> next

    let inline forLoop xs (block : 'a -> State<'st, unit>) state =
        let mutable state = state
        for x in xs do
            let newState, () = block x state
            state <- newState
        state, ()

    let inline whileLoop cond (block : State<'st, unit>) state =
        let mutable state = state
        while cond() do
            let newState, () = block state
            state <- newState
        state, ()

    let inline tryWith (block : State<'st, 'a>) (catcher : exn -> State<'st, 'a>) state =
        try block state
        with | exn -> catcher exn state

    let inline tryFinally (block : State<'st, 'a>) fin state =
        try block state
        with | _ -> fin()

    let inline using disposable body (state : _ -> State<'st, 'a>) =
        use d = disposable
        body d state

type StatefulBuilder() =
    member inline this.Zero() : State<_, _> = State.zero
    member inline this.Return(x) : State<_, _> = State.ret x
    member inline this.ReturnFrom(st : State<_, _>) = st
    member inline this.Delay(f) : State<_, _> = State.delay f
    member inline this.Run(st : State<_, _>) = st
    member inline this.Bind(st, cont) : State<_, _> = State.bind st cont
    member inline this.Combine(st, cont) : State<_, _> = State.combine st cont
    member inline this.TryWith(st, catcher) : State<_, _> = State.tryWith st catcher
    member inline this.TryFinally(st, fin) : State<_, _> = State.tryFinally st fin
    member inline this.While(cond, body) : State<_, _> = State.whileLoop cond body
    member inline this.For(xs, body) : State<_, _> = State.forLoop xs body
    member inline this.Using(disposable, body) : State<_, _> = State.using disposable body

let stateful = StatefulBuilder()

let tryFindFirstDuplicateBy (selector : _ -> _) (xs : _ seq) =
    let set = HashSet()
    use enumer = xs.GetEnumerator()
    let mutable dup = None
    while enumer.MoveNext() && dup.IsNone do
        let x = enumer.Current
        if not (set.Add(selector x)) then
            dup <- Some x
    dup
