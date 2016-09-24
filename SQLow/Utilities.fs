[<AutoOpen>]
module private SQLow.Utilities
open System
open System.Collections
open System.Collections.Generic

let inline (|?) opt def = defaultArg opt def

let rmap (f : 'a -> 'b) (list : 'a ResizeArray) =
    list |> Seq.map f |> ResizeArray

let toReadOnlyList (values : 'a seq) =
    ResizeArray(values) :> IReadOnlyList<_>

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

type SourceException(info : SourceInfo, msg) =
    inherit Exception(msg)
    member this.Info = info

let inline bug msg = failwith msg

let inline failAt (source : SourceInfo) (msg : string) =
    raise (SourceException(source, msg))

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

        
