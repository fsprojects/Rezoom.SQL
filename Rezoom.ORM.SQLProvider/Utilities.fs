[<AutoOpen>]
module private Rezoom.ORM.SQLProvider.Utilities
open System
open System.Collections
open System.Collections.Generic

let ciDictBy key values =
    let d = new Dictionary<string, _>(StringComparer.OrdinalIgnoreCase)
    for value in values do
        d.[key value] <- value
    d :> IReadOnlyDictionary<_, _>

let ciContains needle (haystack : string) =
    haystack.IndexOf(needle, StringComparison.OrdinalIgnoreCase) >= 0

type SingleCIDictionary<'a>(key : string, value : 'a) =
    interface IReadOnlyDictionary<string, 'a> with
        member this.ContainsKey(k) = key.Equals(k, StringComparison.OrdinalIgnoreCase)
        member this.Count = 1
        member this.GetEnumerator() = ([| KeyValuePair(key, value) |] |> Seq.ofArray).GetEnumerator() :> IEnumerator<_>
        member this.GetEnumerator() = [| KeyValuePair(key, value) |].GetEnumerator()
        member this.Item
            with get (k) =
                if key.Equals(k, StringComparison.OrdinalIgnoreCase) then value
                else raise <| KeyNotFoundException()
        member this.TryGetValue(k, v) =
            if key.Equals(k, StringComparison.OrdinalIgnoreCase) then
                v <- value
                true
            else false
        member this.Keys = upcast [| key |]
        member this.Values = upcast [| value |]

let ciSingle key value = SingleCIDictionary(key, value)   

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

let inline failAt (source : SourceInfo) (msg : string) =
    raise (SourceException(source, msg))
