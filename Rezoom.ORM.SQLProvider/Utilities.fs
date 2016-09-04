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
        member this.Keys = upcast [| key |]
        member this.TryGetValue(k, v) =
            if key.Equals(k, StringComparison.OrdinalIgnoreCase) then
                v <- value
                true
            else false
        member this.Values = upcast [| value |]

let ciSingle key value = SingleCIDictionary(key, value)   

let toReadOnlyList (values : 'a seq) =
    ResizeArray(values) :> IReadOnlyList<_>

type SourceException(info : SourceInfo, msg) =
    inherit Exception(msg)
    member this.Info = info

let inline failAt (source : SourceInfo) (msg : string) =
    raise (SourceException(source, msg))
