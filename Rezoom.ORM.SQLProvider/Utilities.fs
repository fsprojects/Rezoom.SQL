[<AutoOpen>]
module private Rezoom.ORM.SQLProvider.Utilities
open System
open System.Collections.Generic

let ciDictBy key values =
    let d = new Dictionary<string, _>(StringComparer.OrdinalIgnoreCase)
    for value in values do
        d.[key value] <- value
    d :> IReadOnlyDictionary<_, _>

let ciContains needle (haystack : string) =
    haystack.IndexOf(needle, StringComparison.OrdinalIgnoreCase) >= 0

type SourceException(info : SourceInfo, msg) =
    inherit Exception(msg)
    member this.Info = info

let inline failAt (source : SourceInfo) (msg : string) =
    raise (SourceException(source, msg))
