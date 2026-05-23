module Rezoom.SQL.Mapping.Levenshtein
open System

module Slow =
    // slow but obvious recursive implementation
    let rec private distanceList (charsEqual : char -> char -> bool) (a : char list) (b : char list) =
        match a, b with
        | [], x
        | x, [] -> x.Length
        | (aChr :: aSuf), (bChr :: bSuf) ->
            let replace = distanceList charsEqual aSuf bSuf + if charsEqual aChr bChr then 0 else 1
            let insertOrDelete = 1 + min (distanceList charsEqual aSuf b) (distanceList charsEqual bSuf a)
            min replace insertOrDelete
    let distance (a : string) (b : string) =
        distanceList (=) (List.ofArray (a.ToCharArray())) (List.ofArray (b.ToCharArray()))
    let distanceCI (a : string) (b : string) =
        distanceList (fun c1 c2 -> Char.ToUpperInvariant(c1) = Char.ToUpperInvariant(c2)) (List.ofArray (a.ToCharArray())) (List.ofArray (b.ToCharArray()))

let inline private distanceCore (charsEqual : char -> char -> bool) (a : string) (b : string) =
    let distances = Array2D.zeroCreate (a.Length + 1) (b.Length + 1)
    // fill in left col with dist to empty b for all possible a prefix lengths
    for aPrefixLen = 0 to a.Length do
        distances.[aPrefixLen, 0] <- aPrefixLen
    // fill in top row with dist to empty a for all possible b prefix lengths
    for bPrefixLen = 0 to b.Length do
        distances.[0, bPrefixLen] <- bPrefixLen
    for aCharIndex = 0 to a.Length - 1 do
        for bCharIndex = 0 to b.Length - 1 do
            // the prefix-length coordinates we are filling in are +1
            // because when looking at the 1st character of A, the length of that prefix is 1
            let aPrefixLen = aCharIndex + 1
            let bPrefixLen = bCharIndex + 1
            let deleteOrInsert =
                1 + min (distances.[aPrefixLen, bCharIndex]) (distances.[aCharIndex, bPrefixLen]) 
            let replace =
                distances.[aCharIndex, bCharIndex] + if charsEqual a.[aCharIndex] b.[bCharIndex] then 0 else 1
            distances.[aPrefixLen, bPrefixLen] <-
                min replace deleteOrInsert
    distances.[a.Length, b.Length]

let distance = distanceCore (=)
let distanceCI = distanceCore (fun c1 c2 -> Char.ToUpperInvariant(c1) = Char.ToUpperInvariant(c2))

let mistakeCandidates (input : string) (validStrings : string seq) =
    validStrings |> Seq.filter (fun v -> distanceCI v input <= 1)
