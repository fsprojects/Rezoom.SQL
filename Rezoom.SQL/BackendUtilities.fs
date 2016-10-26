module Rezoom.SQL.BackendUtilities
open System
open System.Text
open System.Globalization
open System.Collections.Generic
open Rezoom.SQL.Mapping
open Rezoom.SQL

type Fragment = CommandFragment
type Fragments = Fragment seq

let simplifyFragments (fragments : Fragments) =
    seq {
        let mutable hasWhitespace = false
        let builder = StringBuilder()
        for fragment in fragments do
            match fragment with
            | CommandText text ->
                ignore <| builder.Append(text)
                hasWhitespace <- text.EndsWith(" ")
            | Whitespace ->
                if not hasWhitespace then ignore <| builder.Append(' ')
                hasWhitespace <- true
            | Parameter _
            | LocalName _ ->
                if builder.Length > 0 then
                    yield CommandText <| builder.ToString()
                    ignore <| builder.Clear()
                yield fragment
                hasWhitespace <- false
        if builder.Length > 0 then
            yield CommandText <| builder.ToString()
            ignore <| builder.Clear()
    }

let ws = Whitespace
let text str = CommandText str

let join separator (fragments : Fragments seq) =
    seq {
        let separator = CommandText separator
        let mutable first = true
        for element in fragments do
            if not first then yield separator
            else first <- false
            yield! element
    }

let join1 separator sequence = join separator (sequence |> Seq.map Seq.singleton)

