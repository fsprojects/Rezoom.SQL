namespace Rezoom.SQL
open System

/// The position in the source query that a syntactic element appeared.
type SourcePosition =
    {   Index : int
        Line : int
        Column : int
    }
    static member Invalid =
        {   Index = -1
            Line = -1
            Column = -1
        }

type ParsingException(msg, pos : SourcePosition) =
    inherit Exception(msg)
    member this.Position = pos

/// The span of (start, end) positions in the source file
/// that a syntactic element occupies.
type SourceInfo =
    {   StartPosition : SourcePosition
        EndPosition : SourcePosition
    }
    static member Invalid =
        {   StartPosition = SourcePosition.Invalid
            EndPosition = SourcePosition.Invalid
        }
    static member private ContextLength = 10
    static member private ContextBefore(source : string) =
        let mutable i = source.Length - 1
        let mutable inWord = false
        let mutable boundaryCount = 0
        while i >= 0 && boundaryCount < SourceInfo.ContextLength do
            let inWordNow = Char.IsLetterOrDigit(source.[i])
            if inWord <> inWordNow then
                boundaryCount <- boundaryCount + 1
            inWord <- inWordNow
            i <- i - 1
        i <- min (i + 1) (source.Length - 1)
        source.Substring(i, source.Length - i)
    static member private ContextAfter(source : string) =
        let mutable i = 0
        let mutable inWord = false
        let mutable boundaryCount = 0
        while i < source.Length && boundaryCount < SourceInfo.ContextLength do
            let inWordNow = Char.IsLetterOrDigit(source.[i])
            if inWord <> inWordNow then
                boundaryCount <- boundaryCount + 1
            inWord <- inWordNow
            i <- i + 1
        i <- max 0 (i - 1)
        source.Substring(0, i)
            
    static member private Emphasize(source : string) =
        " ⇨ " + source + " ⇦ "
    member this.ShowInSource(source : string) =
        if
            this.StartPosition.Index < 0
            || this.EndPosition.Index < 0
            || this.StartPosition.Index >= int source.Length
            || this.EndPosition.Index >= int source.Length
        then
            "(no known source (possibly generated code))"
        else
            let context = 20
            let before = SourceInfo.ContextBefore(source.Substring(0, this.StartPosition.Index))
            let after = SourceInfo.ContextAfter(source.Substring(this.EndPosition.Index))
            let middle = source.Substring(this.StartPosition.Index, this.EndPosition.Index - this.StartPosition.Index)
            before + SourceInfo.Emphasize(middle) + after
    static member OfPosition(pos : SourcePosition) =
        {   StartPosition = pos
            EndPosition = pos
        }
    static member Between(left : SourceInfo, right : SourceInfo) =
        {   StartPosition = min left.EndPosition right.EndPosition
            EndPosition = max left.StartPosition right.StartPosition
        }

/// `'a` with the positions in source that it spanned.
type WithSource<'a> =
    {   /// The position in source of the syntactic element
        Source : SourceInfo
        /// The syntactic element
        Value : 'a
    }

type SourceInfoException(msg : string, pos : SourceInfo) =
    inherit Exception(msg)
    member this.SourceInfo = pos

type SourceException(msg, pos : SourceInfo, source, fileName) =
    inherit Exception
        ( msg
        + "\r\n"
        + fileName
        + "("
        + string pos.StartPosition.Line
        + ","
        + string pos.StartPosition.Column
        + "): "
        + pos.ShowInSource(source)
        )
    member __.FileName = fileName
    member __.Reason = msg
    member __.FullSourceContext = source
    member __.SourceContext = pos.ShowInSource(source)

[<AutoOpen>]
module SourceInfoModule =
    let inline catchSource fileName source f =
        try f()
        with
        | :? SourceInfoException as exn ->
            raise (SourceException(exn.Message, exn.SourceInfo, source, fileName))