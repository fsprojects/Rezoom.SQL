namespace StaticQL
open System

/// The position in the source query that a syntactic element appeared.
type SourcePosition =
    {   Index : int64
        Line : int64
        Column : int64
    }
    static member Zero =
        {   Index = 0L
            Line = 0L
            Column = 0L
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
    static member Zero =
        {   StartPosition = SourcePosition.Zero
            EndPosition = SourcePosition.Zero
        }
    member this.ShowInSource(source : string) =
        source.Substring
            ( int this.StartPosition.Index
            , int (this.EndPosition.Index - this.StartPosition.Index)
            )
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

type SourceException(msg : string, pos : SourceInfo) =
    inherit Exception(msg)
    member this.SourceInfo = pos