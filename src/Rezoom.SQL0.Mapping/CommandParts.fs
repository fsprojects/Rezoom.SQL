namespace Rezoom.SQL.Mapping
open System
open System.Data
open System.Collections.Generic
open Rezoom

[<NoComparison>]
type CommandFragment =
    /// A name which should be localized to this command for batching.
    /// For example, if the command creates a temp table, the real name should be chosen dynamically
    /// so it doesn't break when the command is batched with others that create the same-named temp table.
    | LocalName of string
    /// Chunk of raw SQL text.
    | CommandText of string
    /// References parameter by index.
    | Parameter of int
    /// Directly specifies parameter value.
    | InlineParameter of DbType * obj
    /// At least one unit of whitespace.
    | Whitespace
    /// Whitespace, preferably a line break.
    | LineBreak
    /// Increase indentation level for following line breaks.
    | Indent
    /// Decrease indentation level for following line breaks.
    | Outdent
    /// Converts a sequence of fragments *without parameters* to a string.
    static member Stringize(newline : string, indent : string, fragments : CommandFragment seq) =
        seq {
            let mutable indentation = ""
            let mutable pendingLine = false
            for fragment in fragments do
                let text =
                    match fragment with
                    | LocalName name -> Some name
                    | CommandText text -> Some text
                    | Whitespace -> Some " "
                    | Parameter i -> Some ("@P" + string i)
                    | InlineParameter (_, p) -> Some ("@{" + string p + "}")
                    | LineBreak ->
                        pendingLine <- true
                        None
                    | Indent ->
                        indentation <- indentation + indent
                        None
                    | Outdent ->
                        indentation <- indentation.Substring(0, max 0 (indentation.Length - indent.Length))
                        None
                match text with
                | None -> ()
                | Some text ->
                    if pendingLine then
                        pendingLine <- false
                        yield newline
                        yield indentation
                    yield text
        } |> String.concat ""
    static member Stringize(fragments : CommandFragment seq) =
        CommandFragment.Stringize("\n", "", fragments)

[<AbstractClass>]
type ResultSetProcessor() =
    /// Start processing a result set.
    abstract member BeginResultSet : IDataReader -> unit
    /// Process a single row of the result set.
    abstract member ProcessRow : unit -> unit
    /// Obtain the result object after processing *all* result sets.
    abstract member ObjectGetResult : unit -> obj

[<AbstractClass>]
type ResultSetProcessor<'output>() =
    inherit ResultSetProcessor()
    abstract member GetResult : unit -> 'output
    override this.ObjectGetResult() = this.GetResult() |> box

[<NoComparison>]
[<NoEquality>]
type CommandData =
    {   ConnectionName : string
        Identity : string
        Fragments : CommandFragment IReadOnlyList
        DependencyMask : BitMask
        InvalidationMask : BitMask
        Cacheable : bool
        ResultSetCount : int option
    }

type CommandCategory = CommandCategory of connectionName : string

[<NoComparison>]
[<CustomEquality>]
type CommandParameter =
    | ListParameter of DbType * Array
    | ScalarParameter of DbType * obj
    | RawSQLParameter of CommandFragment array
    member this.Equals(other : CommandParameter) =
        match this, other with
        | ListParameter (ty1, arr1), ListParameter (ty2, arr2) ->
            ty1 = ty2 && arr1.Length = arr2.Length && (
                let mutable all = true
                let mutable i = 0
                while all && i < arr1.Length do
                    let e1 = arr1.GetValue(i)
                    let e2 = arr2.GetValue(i)
                    all <- EqualityComparer<obj>.Default.Equals(e1, e2)
                    i <- i + 1
                all
            )

        | ScalarParameter (ty1, obj1), ScalarParameter (ty2, obj2) ->
            ty1 = ty2 && EqualityComparer<obj>.Default.Equals(obj1, obj2)

        | RawSQLParameter frags1, RawSQLParameter frags2 -> frags1 = frags2

        | _ -> false
    override this.Equals(other : obj) =
        match other with
        | :? CommandParameter as p -> this.Equals(p)
        | _ -> false
    override this.GetHashCode() =
        let mutable h = 0
        match this with
        | ScalarParameter (ty, o) ->
            h <- ((h <<< 5) + h) ^^^ (hash o ^^^ hash ty)
        | ListParameter (ty, os) ->
            h <- ((h <<< 5) + h) ^^^ hash ty
            for o in os do
                h <- ((h <<< 5) + h) ^^^ hash o
        | RawSQLParameter frags ->
            for frag in frags do
                h <- ((h <<< 5) + h) ^^^ hash frag
        h
    interface IEquatable<CommandParameter> with
        member this.Equals(other) = this.Equals(other)


