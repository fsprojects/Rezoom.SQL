namespace Rezoom.SQL.Mapping
open System
open System.Data
open System.Collections.Generic
open Rezoom

type CommandFragment =
    /// A name which should be localized to this command for batching.
    /// For example, if the command creates a temp table, the real name should be chosen dynamically
    /// so it doesn't break when the command is batched with others that create the same-named temp table.
    | LocalName of string
    /// Chunk of raw SQL text.
    | CommandText of string
    /// References parameter by index.
    | Parameter of int
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
        CommandFragment.Stringize(" ", "", fragments)

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

type CommandParameter =
    | ListParameter of DbType * Array
    | ScalarParameter of DbType * obj

