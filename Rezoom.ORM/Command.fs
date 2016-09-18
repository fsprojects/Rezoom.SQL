namespace Rezoom.ORM
open System
open System.Data
open System.Collections.Generic
open Rezoom.ORM.CodeGeneration

type CommandFragment =
    /// A name which should be localized to this command for batching.
    /// For example, if the command creates a temp table, the real name should be chosen dynamically
    /// so it doesn't break when the command is batched with others that create the same-named temp table.
    | LocalName of string
    /// Chunk of raw SQL text.
    | CommandText of string
    /// References parameter by index.
    | Parameter of int

[<AbstractClass>]
type Command() =
    abstract member Fragments : CommandFragment IReadOnlyList
    abstract member Parameters : obj IReadOnlyList
    
    abstract member BeginResultSet : IDataReader -> unit
    abstract member ProcessRow : unit -> unit
    abstract member GetResultObject : unit -> obj

/// Represents multiple result sets as the output from a single command.
[<AbstractClass>]
type ResultSets() =
    abstract member AllResultSets : obj seq

type ResultSets<'a, 'b>(a : 'a, b : 'b) =
    inherit ResultSets()
    member __.ResultSet1 = a
    member __.ResultSet2 = b
    override __.AllResultSets =
        Seq.ofArray [| box a; box b |]

type ResultSets<'a, 'b, 'c>(a : 'a, b : 'b, c : 'c) =
    inherit ResultSets()
    member __.ResultSet1 = a
    member __.ResultSet2 = b
    member __.ResultSet3 = c
    override __.AllResultSets =
        Seq.ofArray [| box a; box b; box c |]

/// A command which can be expected to produce `'output` when run.
[<AbstractClass>]
type Command<'output>() =
    inherit Command()
    abstract member GetResult : unit -> 'output

type Command1<'a>(fragments, parameters) =
    inherit Command<'a>()
    let reader = ReaderTemplate<'a>.Template().CreateReader()
    let mutable row = Unchecked.defaultof<Row>
    let result = lazy reader.ToEntity()
    override __.Fragments = fragments
    override __.Parameters = parameters
    override __.BeginResultSet(dataReader) =
        reader.ProcessColumns(DataReader.columnMap(dataReader))
        row <- DataReader.DataReaderRow(dataReader)
    override __.ProcessRow() =
        reader.Read(row)
    override __.GetResultObject() = upcast result.Value
    override __.GetResult() = result.Value

type Command2<'a, 'b>(fragments, parameters) =
    inherit Command<ResultSets<'a, 'b>>()
    let aReader = ReaderTemplate<'a>.Template().CreateReader()
    let bReader = ReaderTemplate<'b>.Template().CreateReader()
    let mutable row = Unchecked.defaultof<Row>
    let mutable readers =
        [
            Unchecked.defaultof<EntityReader>
            aReader :> EntityReader
            bReader :> EntityReader
        ]
    let result = lazy ResultSets<'a, 'b>(aReader.ToEntity(), bReader.ToEntity())
    override __.Fragments = fragments
    override __.Parameters = parameters
    override __.BeginResultSet(dataReader) =
        readers <- List.tail readers
        (List.head readers).ProcessColumns(DataReader.columnMap(dataReader))
        row <- DataReader.DataReaderRow(dataReader)
    override __.ProcessRow() =
        (List.head readers).Read(row)
    override __.GetResultObject() = upcast result.Value
    override __.GetResult() = result.Value

type Command3<'a, 'b, 'c>(fragments, parameters) =
    inherit Command<ResultSets<'a, 'b, 'c>>()
    let aReader = ReaderTemplate<'a>.Template().CreateReader()
    let bReader = ReaderTemplate<'b>.Template().CreateReader()
    let cReader = ReaderTemplate<'c>.Template().CreateReader()
    let mutable row = Unchecked.defaultof<Row>
    let mutable readers =
        [
            Unchecked.defaultof<EntityReader>
            aReader :> EntityReader
            bReader :> EntityReader
            cReader :> EntityReader
        ]
    let result = lazy ResultSets<'a, 'b, 'c>(aReader.ToEntity(), bReader.ToEntity(), cReader.ToEntity())
    override __.Fragments = fragments
    override __.Parameters = parameters
    override __.BeginResultSet(dataReader) =
        readers <- List.tail readers
        (List.head readers).ProcessColumns(DataReader.columnMap(dataReader))
        row <- DataReader.DataReaderRow(dataReader)
    override __.ProcessRow() =
        (List.head readers).Read(row)
    override __.GetResultObject() = upcast result.Value
    override __.GetResult() = result.Value
