module Rezoom.SQL.Test.PrimitiveReaders
open NUnit.Framework
open FsUnit
open Rezoom.SQL.Mapping
open Rezoom.SQL.Mapping.CodeGeneration
open System

type Enum16 =
    | One16 = 1s
    | Two16 = 32767s

type Enum32U =
    | One32U = 1u
    | Two32U = 4294967295u

type Enum64 =
    | One64 = 1L
    | Two64 = 9223372036854775807L

type Enum64U =
    | One64U = 1UL
    | Two64U = 18446744073709551615UL

let testXCore inRow (expected : 'a) ctype =
    let colMap =
        [|
            Guid.NewGuid().ToString("N"), ctype
        |] |> ColumnMap.Parse
    let row = ObjectRow([| box <| inRow expected |])
    let reader = ReaderTemplate<'a>.Template().CreateReader()
    reader.ProcessColumns(colMap)
    reader.Read(row)
    let mat = reader.ToEntity()
    Assert.AreEqual(expected, mat)
let testCore expected ctype = testXCore id expected ctype
let testRef (expected : 'a) ctype =
    testCore expected ctype
    testCore expected ColumnType.Object
    testCore (null : 'a) ctype
    testCore (null : 'a) ColumnType.Object
    testCore (None : 'a option) ColumnType.Object
    testXCore Option.get (Some expected) ColumnType.Object
let test (expected : 'a) ctype =
    testCore expected ctype
    testCore expected ColumnType.Object
    testCore (Nullable<'a>(expected)) ctype
    testCore (Nullable<'a>(expected)) ColumnType.Object
    testCore (Nullable<'a>()) ctype
    testCore (Nullable<'a>()) ColumnType.Object
    testCore (None : 'a option) ColumnType.Object
    testXCore Option.get (Some expected) ColumnType.Object

[<Test>]
let ``read string`` () =
    testRef "thirteen" ColumnType.String

[<Test>]
let ``read byte array`` () =
    testRef [|0uy;1uy;2uy;3uy|] ColumnType.Object

[<Test>]
let ``read int32`` () =
    test 13 ColumnType.Int32
[<Test>]
let ``read int16`` () =
    test 13s ColumnType.Int16
[<Test>]
let ``read int64`` () =
    test 13L ColumnType.Int64

[<Test>]
let ``read uint32`` () =
    test 13u ColumnType.UInt32
[<Test>]
let ``read uint16`` () =
    test 13us ColumnType.UInt16
[<Test>]
let ``read uint64`` () =
    test 13UL ColumnType.UInt64

[<Test>]
let ``read byte`` () =
    test 13uy ColumnType.Byte
[<Test>]
let ``read sbyte`` () =
    test 13y ColumnType.SByte

[<Test>]
let ``read single`` () =
    test 13.5f ColumnType.Single

[<Test>]
let ``read double`` () =
    test 13.5 ColumnType.Double

[<Test>]
let ``read decimal`` () =
    test 13.5m ColumnType.Decimal

[<Test>]
let ``read DateTime`` () =
    test DateTime.UtcNow ColumnType.DateTime

[<Test>]
let ``read DateTimeOffset`` () =
    test DateTimeOffset.UtcNow ColumnType.DateTimeOffset

[<Test>]
let ``read Guid`` () =
    test (Guid.NewGuid()) ColumnType.Guid
    test Guid.Empty ColumnType.Guid
    testXCore (fun (g : Guid) -> g.ToString()) (Guid.NewGuid()) ColumnType.String


[<Test>]
let ``read boolean`` () =
    test false ColumnType.Boolean
    test true ColumnType.Boolean
    testXCore (fun b -> if b then 1 else 0) true ColumnType.Int32
    testXCore (fun b -> if b then 1 else 0) false ColumnType.Int32

[<Test>]
let ``read DateTimeKind enum (via TryParser)`` () =
    let mutable e = DateTimeKind.Unspecified
    let succ = PrimitiveConverters.EnumTryParser<DateTimeKind>.TryParse("Local", &e)
    Assert.IsTrue(succ)
    Assert.AreEqual(DateTimeKind.Local, e)
    let succ = PrimitiveConverters.EnumTryParser<DateTimeKind>.TryParse("Test", &e)
    Assert.IsFalse(succ)

[<Test>]
let ``read Enum16 (via TryParser)`` () =
    let mutable e = Enum16.One16
    let succ = PrimitiveConverters.EnumTryParser<Enum16>.TryParse("Two16", &e)
    Assert.IsTrue(succ)
    Assert.AreEqual(Enum16.Two16, e)

[<Test>]
let ``read Enum32U (via TryParser)`` () =
    let mutable e = Enum32U.One32U
    let succ = PrimitiveConverters.EnumTryParser<Enum32U>.TryParse("Two32U", &e)
    Assert.IsTrue(succ)
    Assert.AreEqual(Enum32U.Two32U, e)

[<Test>]
let ``read Enum64 (via TryParser)`` () =
    let mutable e = Enum64.One64
    let succ = PrimitiveConverters.EnumTryParser<Enum64>.TryParse("Two64", &e)
    Assert.IsTrue(succ)
    Assert.AreEqual(Enum64.Two64, e)

[<Test>]
let ``read Enum64U (via TryParser)`` () =
    let mutable e = Enum64U.One64U
    let succ = PrimitiveConverters.EnumTryParser<Enum64U>.TryParse("Two64U", &e)
    Assert.IsTrue(succ)
    Assert.AreEqual(Enum64U.Two64U, e)

[<Test>]
let ``read DateTimeKind`` () =
    test DateTimeKind.Local ColumnType.Int32

[<Test>]
let ``read enums from string``() =
    let happy (expected : 'a) (str : string) =
        let colMap =
            [|
                Guid.NewGuid().ToString("N"), ColumnType.String
            |] |> ColumnMap.Parse
        let row = ObjectRow(str :> obj)
        let reader = ReaderTemplate<'a>.Template().CreateReader()
        reader.ProcessColumns(colMap)
        reader.Read(row)
        let mat = reader.ToEntity()
        Assert.AreEqual(expected :> obj, mat :> obj)

        let reader = ReaderTemplate<'a Nullable>.Template().CreateReader()
        reader.ProcessColumns(colMap)
        reader.Read(row)
        let mat = reader.ToEntity()
        Assert.AreEqual(expected :> obj, mat :> obj)

        let reader = ReaderTemplate<'a Nullable>.Template().CreateReader()
        reader.ProcessColumns(colMap)
        reader.Read(ObjectRow(null : obj))
        let mat = reader.ToEntity()
        Assert.IsNull(mat)
    let sad (example : 'a) (str : string) =
        let colMap =
            [|
                Guid.NewGuid().ToString("N"), ColumnType.String
            |] |> ColumnMap.Parse
        let row = ObjectRow(str :> obj)
        let reader = ReaderTemplate<'a>.Template().CreateReader()
        reader.ProcessColumns(colMap)
        Assert.IsTrue(
            try
                reader.Read(row)
                ignore <| reader.ToEntity()
                false
            with
            | exn -> true)

    happy DateTimeKind.Local "Local"
    happy DateTimeKind.Utc "Utc"
    happy StringComparison.InvariantCultureIgnoreCase "InvariantCultureIgnoreCase"
    happy Enum64U.Two64U "Two64U"
    sad DateTimeKind.Unspecified "Something"
    sad StringComparison.CurrentCulture ""
