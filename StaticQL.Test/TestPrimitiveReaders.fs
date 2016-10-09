namespace StaticQL.Test.PrimitiveReaders
open StaticQL.Mapping
open StaticQL.Mapping.CodeGeneration
open System
open Microsoft.VisualStudio.TestTools.UnitTesting

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

[<TestClass>]
type TestPrimitiveReaders() =
    let testCore (expected : 'a) ctype =
        let colMap =
            [|
                Guid.NewGuid().ToString("N"), ctype
            |] |> ColumnMap.Parse
        let row = ObjectRow(box expected)
        let reader = ReaderTemplate<'a>.Template().CreateReader()
        reader.ProcessColumns(colMap)
        reader.Read(row)
        let mat = reader.ToEntity()
        Assert.AreEqual(expected, mat)
    let testRef (expected : 'a) ctype =
        testCore expected ctype
        testCore expected ColumnType.Object
        testCore (null : 'a) ctype
        testCore (null : 'a) ColumnType.Object
    let test (expected : 'a) ctype =
        testCore expected ctype
        testCore expected ColumnType.Object
        testCore (Nullable<'a>(expected)) ctype
        testCore (Nullable<'a>(expected)) ColumnType.Object
        testCore (Nullable<'a>()) ctype
        testCore (Nullable<'a>()) ColumnType.Object

    [<TestMethod>]
    member __.TestReadString() = testRef "thirteen" ColumnType.String

    [<TestMethod>]
    member __.TestReadByteArray() = testRef [|0uy;1uy;2uy;3uy|] ColumnType.Object

    [<TestMethod>]
    member __.TestReadInt32() = test 13 ColumnType.Int32
    [<TestMethod>]
    member __.TestReadInt16() = test 13s ColumnType.Int16
    [<TestMethod>]
    member __.TestReadInt64() = test 13L ColumnType.Int64

    [<TestMethod>]
    member __.TestReadUInt32() = test 13u ColumnType.UInt32
    [<TestMethod>]
    member __.TestReadUInt16() = test 13us ColumnType.UInt16
    [<TestMethod>]
    member __.TestReadUInt64() = test 13UL ColumnType.UInt64

    [<TestMethod>]
    member __.TestReadByte() = test 13uy ColumnType.Byte
    [<TestMethod>]
    member __.TestReadSByte() = test 13y ColumnType.SByte

    [<TestMethod>]
    member __.TestReadSingle() = test 13.5f ColumnType.Single
    [<TestMethod>]
    member __.TestReadDouble() = test 13.5 ColumnType.Double
    [<TestMethod>]
    member __.TestReadDecimal() = test 13.5m ColumnType.Decimal

    [<TestMethod>]
    member __.TestReadDateTime() = test DateTime.UtcNow ColumnType.DateTime

    [<TestMethod>]
    member __.TestEnumTryParser() =
        let mutable e = DateTimeKind.Unspecified
        let succ = PrimitiveConverters.EnumTryParser<DateTimeKind>.TryParse("Local", &e)
        Assert.IsTrue(succ)
        Assert.AreEqual(DateTimeKind.Local, e)
        let succ = PrimitiveConverters.EnumTryParser<DateTimeKind>.TryParse("Test", &e)
        Assert.IsFalse(succ)

    [<TestMethod>]
    member __.TestEnumTryParser16() =
        let mutable e = Enum16.One16
        let succ = PrimitiveConverters.EnumTryParser<Enum16>.TryParse("Two16", &e)
        Assert.IsTrue(succ)
        Assert.AreEqual(Enum16.Two16, e)

    [<TestMethod>]
    member __.TestEnumTryParser32U() =
        let mutable e = Enum32U.One32U
        let succ = PrimitiveConverters.EnumTryParser<Enum32U>.TryParse("Two32U", &e)
        Assert.IsTrue(succ)
        Assert.AreEqual(Enum32U.Two32U, e)

    [<TestMethod>]
    member __.TestEnumTryParser64() =
        let mutable e = Enum64.One64
        let succ = PrimitiveConverters.EnumTryParser<Enum64>.TryParse("Two64", &e)
        Assert.IsTrue(succ)
        Assert.AreEqual(Enum64.Two64, e)

    [<TestMethod>]
    member __.TestEnumTryParser64U() =
        let mutable e = Enum64U.One64U
        let succ = PrimitiveConverters.EnumTryParser<Enum64U>.TryParse("Two64U", &e)
        Assert.IsTrue(succ)
        Assert.AreEqual(Enum64U.Two64U, e)

    [<TestMethod>]
    member __.TestReadEnum() = test DateTimeKind.Local ColumnType.Int32

    [<TestMethod>]
    member __.TestReadEnumString() =
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
            Assert.IsTrue <|
            try
                reader.Read(row)
                ignore <| reader.ToEntity()
                false
            with
            | exn -> true

        happy DateTimeKind.Local "Local"
        happy DateTimeKind.Utc "Utc"
        happy StringComparison.InvariantCultureIgnoreCase "InvariantCultureIgnoreCase"
        happy Enum64U.Two64U "Two64U"
        sad DateTimeKind.Unspecified "Something"
        sad StringComparison.CurrentCulture ""