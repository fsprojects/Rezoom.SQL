namespace Rezoom.ORM.Test.PrimitiveReaders
open Rezoom.ORM
open System
open Microsoft.VisualStudio.TestTools.UnitTesting

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
        sad DateTimeKind.Unspecified "Something"
        sad StringComparison.CurrentCulture ""