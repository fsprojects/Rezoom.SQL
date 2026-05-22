module Rezoom.SQL.Test.UserPrimitives
open NUnit.Framework
open FsUnit
open Rezoom.SQL.Mapping
open Rezoom.SQL.Mapping.CodeGeneration
open System
open System.Reflection
open Rezoom.SQL.Test.UserTypes

// conversion extensions for a type defined in another assembly
[<AutoOpen>]
module IntyPhoneNumberConverters =
    type IntyPhoneNumber with
        member phone.ToPrimitive() =
            int64 phone.CountryCode * 10_000_000_000L
            + int64 phone.AreaCode * 10_000_000L
            + int64 phone.Number
        static member FromPrimitive (packed : int64) =
            let countryCode = int (packed / 10_000_000_000L)
            let areaCode = int ((packed / 10_000_000L) % 1_000L)
            let number = int (packed % 10_000_000L)
            IntyPhoneNumber(countryCode, areaCode, number)

let private mappings =
    let thisAsm = Assembly.GetExecutingAssembly()
    let referencedAsm = typeof<IntyPhoneNumber>.Assembly
    PrimitiveConverters.buildCustomMappingsFromAssemblies [|thisAsm; referencedAsm|]

type TimeTestRow =
    {   Id : int
        Time : TimeOnly
    }

[<Test>]
let ``read timeonly`` () =
    let testTime = System.TimeOnly.FromDateTime(DateTime.Parse("2026-05-20T22:26:17.9873551-04:00"))
    let colMap =
        [|
            "Id", ColumnType.Int32
            "Time", ColumnType.String
        |] |> ColumnMap.Parse
    let row = ObjectRow(27, testTime.ToString("o"))
    let reader = ReaderTemplate<TimeTestRow>.Template(mappings).CreateReader()
    reader.ProcessColumns(colMap)
    reader.Read(row)
    let timeRow = reader.ToEntity()
    Assert.IsNotNull(timeRow)
    Assert.AreEqual(27, timeRow.Id)
    Assert.AreEqual(testTime, timeRow.Time)

type StringyPhoneTestRow =
    {   Id : int
        Phone : StringyPhoneNumber
    }

[<Test>]
let ``read stringy phone number`` () =
    let testPhone = { CountryCode = 1; AreaCode = 800; Number = 5551234 }
    let colMap =
        [|
            "Id", ColumnType.Int32
            "Phone", ColumnType.String
        |] |> ColumnMap.Parse
    let row = ObjectRow(42, StringyPhoneNumber.ToPrimitive(testPhone))
    let reader = ReaderTemplate<StringyPhoneTestRow>.Template(mappings).CreateReader()
    reader.ProcessColumns(colMap)
    reader.Read(row)
    let phoneRow = reader.ToEntity()
    Assert.IsNotNull(phoneRow)
    Assert.AreEqual(42, phoneRow.Id)
    Assert.AreEqual(testPhone, phoneRow.Phone)

type IntyPhoneTestRow =
    {   Id : int
        Phone : IntyPhoneNumber
    }

[<Test>]
let ``read inty phone number`` () =
    let testPhone = IntyPhoneNumber(1, 800, 5551234)
    let colMap =
        [|
            "Id", ColumnType.Int32
            "Phone", ColumnType.Int64
        |] |> ColumnMap.Parse
    let row = ObjectRow(99, testPhone.ToPrimitive())
    let reader = ReaderTemplate<IntyPhoneTestRow>.Template(mappings).CreateReader()
    reader.ProcessColumns(colMap)
    reader.Read(row)
    let phoneRow = reader.ToEntity()
    Assert.IsNotNull(phoneRow)
    Assert.AreEqual(99, phoneRow.Id)
    Assert.AreEqual(testPhone.CountryCode, phoneRow.Phone.CountryCode)
    Assert.AreEqual(testPhone.AreaCode, phoneRow.Phone.AreaCode)
    Assert.AreEqual(testPhone.Number, phoneRow.Phone.Number)

type TimeSpanTestRow =
    {   Id : int
        Span : TimeSpan
    }

[<Test>]
let ``read timespan via let-bound converters`` () =
    let testSpan = TimeSpan.FromHours(13.5)
    let colMap =
        [|
            "Id", ColumnType.Int32
            "Span", ColumnType.Int64
        |] |> ColumnMap.Parse
    let row = ObjectRow(13, testSpan.Ticks)
    let reader = ReaderTemplate<TimeSpanTestRow>.Template(mappings).CreateReader()
    reader.ProcessColumns(colMap)
    reader.Read(row)
    let spanRow = reader.ToEntity()
    Assert.IsNotNull(spanRow)
    Assert.AreEqual(13, spanRow.Id)
    Assert.AreEqual(testSpan, spanRow.Span)