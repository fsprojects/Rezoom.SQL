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
    UserTypeLibraryLoader.loadUserTypeLibrary [|thisAsm; referencedAsm|]

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
    Assert.NotNull(timeRow)
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
    Assert.NotNull(phoneRow)
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
    Assert.NotNull(phoneRow)
    Assert.AreEqual(99, phoneRow.Id)
    Assert.AreEqual(testPhone.CountryCode, phoneRow.Phone.CountryCode)
    Assert.AreEqual(testPhone.AreaCode, phoneRow.Phone.AreaCode)
    Assert.AreEqual(testPhone.Number, phoneRow.Phone.Number)

type DateTimeOffsetTestRow =
    {   Id : int
        Stamp : DateTimeOffset
    }

[<Test>]
let ``read datetimeoffset via override`` () =
    let testStamp = DateTimeOffset.Parse("2026-05-20T22:26:17.9873551-04:00")
    let colMap =
        [|
            "Id", ColumnType.Int32
            "Stamp", ColumnType.String
        |] |> ColumnMap.Parse
    let row = ObjectRow(7, testStamp.ToString("o"))
    let reader = ReaderTemplate<DateTimeOffsetTestRow>.Template(mappings).CreateReader()
    reader.ProcessColumns(colMap)
    reader.Read(row)
    let stampRow = reader.ToEntity()
    Assert.NotNull(stampRow)
    Assert.AreEqual(7, stampRow.Id)
    Assert.AreEqual(testStamp, stampRow.Stamp)

type StringyPhoneOptionalTestRow =
    {   Id : int
        Phone : StringyPhoneNumber option
    }

[<Test>]
let ``read option of custom type with value`` () =
    let testPhone = { CountryCode = 1; AreaCode = 800; Number = 5551234 }
    let colMap =
        [|
            "Id", ColumnType.Int32
            "Phone", ColumnType.String
        |] |> ColumnMap.Parse
    let row = ObjectRow(42, StringyPhoneNumber.ToPrimitive(testPhone))
    let reader = ReaderTemplate<StringyPhoneOptionalTestRow>.Template(mappings).CreateReader()
    reader.ProcessColumns(colMap)
    reader.Read(row)
    let phoneRow = reader.ToEntity()
    Assert.NotNull(phoneRow)
    Assert.AreEqual(42, phoneRow.Id)
    Assert.AreEqual(Some testPhone, phoneRow.Phone)

[<Test>]
let ``read option of custom type with null`` () =
    let colMap =
        [|
            "Id", ColumnType.Int32
            "Phone", ColumnType.String
        |] |> ColumnMap.Parse
    let row = ObjectRow(42, (null : obj))
    let reader = ReaderTemplate<StringyPhoneOptionalTestRow>.Template(mappings).CreateReader()
    reader.ProcessColumns(colMap)
    reader.Read(row)
    let phoneRow = reader.ToEntity()
    Assert.NotNull(phoneRow)
    Assert.AreEqual(42, phoneRow.Id)
    Assert.AreEqual(None, phoneRow.Phone)

type TimeOnlyNullableTestRow =
    {   Id : int
        Time : Nullable<TimeOnly>
    }

[<Test>]
let ``read nullable of custom type with value`` () =
    let testTime = System.TimeOnly.FromDateTime(DateTime.Parse("2026-05-20T22:26:17.9873551-04:00"))
    let colMap =
        [|
            "Id", ColumnType.Int32
            "Time", ColumnType.String
        |] |> ColumnMap.Parse
    let row = ObjectRow(27, testTime.ToString("o"))
    let reader = ReaderTemplate<TimeOnlyNullableTestRow>.Template(mappings).CreateReader()
    reader.ProcessColumns(colMap)
    reader.Read(row)
    let timeRow = reader.ToEntity()
    Assert.NotNull(timeRow)
    Assert.AreEqual(27, timeRow.Id)
    Assert.That(timeRow.Time.HasValue)
    Assert.AreEqual(testTime, timeRow.Time.Value)

[<Test>]
let ``read nullable of custom type with null`` () =
    let colMap =
        [|
            "Id", ColumnType.Int32
            "Time", ColumnType.String
        |] |> ColumnMap.Parse
    let row = ObjectRow(27, (null : obj))
    let reader = ReaderTemplate<TimeOnlyNullableTestRow>.Template(mappings).CreateReader()
    reader.ProcessColumns(colMap)
    reader.Read(row)
    let timeRow = reader.ToEntity()
    Assert.NotNull(timeRow)
    Assert.AreEqual(27, timeRow.Id)
    Assert.That(not timeRow.Time.HasValue)

type CustomUserIdTestRow =
    {   Id : int
        UserId : CustomUserId
    }

[<Test>]
let ``read single-case ref DU wrapping Guid`` () =
    let testGuid = Guid.Parse("12345678-1234-1234-1234-123456789abc")
    let colMap =
        [|
            "Id", ColumnType.Int32
            "UserId", ColumnType.Guid
        |] |> ColumnMap.Parse
    let row = ObjectRow(1, testGuid)
    let reader = ReaderTemplate<CustomUserIdTestRow>.Template(mappings).CreateReader()
    reader.ProcessColumns(colMap)
    reader.Read(row)
    let userRow = reader.ToEntity()
    Assert.NotNull(userRow)
    Assert.AreEqual(1, userRow.Id)
    Assert.AreEqual(CustomUserId testGuid, userRow.UserId)

type CustomStringIdTestRow =
    {   Id : int
        StringId : CustomStringId
    }

[<Test>]
let ``read single-case ref DU wrapping string`` () =
    let testString = "alice@example.com"
    let colMap =
        [|
            "Id", ColumnType.Int32
            "StringId", ColumnType.String
        |] |> ColumnMap.Parse
    let row = ObjectRow(2, testString)
    let reader = ReaderTemplate<CustomStringIdTestRow>.Template(mappings).CreateReader()
    reader.ProcessColumns(colMap)
    reader.Read(row)
    let stringRow = reader.ToEntity()
    Assert.NotNull(stringRow)
    Assert.AreEqual(2, stringRow.Id)
    Assert.AreEqual(CustomStringId testString, stringRow.StringId)

type CustomUserIdStructTestRow =
    {   Id : int
        UserId : CustomUserIdStruct
    }

[<Test>]
let ``read single-case struct DU wrapping Guid`` () =
    let testGuid = Guid.Parse("87654321-4321-4321-4321-cba987654321")
    let colMap =
        [|
            "Id", ColumnType.Int32
            "UserId", ColumnType.Guid
        |] |> ColumnMap.Parse
    let row = ObjectRow(3, testGuid)
    let reader = ReaderTemplate<CustomUserIdStructTestRow>.Template(mappings).CreateReader()
    reader.ProcessColumns(colMap)
    reader.Read(row)
    let userRow = reader.ToEntity()
    Assert.NotNull(userRow)
    Assert.AreEqual(3, userRow.Id)
    Assert.AreEqual(CustomUserIdStruct testGuid, userRow.UserId)

type CustomStringIdStructTestRow =
    {   Id : int
        StringId : CustomStringIdStruct
    }

[<Test>]
let ``read single-case struct DU wrapping string`` () =
    let testString = "bob@example.com"
    let colMap =
        [|
            "Id", ColumnType.Int32
            "StringId", ColumnType.String
        |] |> ColumnMap.Parse
    let row = ObjectRow(4, testString)
    let reader = ReaderTemplate<CustomStringIdStructTestRow>.Template(mappings).CreateReader()
    reader.ProcessColumns(colMap)
    reader.Read(row)
    let stringRow = reader.ToEntity()
    Assert.NotNull(stringRow)
    Assert.AreEqual(4, stringRow.Id)
    Assert.AreEqual(CustomStringIdStruct testString, stringRow.StringId)

[<Test>]
let ``read single-case DU without explicit usertype mappings`` () =
    let testString = "bob@example.com"
    let colMap =
        [|
            "Id", ColumnType.Int32
            "StringId", ColumnType.String
        |] |> ColumnMap.Parse          //                          |                           |
    let row = ObjectRow(4, testString) //                         vvv works without usertypes vvv
    let reader = ReaderTemplate<CustomStringIdStructTestRow>.Template(UserTypeLibrary.Empty).CreateReader()
    reader.ProcessColumns(colMap)
    reader.Read(row)
    let stringRow = reader.ToEntity()
    Assert.NotNull(stringRow)
    Assert.AreEqual(4, stringRow.Id)
    Assert.AreEqual(CustomStringIdStruct testString, stringRow.StringId)

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
    Assert.NotNull(spanRow)
    Assert.AreEqual(13, spanRow.Id)
    Assert.AreEqual(testSpan, spanRow.Span)