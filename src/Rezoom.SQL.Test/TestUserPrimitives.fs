module Rezoom.SQL.Test.UserPrimitives
open NUnit.Framework
open FsUnit
open Rezoom.SQL.Mapping
open Rezoom.SQL.Mapping.CodeGeneration
open System
open System.Reflection
open Rezoom.SQL.Test.UserTypes

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