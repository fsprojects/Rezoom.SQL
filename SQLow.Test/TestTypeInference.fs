namespace SQLow.Test
open Microsoft.VisualStudio.TestTools.UnitTesting
open SQLow

[<TestClass>]
type TestTypeInference() =
    static let zeroSchema name =
        {   SchemaName = name
            Tables = Map.empty
            Views = Map.empty
        }
    static let zeroModel =
        {   Schemas =
                [   zeroSchema (Name("main"))
                    zeroSchema (Name("temp"))
                ] |> List.map (fun s -> s.SchemaName, s) |> Map.ofList
            DefaultSchema = Name("main")
            TemporarySchema = Name("temp")
        }
    [<TestMethod>]
    member __.TestSimpleSelect() =
        let cmd = zeroModel |> Command.ofSql @"
            create table Users(id int, name nvarchar(128), email nvarchar(128));
            select * from Users
        "
        printfn "%A" cmd
