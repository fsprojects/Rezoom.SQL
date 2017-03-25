module Rezoom.SQL.Test.TestNavProperties
open System
open NUnit.Framework
open FsUnit
open Rezoom.SQL.Compiler
open Rezoom.SQL.Mapping

let columns (sql : string) expected =
    let userModel = userModel1()
    let parsed = CommandEffect.OfSQL(userModel.Model, "anonymous", sql)
    let sets = parsed.ResultSets() |> Seq.toArray
    if sets.Length <> 1 then fail "expected 1 result set"
    let cols = sets.[0].Columns |> Seq.map (fun c -> c.ColumnName.Value, c.Expr.Info.Type.ToString()) |> Seq.toList
    printfn "%A" cols
    Assert.AreEqual(expected, cols)

[<Test>]
let ``1 user many groups`` () =
    columns
        """
            select u.Id, u.Name, many Groups(g.Id, g.Name)
            from Users u
            join UserGroupMaps ugm on ugm.UserId = u.Id
            join Groups g on g.Id = ugm.GroupId
        """
        [   "Id", "INT"
            "Name", "STRING?"
            "Groups*$Id", "INT"
            "Groups*$Name", "STRING?"
        ]

[<Test>]
let ``1 user 1 group`` () =
    columns
        """
            select u.Id, u.Name, one Group(g.Id, g.Name)
            from Users u
            join UserGroupMaps ugm on ugm.UserId = u.Id
            join Groups g on g.Id = ugm.GroupId
        """
        [   "Id", "INT"
            "Name", "STRING?"
            "Group$Id", "INT"
            "Group$Name", "STRING?"
        ]

[<Test>]
let ``1 user many groups left join no nav`` () =
    columns
        """
            select u.Id, u.Name, g.Id as GroupId, g.Name as GroupName
            from Users u
            left join UserGroupMaps ugm on ugm.UserId = u.Id
            left join Groups g on g.Id = ugm.GroupId
        """
        [   "Id", "INT"
            "Name", "STRING?"
            "GroupId", "INT?"
            "GroupName", "STRING?"
        ]

[<Test>]
let ``1 user many groups left join nav`` () =
    columns
        """
            select u.Id, u.Name, many Groups(g.Id, g.Name)
            from Users u
            left join UserGroupMaps ugm on ugm.UserId = u.Id
            left join Groups g on g.Id = ugm.GroupId
        """
        [   "Id", "INT"
            "Name", "STRING?"
            "Groups*$Id", "INT"
            "Groups*$Name", "STRING?"
        ]

[<Test>]
let ``1 user many maps many groups left join nav`` () =
    columns
        """
            select u.Id, u.Name, many Maps(ugm.UserId, ugm.GroupId, one Group(g.Id, g.Name))
            from Users u
            left join UserGroupMaps ugm on ugm.UserId = u.Id
            left join Groups g on g.Id = ugm.GroupId
        """
        [   "Id", "INT"
            "Name", "STRING?"
            "Maps*$UserId", "INT"
            "Maps*$GroupId", "INT"
            "Maps*$Group$Id", "INT"
            "Maps*$Group$Name", "STRING?"
        ]

[<Test>]
let ``1 user many maps many groups left join nav/nonav`` () =
    columns
        """
            select u.Id, u.Name, many Maps(ugm.UserId, ugm.GroupId, g.Id as GroupGroupId, g.Name as GroupGroupName)
            from Users u
            left join UserGroupMaps ugm on ugm.UserId = u.Id
            left join Groups g on g.Id = ugm.GroupId
        """
        [   "Id", "INT"
            "Name", "STRING?"
            "Maps*$UserId", "INT"
            "Maps*$GroupId", "INT"
            "Maps*$GroupGroupId", "INT?" // note that this is back to nullable now
            "Maps*$GroupGroupName", "STRING?"
        ]