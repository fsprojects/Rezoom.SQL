namespace Rezoom.ADO.Test
open Rezoom
open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type TestReaderQueries() =
    [<TestMethod>]
    member __.TestQuerySingleUser() =
        {
            Task =
                plan {
                    let! (user : User) = readerQuery "select Id, Name from Users where Id = {0}" [1]
                    return user.Name
                }
            ExpectedResult = Value "Jim"
        } |> test

    [<TestMethod>]
    member __.TestConcurrentQueries() =
        {
            Task =
                plan {
                    let! (users : User list) = readerQuery "select * from Users" []
                    let names = new ResizeArray<string>()
                    for user in batch users do
                        let! name = readerQuery "select Name from Users where Id = {0}" [user.Id]
                        names.Add(name)
                    return names |> Set.ofSeq
                }
            ExpectedResult = Value (["Ellen"; "Jim"; "Mary"; "Rick"] |> Set.ofSeq)
        } |> test

    [<TestMethod>]
    member __.TestFancyQueryFromUser() =
        {
            Task =
                plan {
                    let! (users : User list) =
                        readerQuery
                            @"select u.*, gm.Id as GroupMaps$Id, gm.*, g.Id as GroupMaps$Group$Id, g.*
                              from Users u
                              join UserGroupMaps gm on gm.UserId = u.Id
                              join Groups g on g.Id = gm.GroupId"
                            []
                    let users = users |> List.sortBy (fun u -> u.Id)
                    return
                        match users with
                        |
                            [   {   Id = 1
                                    Name = "Jim"
                                    GroupMaps = [| { Group = { Name = "Admins" } }; { Group = { Name = "Content Creators" } } |]
                                }
                                _
                                _
                                _
                            ] -> true
                        | _ ->
                            failwith "Bad user structure"
                }
            ExpectedResult = Value true
        } |> test
