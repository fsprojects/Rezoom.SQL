namespace Rezoom.ADO.Test
open Rezoom
open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type TestQueries() =
    [<TestMethod>]
    member __.TestQuerySingleUser() =
        {
            Task =
                plan {
                    let! user = query "select Id, Name from Users where Id = {0}" [1]
                    return string <| user.Rows.[0].[1]
                }
            ExpectedResult = Value "Jim"
        } |> test

    [<TestMethod>]
    member __.TestConcurrentQueries() =
        {
            Task =
                plan {
                    let! users = query "select Id from Users" []
                    let names = new ResizeArray<string>()
                    for user in batch users.Rows do
                        let id = user.[0]
                        let! name = query "select Name from Users where Id = {0}" [id]
                        names.Add(name.Rows.[0].[0] |> string)
                    return names |> Set.ofSeq
                }
            ExpectedResult = Value (["Ellen"; "Jim"; "Mary"; "Rick"] |> Set.ofSeq)
        } |> test