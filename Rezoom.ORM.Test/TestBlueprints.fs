namespace Rezoom.ORM.Test.Blueprints
open Rezoom.ORM
open System
open Microsoft.VisualStudio.TestTools.UnitTesting

type Folder =
    {
        Id : int
        ParentFolder : Folder
        ChildFolders : Folder list
    }

type UserFriendMap =
    {
        Friend1 : User
        Friend2 : User
    }

and User =
    {
        Id : int
        Friend1Maps : UserFriendMap list
        Friend2Maps : UserFriendMap list
    }

[<TestClass>]
type TestBlueprints() =
    [<TestMethod>]
    member __.TestFolder() =
        let blue = Blueprint.ofType typeof<Folder>
        match blue.Cardinality with
        | One { Shape = Composite folder } ->
            match folder.Columns.["ParentFolder"].ReverseRelationship.Value with
            | None -> failwith "No reverse relationship for parent folder"
            | Some childFolders ->
                Assert.IsTrue("ChildFolders".Equals(childFolders.Name, StringComparison.OrdinalIgnoreCase))
                Assert.IsTrue(obj.ReferenceEquals(childFolders, folder.Columns.["ChildFolders"]))

            match folder.Columns.["ChildFolders"].ReverseRelationship.Value with
            | None -> failwith "No reverse relationship for child folders"
            | Some parent ->
                Assert.IsTrue("ParentFolder".Equals(parent.Name, StringComparison.OrdinalIgnoreCase))
        | _ -> failwith "Wrong cardinality/shape"

    [<TestMethod>]
    member __.TestUser() =
        let blue = Blueprint.ofType typeof<User>
        match blue.Cardinality with
        | One { Shape = Composite user } ->
            match user.Columns.["Friend1Maps"].ReverseRelationship.Value with
            | None -> failwith "No reverse relationship for friend1maps"
            | Some friend1Maps ->
                Assert.IsTrue("Friend1".Equals(friend1Maps.Name, StringComparison.OrdinalIgnoreCase))
            match user.Columns.["Friend2Maps"].ReverseRelationship.Value with
            | None -> failwith "No reverse relationship for friend2maps"
            | Some friend2Maps ->
                Assert.IsTrue("Friend2".Equals(friend2Maps.Name, StringComparison.OrdinalIgnoreCase))
        | _ -> failwith "Wrong cardinality/shape"

    [<TestMethod>]
    member __.TestFriendMap() =
        let blue = Blueprint.ofType typeof<UserFriendMap>
        match blue.Cardinality with
        | One { Shape = Composite friendMap } ->
            match friendMap.Columns.["Friend1"].ReverseRelationship.Value with
            | None -> failwith "No reverse relationship for friend1"
            | Some friend1Maps ->
                Assert.IsTrue("Friend1Maps".Equals(friend1Maps.Name, StringComparison.OrdinalIgnoreCase))
            match friendMap.Columns.["Friend2"].ReverseRelationship.Value with
            | None -> failwith "No reverse relationship for friend1"
            | Some friend2Maps ->
                Assert.IsTrue("Friend2Maps".Equals(friend2Maps.Name, StringComparison.OrdinalIgnoreCase))
        | _ -> failwith "Wrong cardinality/shape"