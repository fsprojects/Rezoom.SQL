module Rezoom.SQL.Test.Blueprints
open NUnit.Framework
open FsUnit
open Rezoom.SQL.Mapping
open System

type Folder =
    {
        Id : int
        ParentFolder : Folder
        ChildFolders : Folder list
    }

[<Test>]
let ``folder blueprint makes sense`` () =
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

[<Test>]
let ``user blueprint makes sense`` () =
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

[<Test>]
let ``friend map blueprint makes sense`` () =
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

type Foo =
    {
        FooId : int
        ChildBars : Bar array
    }
and Bar =
    {
        BarId : int
        ParentFoo : Foo
    }

[<Test>]
let ``foo blueprint makes sense`` () =
    let blue = Blueprint.ofType typeof<Foo>
    match blue.Cardinality with
    | One { Shape = Composite fooMap } ->
        match fooMap.Columns.["ChildBars"].ReverseRelationship.Value with
        | None -> failwith "No reverse relationship for ChildBars"
        | Some parentFoo ->
            Assert.IsTrue("ParentFoo".Equals(parentFoo.Name, StringComparison.OrdinalIgnoreCase))
    | _ -> failwith "Wrong cardinality/shape"

[<Test>]
let ``bar blueprint makes sense`` () =
    let blue = Blueprint.ofType typeof<Bar>
    match blue.Cardinality with
    | One { Shape = Composite barMap } ->
        match barMap.Columns.["ParentFoo"].ReverseRelationship.Value with
        | None -> failwith "No reverse relationship for ParentFoo"
        | Some childBars ->
            Assert.IsTrue("ChildBars".Equals(childBars.Name, StringComparison.OrdinalIgnoreCase))
    | _ -> failwith "Wrong cardinality/shape"