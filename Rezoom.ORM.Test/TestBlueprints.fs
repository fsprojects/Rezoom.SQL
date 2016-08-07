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

[<TestClass>]
type TestBlueprints() =
    [<TestMethod>]
    member __.TestFolder() =
        let blue = Blueprint.ofType typeof<Folder>
        match blue.Cardinality with
        | One { Shape = Composite folder } ->
            match folder.Columns.["ChildFolders"].ReverseRelationship.Value with
            | None -> failwith "No reverse relationship for child folders"
            | Some parent ->
                Assert.AreEqual("ParentFolder", parent.Name)
            match folder.Columns.["ParentFolder"].ReverseRelationship.Value with
            | None -> failwith "No reverse relationship for parent folder"
            | Some childFolders ->
                Assert.AreEqual("ChildFolders", childFolders.Name)
                Assert.IsTrue(obj.ReferenceEquals(childFolders, folder.Columns.["ChildFolders"]))
        | _ -> failwith "Wrong cardinality/shape"