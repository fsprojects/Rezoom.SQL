module Rezoom.SQL.Test.TestUserPrimitivesCompiler
open System
open System.IO
open NUnit.Framework
open FsUnit
open Rezoom.SQL.Compiler
open Rezoom.SQL.Mapping
open Rezoom.SQL.Migrations


[<Test>]
let ``user model with custom primitives loads`` () =
    let model = userModelByName "user-model-7-usertypes"
    printfn "%d" model.UserTypeLibrary.CountPrimitives

let userModel = lazy userModelByName "user-model-7-usertypes"

[<Test>]
let ``simple query against user model preserves usertypes`` () =
    let cmd = CommandEffect.OfSQL(userModel.Value.Model, "anonymous", @"
        select Email, Id, Phone, Name from Users
    ", userModel.Value.UserTypeLibrary)
    let resultSet = cmd.ResultSets() |> Seq.exactlyOne
    let colInfos = resultSet.Columns |> Seq.map (fun c -> c.Expr.Info) |> Seq.toList
    printfn "%A" colInfos
    match colInfos with
    |   [   {   Type = { Type = UserTypeBasedOn(emailUserType, StringType) }
                Column = Some { ColumnName = emailCol }
            }
            {   Type = { Type = UserTypeBasedOn(idUserType, IntegerType Integer32) }
                Column = Some { ColumnName = idCol }
            }
            {   Type = { Type = UserTypeBasedOn(phoneUserType, StringType) }
                Column = Some { ColumnName = phoneCol }
            }
            {   Type = { Type = StringType }
                Column = Some  { ColumnName = nameCol }
            }
        ] ->
            Assert.AreEqual(Name("email"), emailCol)
            Assert.AreEqual(Name("id"), idCol)
            Assert.AreEqual(Name("phone"), phoneCol)
            Assert.AreEqual(Name("name"), nameCol)

            Assert.AreEqual("Rezoom.SQL.Test.UserTypes.EmailAddress", emailUserType.UserCLRType.FullName)
            Assert.AreEqual("Rezoom.SQL.Test.UserTypes.UserId", idUserType.UserCLRType.FullName)
            Assert.AreEqual("Rezoom.SQL.Test.UserTypes.StringyPhoneNumber", phoneUserType.UserCLRType.FullName)
    | _ ->
        failwith "Result set shape did not match expected"

[<Test>]
let ``query parameter unifies to usertype`` () =
    let cmd = CommandEffect.OfSQL(userModel.Value.Model, "anonymous", @"
        select * from Users where Email = @email
    ", userModel.Value.UserTypeLibrary)
    let parm, parmTy = cmd.Parameters |> Seq.exactlyOne
    printfn "%A" parm
    match parm, parmTy with
    | NamedParameter e, { Type = UserTypeBasedOn(userTy, StringType); Nullable = false } ->
        Assert.AreEqual(Name("email"), e)
        Assert.AreEqual("Rezoom.SQL.Test.UserTypes.EmailAddress", userTy.UserCLRType.FullName)
    | _ ->
        failwith "Parameter shape did not match expected"