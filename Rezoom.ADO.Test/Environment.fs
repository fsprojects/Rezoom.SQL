[<AutoOpen>]
module Rezoom.ADO.Test.Environment
open Rezoom
open Rezoom.ADO
open Rezoom.Execution
open System
open System.Collections
open System.Collections.Generic
open System.Data.SQLite
open FSharp.Reflection

let private initializeSchema (conn : SQLiteConnection) =
    let cmd = conn.CreateCommand()
    cmd.CommandText <- @"
        create table Users
            ( Id int primary key
            , Name text
            )
        ;
        create table Groups
            ( Id int primary key
            , Name text
            )
        ;
        create table UserGroupMaps
            ( UserId int
            , GroupId int
            , primary key(UserId, GroupId)
            , foreign key(UserId) references Users(Id)
            , foreign key(GroupId) references Groups(Id)
            )
        ;
    "
    cmd.Connection <- conn
    cmd.ExecuteNonQuery()

let private initializeData (conn : SQLiteConnection) =
    let cmd = conn.CreateCommand()
    cmd.CommandText <- @"
        insert into Users(Id, Name)
        values
          ( 1, ""Jim"" )
        , ( 2, ""Mary"" )
        , ( 3, ""Ellen"" )
        , ( 4, ""Rick"" )
        ;
        insert into Groups(Id, Name)
        values
          ( 1, ""Admins"" )
        , ( 2, ""Content Creators"" )
        , ( 3, ""Content Reviewers"" )
        , ( 4, ""Content Organizers"" )
        ;
        insert into UserGroupMaps(UserId, GroupId)
        values
          ( 1, 1 )
        , ( 1, 2 )
        , ( 2, 3 )
        , ( 2, 4 )
        , ( 3, 2 )
        , ( 4, 3 )
        ;
    "
    cmd.Connection <- conn
    cmd.ExecuteNonQuery()

let private initializeDb (conn : SQLiteConnection) =
    ignore <| initializeSchema conn
    ignore <| initializeData conn

type TestDbServiceFactory() =
    inherit DbServiceFactory()
    override __.CreateConnection() =
        let filename = (Guid.NewGuid().ToString("n").Substring(0, 4) + ".db")
        SQLiteConnection.CreateFile(filename)
        let conn = new SQLiteConnection("Data Source=" + filename)
        conn.Open()
        initializeDb conn
        upcast conn

let query query args =
    let args = Array.ofList args
    let command =
        { new FormattableString() with
            member __.Format = query
            member __.GetArguments() = args
            member __.ArgumentCount = args.Length
            member __.GetArgument(index) = args.[index]
            member __.ToString(provider) = String.Format(provider, query, args)
        } |> Command.Query
    plan {
        let! rs = CommandErrand(command).ToPlan()
        return rs.[0]
    }
    
type 'a ExpectedResult =
    | Exception of (exn -> bool)
    | Value of 'a

type 'a TestTask =
    {
        Task : 'a Plan
        ExpectedResult : 'a ExpectedResult
    }

let test (task : 'a TestTask) =
    use context =
        new ExecutionContext(new TestDbServiceFactory(), new DebugExecutionLog())
    let answer =
        try
            context.Execute(task.Task).Result |> Some
        with
        | ex ->
            match task.ExpectedResult with
            | Exception predicate ->
                if predicate ex then None
                else reraise()
            | _ -> reraise()
    match answer with
    | None -> ()
    | Some v ->
        match task.ExpectedResult with
        | Exception predicate ->
            failwithf "Got value %A when exception was expected" v
        | Value expect ->
            if expect <> v then
                failwithf "Got %A; expected %A" v expect
            else ()
        