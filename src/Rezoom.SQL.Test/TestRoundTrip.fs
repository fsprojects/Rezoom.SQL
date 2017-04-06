module Rezoom.SQL.Test.TestRoundTrip
open System
open NUnit.Framework
open FsUnit
open Rezoom.SQL.Compiler
open Rezoom.SQL.Mapping

let roundtrip (sql : string) =
    let userModel = userModel1()
    let parsed = CommandEffect.OfSQL(userModel.Model, "anonymous", sql)
    let indexer = { new IParameterIndexer with member __.ParameterIndex(_) = 0 }
    let backend = DefaultBackend() :> IBackend
    let fragments = backend.ToCommandFragments(indexer, parsed.Statements)
    let str = CommandFragment.Stringize(fragments)
    Console.WriteLine(str)
    let parsedBack = CommandEffect.OfSQL(userModel.Model, "readback", str)
    let fragmentsBack = backend.ToCommandFragments(indexer, parsedBack.Statements)
    let strBack = CommandFragment.Stringize(fragmentsBack)
    Console.WriteLine(String('-', 80))
    Console.WriteLine(strBack)
    Assert.AreEqual(str, strBack)

[<Test>]
let ``select`` () =
    roundtrip """
        select * from Users u where u.Id = 1
    """
[<Test>]
let ``fancy select`` () =
    roundtrip """
        select g.*, u.*
        from Users u
        left join UserGroupMaps gm on gm.UserId = u.Id
        left join Groups g on g.Id = gm.GroupId
        where g.Name like '%grp%' escape '%'
    """

[<Test>]
let ``fancy select with order by`` () =
    roundtrip """
        select g.*, u.*
        from Users u
        left join UserGroupMaps gm on gm.UserId = u.Id
        left join Groups g on g.Id = gm.GroupId
        where g.Name like '%grp%' escape '%'
        order by g.Id asc, u.Id desc
    """

[<Test>]
let ``insert`` () =
    roundtrip """
        insert into Users(id, name, email)
        values (0, 'ben', 'ben@example.com')
    """

[<Test>]
let ``insert from select`` () =
    roundtrip """
        insert into Groups(Id, Name)
        select Id, Name from Groups
    """

[<Test>]
let ``insert row syntax`` () =
    roundtrip """
        insert into Groups row
            Id = 1
            , Name = 'g'
    """

[<Test>]
let ``delete`` () =
    roundtrip """
        delete from Users where Email like '%earthlink.net'
    """

[<Test>]
let ``drop`` () =
    roundtrip """
        drop table main.Users
    """

[<Test>]
let ``create table with column list and fk`` () =
    roundtrip """
        create table Foo
            ( bar int primary key
            , baz float32
            , foreign key (bar, baz) references Users(Email, Name)
            );
    """

[<Test>]
let ``between`` () =
    roundtrip """
        select * from Users where Id between 1+1 and 2+2
    """

[<Test>]
let ``create index`` () =
    roundtrip """
        create index IX_User on Users
            (Id, Name);
    """

[<Test>]
let ``alter table add column`` () =
    roundtrip """
        alter table UserGroupMaps
            add column Tag int null
    """

[<Test>]
let ``alter table rename to`` () =
    roundtrip """
        alter table UserGroupMaps rename to UserGroupAssociations
    """

[<Test>]
let ``create temp view`` () =
    roundtrip """
        create temp view CoolUsers as select * from Users where name not like '%szany%'
    """

[<Test>]
let ``create temp view with column names`` () =
    roundtrip """
        create temp view CoolUsers(id, name) as select 1, '' from users where name not like '%szany%'
    """

[<Test>]
let ``create temp view and select from it`` () =
    roundtrip """
        create temp view CoolUsers(id, name) as select 1, '' from users where name not like '%szany%';
        select * from CoolUsers;
    """

[<Test>]
let ``create table with composite PK`` () =
    roundtrip """
        create table Maps(UserId int, GroupId int, primary key(UserId, GroupId))
    """

[<Test>]
let ``many nav property`` () =
    roundtrip """
        select u.*, many Groups(g.*)
        from Users u
        left join UserGroupMaps gm on gm.UserId = u.Id
        left join Groups g on g.Id = gm.GroupId
    """
     
[<Test>]
let ``one nav property`` () =
    roundtrip """
        select u.*, one Group(g.*)
        from Users u
        left join UserGroupMaps gm on gm.UserId = u.Id
        left join Groups g on g.Id = gm.GroupId
    """

[<Test>]
let ``date literals`` () =
    roundtrip """
        select *
        from Users u
        where 2016-10-16 > 2015-01-01
    """
    roundtrip """
        select *
        from Users u
        where 2016-10-16T04:30:31 > 2016-10-16T18:14:19.123
    """
    roundtrip """
        select *
        from Users u
        where 2016-10-16T04:30:31+01:30 > 2016-10-16T18:14:19.123-04:00
    """

[<Test>]
let ``join subqueries`` () =
    roundtrip """
        select * from
            (select u.Id from Users u) us
            join
            (select g.Id from Groups g) gs
            on us.Id = gs.Id
    """

[<Test>]
let ``simple CTE`` () =
    roundtrip """
        with
            a(x, y) as
                ( select Id, 1 from Users )
        select * from a;
    """

[<Test>]
let ``recursive CTE`` () =
    roundtrip """
        with recursive
            nums(x) as (
                select 1
                union all
                select x+1 from nums
                limit 1000000
            )
        select x from nums;
    """

[<Test>]
let ``recursive CTE with implicit column names`` () =
    roundtrip """
        with recursive
            nums as (
                select 1 as myname
                union all
                select myname+1 from nums
                limit 1000000
            )
        select myname from nums;
    """

[<Test>]
let ``table with self-referential constraints`` () =
    roundtrip """
        create table Folders
            ( Id int primary key autoincrement
            , Name string(80)
            , ParentId int references Folders(Id)
            );
    """