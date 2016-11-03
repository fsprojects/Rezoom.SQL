namespace Rezoom.SQL.Test
open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open Rezoom.SQL
open Rezoom.SQL.Mapping

[<TestClass>]
type TestRoundTrip() =
    let roundtrip (sql : string) =
        let userModel = UserModel.Load("../../user-model-1", ".")
        let parsed = CommandEffect.OfSQL(userModel.Model, "anonymous", sql)
        let indexer = { new IParameterIndexer with member __.ParameterIndex(_) = 0 }
        let fragments = userModel.Backend.ToCommandFragments(indexer, parsed.Statements)
        let str = CommandFragment.Stringize(fragments)
        Console.WriteLine(str)
        let parsedBack = CommandEffect.OfSQL(userModel.Model, "readback", str)
        let fragmentsBack = userModel.Backend.ToCommandFragments(indexer, parsedBack.Statements)
        let strBack = CommandFragment.Stringize(fragmentsBack)
        Console.WriteLine(String('-', 80))
        Console.WriteLine(strBack)
        Assert.AreEqual(str, strBack)

    [<TestMethod>]
    member __.TestSelect() =
        roundtrip """
            select * from Users u where u.Id = 1
        """
    [<TestMethod>]
    member __.TestFancySelect() =
        roundtrip """
            select g.*, u.*
            from Users u
            left join UserGroupMaps gm on gm.UserId = u.Id
            left join Groups g on g.Id = gm.GroupId
            where g.Name like '%grp%' escape '%'
        """

    [<TestMethod>]
    member __.TestInsert() =
        roundtrip """
            insert into Users(id, name)
            values (0, 'ben')
        """

    [<TestMethod>]
    member __.TestInsertFromSelect() =
        roundtrip """
            insert into Groups
            select * from Groups
        """

    [<TestMethod>]
    member __.TestDelete() =
        roundtrip """
            delete from Users where Email like '%earthlink.net'
        """

    [<TestMethod>]
    member __.TestDrop() =
        roundtrip """
            drop table main.Users
        """

    [<TestMethod>]
    member __.TestCreate() =
        roundtrip """
            create table Foo
                ( bar int primary key not null
                , baz float32
                , foreign key (bar, baz) references Users(Email, Name)
                );
        """

    [<TestMethod>]
    member __.TestAlterAddColumn() =
        roundtrip """
            alter table UserGroupMaps
                add Tag int not null
        """

    [<TestMethod>]
    member __.TestAlterRename() =
        roundtrip """
            alter table UserGroupMaps rename to UserGroupAssociations
        """

    [<TestMethod>]
    member __.TestCreateView() =
        roundtrip """
            create temp view CoolUsers as select * from Users where name not like '%szany%'
        """

    [<TestMethod>]
    member __.TestCreateCompositePK() =
        roundtrip """
            create table Maps(UserId int, GroupId int, primary key(UserId, GroupId))
        """

    [<TestMethod>]
    member __.TestMany() =
        roundtrip """
            select u.*, many Groups(g.*)
            from Users u
            left join UserGroupMaps gm on gm.UserId = u.Id
            left join Groups g on g.Id = gm.GroupId
        """
     
    [<TestMethod>]
    member __.TestOne() =
        roundtrip """
            select u.*, many Group(g.*)
            from Users u
            left join UserGroupMaps gm on gm.UserId = u.Id
            left join Groups g on g.Id = gm.GroupId
        """

    [<TestMethod>]
    member __.TestDateLiterals() =
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

    [<TestMethod>]
    member __.TestSubquery() =
        roundtrip """
            select * from
                (select u.Id from Users u) us
                join
                (select g.Id from Groups g) gs
                on us.Id = gs.Id
        """

    [<TestMethod>]
    member __.TestCTE() =
        roundtrip """
            with
                a(x) as
                    ( select Id from Users )
            select * from a;
        """

    [<TestMethod>]
    member __.TestRecursiveCTE() =
        // TODO: can we *not* require aliases for the selects?
        roundtrip """
            with recursive
                nums(x) as (
                    select 1 as _
                    union all
                    select x+1 as _ from nums
                    limit 1000000
                )
            select x from nums;
        """