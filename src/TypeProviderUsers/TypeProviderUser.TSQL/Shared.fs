namespace TypeProviderUser.TSQL
open Rezoom.SQL
open Rezoom.SQL.Mapping
open Rezoom.SQL.Migrations
open Rezoom.SQL.Synchronous
open System.IO

type TestModel = SQLModel<".">

type TestData = SQL<"""
delete from ArticleComments;
delete from Articles;
delete from Users;
delete from Pictures;

insert into Pictures(SHA256, PNGData)
values  ( x'ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff'
        , x''
        );
insert into Pictures(SHA256, PNGData)
values  ( x'0000000000000000000000000000000000000000000000000000000000000000'
        , x''
        );

vendor tsql {
  set identity_insert dbo.Users on
};

insert into Users(Id,Name, Email, ProfilePictureSHA256, Created, RandomId)
values  ( 1, 'Homer'
        , 'homer.simpson@springfieldnuclear.com'
        , x'ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff'
        , 2017-01-01T00:00:00
        , (newid())
        );
insert into Users(Id, Name, Email, ProfilePictureSHA256, Created, RandomId)
values  ( 2, 'Marge'
        , 'marge@globex.com'
        , x'0000000000000000000000000000000000000000000000000000000000000000'
        ,  2017-01-01T00:00:00
        , (newid())
        );

vendor tsql {
  set identity_insert dbo.Users off
};

insert into Articles(AuthorId, ArticleTitle, ArticleText)
values  ( (select Id from Users where Name = 'Homer')
        , 'My first review as a food critic.'
        , 'Mmmmmmm... donuts'
        );
insert into Articles(AuthorId, ArticleTitle, ArticleText)
values  ( (select Id from Users where Name = 'Homer')
        , 'My second review as a food critic.'
        , 'Mmmmmmm... beer'
        );
insert into ArticleComments(AuthorId, ArticleId, CommentText)
values  ( (select Id from Users where Name = 'Marge')
        , (select Id from Articles where ArticleTitle = 'My first review as a food critic.')
        , 'Are you sure you should be eating so many donuts?'
        );
insert into ArticleComments(AuthorId, ArticleId, CommentText)
values  ( (select Id from Users where Name = 'Marge')
        , (select Id from Articles where ArticleTitle = 'My second review as a food critic.')
        , 'Are you sure you should be drinking so many beers?'
        );
""">

[<AutoOpen>]
module Helpers =
  let runOnTestData (cmd : Command<'a>) =
      TestModel.Migrate(MigrationConfig.Default)
      do
        use cxt = new ConnectionContext()
        do 
(*          use cnx = cxt.GetConnection("rzsql")
          cnx.CreateCommand(CommandText = """
delete from dbo.articlecomments
delete from dbo.articles   
delete from dbo.users
delete from dbo.pictures
          """).ExecuteNonQuery() |> ignore*)
        TestData.Command().Execute(cxt)
      TestModel.Migrate(MigrationConfig.Default)
      use cxt = new ConnectionContext()
      TestData.Command().Execute(cxt)
      cmd.Execute(cxt)