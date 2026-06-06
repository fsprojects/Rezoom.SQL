namespace TypeProviderUser.TSQL
open Microsoft.Extensions.Configuration
open Microsoft.Extensions.DependencyInjection
open NUnit.Framework
open Rezoom.SQL
open Rezoom.SQL.Mapping
open Rezoom.SQL.Migrations
open Rezoom.SQL.Synchronous
open System.IO
open System

type TestModel = SQLModel<".">

type CleanTestData = SQL<"""
vendor tsql {
    drop table if exists __RZSQL_MIGRATIONS;
    drop table if exists UserLocations;
    drop table if exists UserAddresses;
    drop table if exists ArticleComments;
    drop table if exists Articles;
    drop table if exists Users;
    drop table if exists Pictures;
}
""">

type TestData = SQL<"""
delete from UserLocations;
delete from UserAddresses;
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
    // Process-wide service provider. Configuration sources, in order:
    ///  1. appsettings.json
    ///  2. Env vars. REZOOM_TPU_TSQL overrides ConnectionStrings:rzsql.
    let services : IServiceProvider =
        let configuration =
            ConfigurationBuilder()
                .AddJsonFile("appsettings.json", optional = false)
                .AddEnvironmentVariables()
                .Build()
        let envOverride = Environment.GetEnvironmentVariable("REZOOM_TPU_TSQL")
        if not (String.IsNullOrEmpty envOverride) then
            configuration.["ConnectionStrings:rzsql"] <- envOverride
        let collection = ServiceCollection()
        collection.AddSingleton<IConfiguration>(configuration :> IConfiguration) |> ignore
        collection.BuildServiceProvider() :> IServiceProvider

    let connectionProvider = ConnectionProvider.ResolveFrom(services)

    let runOnTestData (cmd : Command<'a>) =
        TestModel.Migrate(MigrationConfig.Default, services)
        do
            use cxt = new ConnectionContext(connectionProvider)
            CleanTestData.Command().Execute(cxt)
        TestModel.Migrate(MigrationConfig.Default, services)
        use cxt = new ConnectionContext(connectionProvider)
        TestData.Command().Execute(cxt)
        cmd.Execute(cxt)