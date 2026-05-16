namespace TypeProviderUser.SQLite
open Microsoft.Extensions.Configuration
open Microsoft.Extensions.DependencyInjection
open Rezoom.Execution
open Rezoom.SQL
open Rezoom.SQL.Mapping
open Rezoom.SQL.Migrations
open Rezoom.SQL.Synchronous
open System
open System.IO

type TestModel = SQLModel<".">

type TestData = SQL<"""
insert into Pictures(SHA256, PNGData)
values  ( x'ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff'
        , x''
        );
insert into Pictures(SHA256, PNGData)
values  ( x'0000000000000000000000000000000000000000000000000000000000000000'
        , x''
        );
insert into Users(Name, Email, ProfilePictureSHA256, Created)
values  ( 'Homer'
        , 'homer.simpson@springfieldnuclear.com'
        , x'ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff'
        , 2017-01-01T00:00:00
        );
insert into Users(Name, Email, ProfilePictureSHA256, Created)
values  ( 'Marge'
        , 'marge@globex.com'
        , x'0000000000000000000000000000000000000000000000000000000000000000'
        ,  2017-01-01T00:00:00
        );
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
    let dbFileName = "rzsql.db"

    /// Process-wide service provider built once from appsettings.json. Rezoom.SQL's
    /// ConnectionProvider.ResolveFrom falls back to a ConfigurationConnectionProvider
    /// built from the registered IConfiguration, so no other registration is needed.
    let services : IServiceProvider =
        let configuration =
            ConfigurationBuilder()
                .AddJsonFile("appsettings.json", optional = false)
                .AddEnvironmentVariables()
                .Build() :> IConfiguration
        let collection = ServiceCollection()
        collection.AddSingleton<IConfiguration>(configuration) |> ignore
        collection.BuildServiceProvider() :> IServiceProvider

    let connectionProvider = ConnectionProvider.ResolveFrom(services)

    let executionConfig =
        { ExecutionConfig.Default with Services = services }

    let private freshDatabase () =
        // Microsoft.Data.Sqlite keeps a pooled connection open on the file, so a
        // direct File.Delete fails with "in use by another process". Drain the
        // pool first.
        Microsoft.Data.Sqlite.SqliteConnection.ClearAllPools()
        if File.Exists(dbFileName) then File.Delete(dbFileName)
        TestModel.Migrate(MigrationConfig.Default, services)

    let runOnTestData (cmd : Command<'a>) =
        freshDatabase ()
        use cxt = new ConnectionContext(connectionProvider)
        TestData.Command().Execute(cxt)
        cmd.Execute(cxt)
