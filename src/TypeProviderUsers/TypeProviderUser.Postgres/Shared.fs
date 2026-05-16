namespace TypeProviderUser.Postgres
open Microsoft.Extensions.Configuration
open Microsoft.Extensions.DependencyInjection
open NUnit.Framework
open Rezoom.SQL
open Rezoom.SQL.Mapping
open Rezoom.SQL.Migrations
open Rezoom.SQL.Synchronous
open System

type TestModel = SQLModel<".">

type CleanTestData = SQL<"""
vendor postgres {
    drop table __RZSQL_MIGRATIONS;
    drop table ArticleComments;
    drop table Articles;
    drop table Users;
    drop table Pictures;
}
""">

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
    /// Process-wide service provider. Configuration sources, in order:
    ///  1. appsettings.json (default Host=localhost;Database=rz;Username=rz;Password=testtest)
    ///  2. Env vars. REZOOM_TPU_POSTGRES overrides ConnectionStrings:rzsql.
    let services : IServiceProvider =
        let configuration =
            ConfigurationBuilder()
                .AddJsonFile("appsettings.json", optional = false)
                .AddEnvironmentVariables()
                .Build()
        let envOverride = Environment.GetEnvironmentVariable("REZOOM_TPU_POSTGRES")
        if not (String.IsNullOrEmpty envOverride) then
            configuration.["ConnectionStrings:rzsql"] <- envOverride
        let collection = ServiceCollection()
        collection.AddSingleton<IConfiguration>(configuration :> IConfiguration) |> ignore
        collection.BuildServiceProvider() :> IServiceProvider

    let connectionProvider = ConnectionProvider.ResolveFrom(services)

    let private postgresProbe =
        lazy
            try
                use conn = connectionProvider.Open("rzsql", "postgres")
                conn.Dispose()
                Ok ()
            with exn -> Error (sprintf "%s: %s" (exn.GetType().Name) exn.Message)

    /// Skips the calling test (NUnit Inconclusive) if Postgres isn't reachable
    /// with the configured connection string. Surfaces the underlying error so
    /// configuration mistakes don't look like silent skips.
    let requirePostgres () =
        match postgresProbe.Value with
        | Ok () -> ()
        | Error msg ->
            Assert.Ignore
                ( "Skipping Postgres TP test: connection probe failed -- "
                + msg
                + " (override the connection string via REZOOM_TPU_POSTGRES)" )

    let runOnTestData (cmd : Command<'a>) =
        requirePostgres ()
        TestModel.Migrate(MigrationConfig.Default, services)
        do
            use cxt = new ConnectionContext(connectionProvider)
            CleanTestData.Command().Execute(cxt)
        TestModel.Migrate(MigrationConfig.Default, services)
        use cxt = new ConnectionContext(connectionProvider)
        TestData.Command().Execute(cxt)
        cmd.Execute(cxt)
