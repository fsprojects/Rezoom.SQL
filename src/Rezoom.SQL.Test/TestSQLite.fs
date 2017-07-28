module Rezoom.SQL.Test.TestSQLite
open NUnit.Framework
open Rezoom.SQL.Compiler

[<Test>]
let ``sqlite non-idempotent random`` () =
    { sqliteTest with
        Command = "select random() as r;"
        Expect =
            { expect with
                Idempotent = Some false
                ResultSets = Some [ [ "r", { Type = IntegerType Integer64; Nullable = false } ] ]
            } |> Good
    } |> assertSimple

[<Test>]
let ``sqlite non-idempotent randomblob`` () =
    { sqliteTest with
        Command = "select randomblob(4) as r;"
        Expect =
            { expect with
                Idempotent = Some false
                ResultSets = Some [ [ "r", { Type = BinaryType; Nullable = false } ] ]
            } |> Good
    } |> assertSimple

[<Test>]
let ``sqlite non-idempotent random in subquery`` () =
    { sqliteTest with
        Command = "select * from (select random() r) q;"
        Expect =
            { expect with
                Idempotent = Some false
                ResultSets = Some [ [ "r", { Type = IntegerType Integer64; Nullable = false } ] ]
            } |> Good
    } |> assertSimple

[<Test>]
let ``sqlite custom constraint name`` () =
    { sqliteTest with
        Command = "create table X(a int constraint myname unique)"
        Expect =
            { expect with
                OutputCommand =
                    """
                    CREATE TABLE "X"  ( "a" INT NOT NULL CONSTRAINT "myname" UNIQUE );
                    """.Trim() |> Some
            } |> Good
    } |> assertSimple

[<Test>]
let ``sqlite custom table constraint name`` () =
    { sqliteTest with
        Command = "create table X(a int, constraint myname unique (a))"
        Expect =
            { expect with
                OutputCommand =
                    """
                    CREATE TABLE "X"  ( "a" INT NOT NULL , CONSTRAINT "myname" UNIQUE("a" ASC) );
                    """.Trim() |> Some
            } |> Good
    } |> assertSimple

[<Test>]
let ``sqlite example migration script`` () =
    { sqliteTest with
        Command =
            """
create table Pictures
( SHA256 binary(32) primary key
, PNGData binary(4096)
);

create table Users
( Id int64 primary key autoincrement
, Name string(80)
, Email string(254)
, ProfilePictureSHA256 binary(32) null references Pictures(SHA256)
, Created datetime
, RandomId guid default(cast(randomblob(16) as guid))
);

create table Articles
( Id int64 primary key autoincrement
, AuthorId int64 references Users(Id)
, ArticleTitle string(80)
, ArticleText string(4096)
);

create index IX_Articles_AuthorId on Articles(AuthorId);

create table ArticleComments
( Id int64 primary key autoincrement
, ArticleId int64 references Articles(Id)
, AuthorId int64 references Users(Id)
, CommentText string(512)
);

create index IX_ArticleComments_AuthorId on ArticleComments(AuthorId);
            """
        Expect =
            { expect with
                OutputCommand =
                    """
CREATE TABLE "Pictures"
( "SHA256" BLOB NOT NULL CONSTRAINT "Pictures_SHA256_PK" PRIMARY KEY ASC
, "PNGData" BLOB NOT NULL
);
CREATE TABLE "Users"
( "Id" INTEGER NOT NULL CONSTRAINT "Users_Id_PK" PRIMARY KEY ASC AUTOINCREMENT
, "Name" VARCHAR NOT NULL , "Email" VARCHAR NOT NULL
, "ProfilePictureSHA256" BLOB CONSTRAINT "Users_ProfilePictureSHA256_FK_Pictures_SHA256" REFERENCES "Pictures" ("SHA256")
, "Created" VARCHAR NOT NULL
, "RandomId" BLOB NOT NULL DEFAULT (CAST(randomblob(16) AS BLOB))
);
CREATE TABLE "Articles"
( "Id" INTEGER NOT NULL CONSTRAINT "Articles_Id_PK" PRIMARY KEY ASC AUTOINCREMENT
, "AuthorId" INT NOT NULL CONSTRAINT "Articles_AuthorId_FK_Users_Id" REFERENCES "Users" ("Id")
, "ArticleTitle" VARCHAR NOT NULL
, "ArticleText" VARCHAR NOT NULL
);
CREATE INDEX "IX_Articles_AuthorId" ON "Articles" ( "AuthorId" ASC );
CREATE TABLE "ArticleComments"
( "Id" INTEGER NOT NULL CONSTRAINT "ArticleComments_Id_PK" PRIMARY KEY ASC AUTOINCREMENT
, "ArticleId" INT NOT NULL CONSTRAINT "ArticleComments_ArticleId_FK_Articles_Id" REFERENCES "Articles" ("Id")
, "AuthorId" INT NOT NULL CONSTRAINT "ArticleComments_AuthorId_FK_Users_Id" REFERENCES "Users" ("Id")
, "CommentText" VARCHAR NOT NULL
);
CREATE INDEX "IX_ArticleComments_AuthorId" ON "ArticleComments" ( "AuthorId" ASC );
                    """.SmushWhitespace() |> Some
            } |> Good
    } |> assertSimple

[<Test>]
let ``sqlite dump function signatures`` () =
    for KeyValue(_, func) in sqliteTest.TestBackend.InitialModel.Builtin.Functions do
        printfn "%s" (dumpSignature func)
        printfn ""