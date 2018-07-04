module Rezoom.SQL.Test.TestPostgres
open NUnit.Framework
open Rezoom.SQL.Compiler

[<Test>]
let ``postgres example migration script`` () =
    { postgresTest with
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
, RandomId guid default(cast('a8078caeae944136ade0f2bf06792a92' as guid))
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
CREATE TABLE "pictures"
( "sha256" BYTEA NOT NULL CONSTRAINT "pictures_sha256_pk" PRIMARY KEY
, "pngdata" BYTEA NOT NULL
);
CREATE TABLE "users"
( "id" BIGSERIAL NOT NULL CONSTRAINT "users_id_pk" PRIMARY KEY
, "name" VARCHAR(80) NOT NULL
, "email" VARCHAR(254) NOT NULL
, "profilepicturesha256" BYTEA CONSTRAINT "users_profilepicturesha256_fk_pictures_sha256"
    REFERENCES "pictures" ("sha256")
, "created" TIMESTAMPTZ NOT NULL
, "randomid" UUID NOT NULL DEFAULT (CAST('a8078caeae944136ade0f2bf06792a92' AS UUID))
);
CREATE TABLE "articles"
( "id" BIGSERIAL NOT NULL CONSTRAINT "articles_id_pk" PRIMARY KEY
, "authorid" BIGINT NOT NULL CONSTRAINT "articles_authorid_fk_users_id" REFERENCES "users" ("id")
, "articletitle" VARCHAR(80) NOT NULL
, "articletext" VARCHAR(4096) NOT NULL
);
CREATE INDEX "ix_articles_authorid" ON "articles" ( "authorid" ASC );
CREATE TABLE "articlecomments"
( "id" BIGSERIAL NOT NULL CONSTRAINT "articlecomments_id_pk" PRIMARY KEY
, "articleid" BIGINT NOT NULL CONSTRAINT "articlecomments_articleid_fk_articles_id" REFERENCES "articles" ("id")
, "authorid" BIGINT NOT NULL CONSTRAINT "articlecomments_authorid_fk_users_id" REFERENCES "users" ("id")
, "commenttext" VARCHAR(512) NOT NULL
);
CREATE INDEX "ix_articlecomments_authorid" ON "articlecomments" ( "authorid" ASC );
                    """ |> Some
            } |> Good
    } |> assertSimple

[<Test>]
let ``postgres dump function signatures`` () =
    for KeyValue(_, func) in postgresTest.TestBackend.InitialModel.Builtin.Functions do
        printfn "%s" (dumpSignature func)
        printfn ""