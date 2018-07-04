module Rezoom.SQL.Test.TestPostgresSmoke
open NUnit.Framework

[<Test>]
let ``smoke test postgres`` () =
    { postgresTest with
        Migration =
            """
create table public.SmokeTable1
( AutoColumn int primary key autoincrement
, GuidColumn guid
, StringMaxColumn string
, StringNColumn string(80)
, StringCollatedColumn string(32) null collate C default ('test') unique
, BinaryMaxColumn binary
, BinaryNColumn binary(64)
, Float32Column float32
, Float64Column float64
, DecimalColumn decimal
, BooleanColumn bool
, DateTimeColumn datetime
, DateTimeOffsetColumn datetimeoffset
, Int16Column int16
, Int32Column int32
, Int64Column int64
);

create table SmokeTableFriend( AutoColumn int64 primary key autoincrement );

create table SmokeTable2
( AutoColumn int64 primary key autoincrement
, ParentId int references SmokeTable1(AutoColumn) on delete cascade
, FriendId int64 null references SmokeTableFriend(AutoColumn) on delete set null
);

create index IX_SmokeTable2_ParentId on SmokeTable2(ParentId desc);

alter table public.SmokeTable1 add default for Float64Column 1.0;
alter table public.SmokeTable1 drop default for Float64Column;

alter table SmokeTable1 alter column StringNColumn collate C;
alter table SmokeTable1 alter column Float64Column null;
alter table SmokeTable1 alter column Float64Column not null;
alter table SmokeTable1 alter column Int16Column int;

drop index IX_SmokeTable2_ParentId;

alter table SmokeTable2 drop constraint SmokeTable2_FriendId_FK_SmokeTableFriend_AutoColumn;
alter table SmokeTable2 drop column FriendId;
drop table SmokeTable2;
drop table SmokeTable1;

create table public.SmokeTable1
( AutoColumn int primary key autoincrement
, GuidColumn guid
, StringMaxColumn string
, StringNColumn string(80)
, StringCollatedColumn string(32) null collate C default ('test') unique
, BinaryMaxColumn binary
, BinaryNColumn binary(64)
, Float32Column float32
, Float64Column float64
, DecimalColumn decimal
, BooleanColumn bool
, DateTimeColumn datetime
, DateTimeOffsetColumn datetimeoffset
);
            """
        Command =
            """
with cte as (select GuidColumn, DateTimeOffsetColumn from SmokeTable1 where AutoColumn = 0)
select s.BooleanColumn
    from SmokeTable1 s
    join cte c on s.GuidColumn = c.GuidColumn
    where s.Float64Column > 0.0
        and s.StringMaxColumn like 'things%stuff'
        and exists(select 1 x from SmokeTable1)
        and s.DateTimeOffsetColumn is c.DateTimeOffsetColumn 
union all
select true
except
select false
            """
        Expect =
            { expect with
                OutputMigration =
                    """
CREATE TABLE "public"."smoketable1" 
( "autocolumn" SERIAL NOT NULL
CONSTRAINT "smoketable1_autocolumn_pk" PRIMARY KEY
, "guidcolumn" UUID NOT NULL
, "stringmaxcolumn" TEXT NOT NULL
, "stringncolumn" VARCHAR(80) NOT NULL
, "stringcollatedcolumn" VARCHAR(32) COLLATE "C" DEFAULT ('test')
CONSTRAINT "smoketable1_stringcollatedcolumn_unique" UNIQUE
, "binarymaxcolumn" BYTEA NOT NULL
, "binaryncolumn" BYTEA NOT NULL
, "float32column" FLOAT4 NOT NULL
, "float64column" FLOAT8 NOT NULL
, "decimalcolumn" NUMERIC(38, 19) NOT NULL
, "booleancolumn" BOOLEAN NOT NULL
, "datetimecolumn" TIMESTAMPTZ NOT NULL
, "datetimeoffsetcolumn" TIMESTAMPTZ NOT NULL
, "int16column" SMALLINT NOT NULL
, "int32column" INT NOT NULL
, "int64column" BIGINT NOT NULL
);
CREATE TABLE "smoketablefriend" 
( "autocolumn" BIGSERIAL NOT NULL
CONSTRAINT "smoketablefriend_autocolumn_pk" PRIMARY KEY
);
CREATE TABLE "smoketable2" 
( "autocolumn" BIGSERIAL NOT NULL
CONSTRAINT "smoketable2_autocolumn_pk" PRIMARY KEY
, "parentid" INT NOT NULL
CONSTRAINT "smoketable2_parentid_fk_smoketable1_autocolumn" REFERENCES "smoketable1" ("autocolumn") ON DELETE CASCADE
, "friendid" BIGINT
CONSTRAINT "smoketable2_friendid_fk_smoketablefriend_autocolumn" REFERENCES "smoketablefriend" ("autocolumn") ON DELETE SET NULL
);
CREATE INDEX "ix_smoketable2_parentid" ON "smoketable2"
( "parentid" DESC
);
ALTER TABLE "public"."smoketable1" ALTER COLUMN "float64column" SET DEFAULT 1.0;
ALTER TABLE "public"."smoketable1" ALTER COLUMN "float64column" DROP DEFAULT;
ALTER TABLE "smoketable1" ALTER COLUMN "stringncolumn" TYPE VARCHAR(80) COLLATE "C";
ALTER TABLE "smoketable1" ALTER COLUMN "float64column" DROP NOT NULL;
ALTER TABLE "smoketable1" ALTER COLUMN "float64column" SET NOT NULL;
ALTER TABLE "smoketable1" ALTER COLUMN "int16column" TYPE INT USING CAST("int16column" AS INT);
DROP INDEX "ix_smoketable2_parentid";
ALTER TABLE "smoketable2" DROP CONSTRAINT "smoketable2_friendid_fk_smoketablefriend_autocolumn" RESTRICT;
ALTER TABLE "smoketable2" DROP COLUMN "friendid" RESTRICT;
DROP TABLE "smoketable2";
DROP TABLE "smoketable1";
CREATE TABLE "public"."smoketable1" 
( "autocolumn" SERIAL NOT NULL
CONSTRAINT "smoketable1_autocolumn_pk" PRIMARY KEY
, "guidcolumn" UUID NOT NULL
, "stringmaxcolumn" TEXT NOT NULL
, "stringncolumn" VARCHAR(80) NOT NULL
, "stringcollatedcolumn" VARCHAR(32) COLLATE "C" DEFAULT ('test')
CONSTRAINT "smoketable1_stringcollatedcolumn_unique" UNIQUE
, "binarymaxcolumn" BYTEA NOT NULL
, "binaryncolumn" BYTEA NOT NULL
, "float32column" FLOAT4 NOT NULL
, "float64column" FLOAT8 NOT NULL
, "decimalcolumn" NUMERIC(38, 19) NOT NULL
, "booleancolumn" BOOLEAN NOT NULL
, "datetimecolumn" TIMESTAMPTZ NOT NULL
, "datetimeoffsetcolumn" TIMESTAMPTZ NOT NULL
);
                    """ |> Some
                OutputCommand =
                    """
WITH "cte"
AS (
SELECT
"smoketable1"."guidcolumn"
, "smoketable1"."datetimeoffsetcolumn"
FROM "smoketable1"
WHERE ("smoketable1"."autocolumn" = 0)
)
SELECT
"s"."booleancolumn"
FROM "smoketable1" AS "s"
INNER JOIN
"cte" AS "c" ON ("s"."guidcolumn" = "c"."guidcolumn")
WHERE (((("s"."float64column" > 0.0) AND ("s"."stringmaxcolumn" LIKE 'things%stuff')) AND (EXISTS(SELECT
1 AS "x"
FROM "smoketable1"))) AND ("s"."datetimeoffsetcolumn" IS NOT DISTINCT FROM "c"."datetimeoffsetcolumn"))
UNION ALL
SELECT
TRUE AS "booleancolumn"
EXCEPT
SELECT
FALSE AS "booleancolumn";
                    """ |> Some
            } |> Good
    } |> assertSimple