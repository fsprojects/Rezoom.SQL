module Rezoom.SQL.Test.TestTSQLSmoke
open NUnit.Framework

[<Test>]
let ``smoke test tsql`` () =
    { tsqlTest with
        Migration =
            """
create table dbo.SmokeTable1
( AutoColumn int primary key autoincrement
, GuidColumn guid
, StringMaxColumn string
, StringNColumn string(80)
, StringCollatedColumn string(32) null collate SQL_Latin1_General_CP1_CI_AS default ('test') unique
, BinaryMaxColumn binary
, BinaryNColumn binary(64)
, Float32Column float32
, Float64Column float64
, DecimalColumn decimal
, BooleanColumn bool
, DateTimeColumn datetime
, DateTimeOffsetColumn datetimeoffset
);

create table SmokeTableFriend( AutoColumn int64 primary key autoincrement );

create table SmokeTable2
( AutoColumn int64 primary key autoincrement
, ParentId int references SmokeTable1(AutoColumn) on delete cascade
, FriendId int64 null references SmokeTableFriend(AutoColumn) on delete set null
);

create index IX_SmokeTable2_ParentId on SmokeTable2(ParentId desc);

alter table dbo.SmokeTable1 add default for Float64Column 1.0;
alter table dbo.SmokeTable1 drop default for Float64Column;

alter table SmokeTable1 alter column StringNColumn collate SQL_Latin1_General_CP1_CI_AS;
alter table SmokeTable1 alter column Float64Column null;
alter table SmokeTable1 alter column Float64Column not null;
alter table SmokeTable1 alter column BooleanColumn int16;

drop index IX_SmokeTable2_ParentId;

alter table SmokeTable2 drop constraint SmokeTable2_FriendId_FK_SmokeTableFriend_AutoColumn;
alter table SmokeTable2 drop column FriendId;
drop table SmokeTable2;
drop table SmokeTable1;

create table dbo.SmokeTable1
( AutoColumn int primary key autoincrement
, GuidColumn guid
, StringMaxColumn string
, StringNColumn string(80)
, StringCollatedColumn string(32) null collate SQL_Latin1_General_CP1_CI_AS default ('test') unique
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
CREATE TABLE [dbo].[SmokeTable1] ( [AutoColumn] INT NOT NULL CONSTRAINT [SmokeTable1_AutoColumn_PK] PRIMARY KEY IDENTITY(1,1)
, [GuidColumn] UNIQUEIDENTIFIER NOT NULL
, [StringMaxColumn] NVARCHAR(max) NOT NULL
, [StringNColumn] NVARCHAR(80) NOT NULL
, [StringCollatedColumn] NVARCHAR(32) COLLATE SQL_Latin1_General_CP1_CI_AS CONSTRAINT [SmokeTable1_StringCollatedColumn_DEFAULT_CONSTRAINT] DEFAULT N'test' CONSTRAINT [SmokeTable1_StringCollatedColumn_UNIQUE] UNIQUE
, [BinaryMaxColumn] VARBINARY(max) NOT NULL
, [BinaryNColumn] VARBINARY(64) NOT NULL
, [Float32Column] FLOAT(24) NOT NULL
, [Float64Column] FLOAT(53) NOT NULL
, [DecimalColumn] NUMERIC(38, 19) NOT NULL
, [BooleanColumn] BIT NOT NULL
, [DateTimeColumn] DATETIME2 NOT NULL
, [DateTimeOffsetColumn] DATETIMEOFFSET NOT NULL );
CREATE TABLE [SmokeTableFriend] ( [AutoColumn] BIGINT NOT NULL CONSTRAINT [SmokeTableFriend_AutoColumn_PK] PRIMARY KEY IDENTITY(1,1) );
CREATE TABLE [SmokeTable2] ( [AutoColumn] BIGINT NOT NULL CONSTRAINT [SmokeTable2_AutoColumn_PK] PRIMARY KEY IDENTITY(1,1)
, [ParentId] INT NOT NULL CONSTRAINT [SmokeTable2_ParentId_FK_SmokeTable1_AutoColumn] REFERENCES [SmokeTable1] ([AutoColumn]) ON DELETE CASCADE
, [FriendId] BIGINT CONSTRAINT [SmokeTable2_FriendId_FK_SmokeTableFriend_AutoColumn] REFERENCES [SmokeTableFriend] ([AutoColumn]) ON DELETE SET NULL );
CREATE INDEX [IX_SmokeTable2_ParentId] ON [SmokeTable2] ( [ParentId] DESC );
ALTER TABLE [dbo].[SmokeTable1] ADD CONSTRAINT [SmokeTable1_Float64Column_DEFAULT_CONSTRAINT] DEFAULT 1.0 FOR [Float64Column];
ALTER TABLE [dbo].[SmokeTable1] DROP CONSTRAINT [SmokeTable1_Float64Column_DEFAULT_CONSTRAINT];
ALTER TABLE [SmokeTable1] ALTER COLUMN [StringNColumn] NVARCHAR(80) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL;
ALTER TABLE [SmokeTable1] ALTER COLUMN [Float64Column] FLOAT(53) NULL;
ALTER TABLE [SmokeTable1] ALTER COLUMN [Float64Column] FLOAT(53) NOT NULL;
ALTER TABLE [SmokeTable1] ALTER COLUMN [BooleanColumn] SMALLINT NOT NULL;
RZSQL_DISTINCTIVE_BATCH_SEPARATORDROP INDEX [IX_SmokeTable2_ParentId] ON [dbo].[SmokeTable2]RZSQL_DISTINCTIVE_BATCH_SEPARATOR;
ALTER TABLE [SmokeTable2] DROP CONSTRAINT [SmokeTable2_FriendId_FK_SmokeTableFriend_AutoColumn];
ALTER TABLE [SmokeTable2] DROP COLUMN [FriendId];
RZSQL_DISTINCTIVE_BATCH_SEPARATORDROP TABLE [SmokeTable2]RZSQL_DISTINCTIVE_BATCH_SEPARATOR;
RZSQL_DISTINCTIVE_BATCH_SEPARATORDROP TABLE [SmokeTable1]RZSQL_DISTINCTIVE_BATCH_SEPARATOR;
CREATE TABLE [dbo].[SmokeTable1] ( [AutoColumn] INT NOT NULL CONSTRAINT [SmokeTable1_AutoColumn_PK] PRIMARY KEY IDENTITY(1,1)
, [GuidColumn] UNIQUEIDENTIFIER NOT NULL
, [StringMaxColumn] NVARCHAR(max) NOT NULL
, [StringNColumn] NVARCHAR(80) NOT NULL
, [StringCollatedColumn] NVARCHAR(32) COLLATE SQL_Latin1_General_CP1_CI_AS CONSTRAINT [SmokeTable1_StringCollatedColumn_DEFAULT_CONSTRAINT] DEFAULT N'test' CONSTRAINT [SmokeTable1_StringCollatedColumn_UNIQUE] UNIQUE
, [BinaryMaxColumn] VARBINARY(max) NOT NULL
, [BinaryNColumn] VARBINARY(64) NOT NULL
, [Float32Column] FLOAT(24) NOT NULL
, [Float64Column] FLOAT(53) NOT NULL
, [DecimalColumn] NUMERIC(38, 19) NOT NULL
, [BooleanColumn] BIT NOT NULL
, [DateTimeColumn] DATETIME2 NOT NULL
, [DateTimeOffsetColumn] DATETIMEOFFSET NOT NULL );
                    """ |> Some
                OutputCommand =
                    """
WITH [cte] AS (
    SELECT [SmokeTable1].[GuidColumn] , [SmokeTable1].[DateTimeOffsetColumn]
    FROM [SmokeTable1]
    WHERE ([SmokeTable1].[AutoColumn] = 0)
)
((SELECT [s].[BooleanColumn]
    FROM [SmokeTable1] AS [s]
    INNER JOIN [cte] AS [c] ON ([s].[GuidColumn] = [c].[GuidColumn])
    WHERE (((([s].[Float64Column] > 0.0) AND ([s].[StringMaxColumn] LIKE N'things%stuff'))
    AND (EXISTS(SELECT 1 AS [x] FROM [SmokeTable1])))
    AND (EXISTS(SELECT [s].[DateTimeOffsetColumn] INTERSECT SELECT [c].[DateTimeOffsetColumn])))
UNION ALL
SELECT 1 AS [BooleanColumn])
EXCEPT SELECT 0 AS [BooleanColumn]);
                    """ |> Some
            } |> Good
    } |> assertSimple