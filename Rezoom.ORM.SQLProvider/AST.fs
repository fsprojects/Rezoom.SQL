namespace Rezoom.ORM.SQLProvider
open System
open System.Collections.Generic
open System.Globalization

type NumericLiteral =
    | IntegerLiteral of uint64
    | FloatLiteral of float

type SignedNumericLiteral =
    {
        Sign : int // -1, 0, 1
        Value : NumericLiteral
    }

type Literal =
    | NullLiteral
    | CurrentTimeLiteral
    | CurrentDateLiteral
    | CurrentTimestampLiteral
    | StringLiteral of string
    | BlobLiteral of byte array
    | NumericLiteral of NumericLiteral

type SavepointName = Name

type Alias = Name option

type TypeBounds =
    {
        Low : SignedNumericLiteral
        High : SignedNumericLiteral option
    }

type TypeName =
    {
        TypeName : Name list
        Bounds : TypeBounds option
    }

type ObjectName =
    {
        SchemaName : Name option
        ObjectName : Name
    }
    override this.ToString() =
        string <|
        match this.SchemaName with
        | None -> this.ObjectName
        | Some schema -> schema + "." + this.ObjectName

type ColumnName =
    {
        Table : ObjectName option
        ColumnName : Name
    }
    override this.ToString() =
        string <|
        match this.Table with
        | None -> this.ColumnName
        | Some tbl -> string tbl + "." + this.ColumnName

type BindParameter =
    | NamedParameter of char * string // char is the prefix: ':', '@', or '$'
    | PositionalParameter of uint32 option
    
type BinaryOperator =
    | Concatenate
    | Multiply
    | Divide
    | Modulo
    | Add
    | Subtract
    | BitShiftLeft
    | BitShiftRight
    | BitAnd
    | BitOr
    | LessThan
    | LessThanOrEqual
    | GreaterThan
    | GreaterThanOrEqual
    | Equal
    | NotEqual
    | Is
    | IsNot
    | And
    | Or

type UnaryOperator =
    | Negative
    | Not
    | BitNot

type SimilarityOperator =
    | Like
    | Glob
    | Match
    | Regexp

type Raise =
    | RaiseIgnore
    | RaiseRollback of string
    | RaiseAbort of string
    | RaiseFail of string

type ExprType =
    | LiteralExpr of Literal
    | BindParameterExpr of BindParameter
    | ColumnNameExpr of ColumnName
    | CastExpr of CastExpr
    | CollateExpr of Expr * Name
    | FunctionInvocationExpr of FunctionInvocationExpr
    | SimilarityExpr of SimilarityOperator * Expr * Expr * Expr option // optional ESCAPE clause
    | BinaryExpr of BinaryOperator * Expr * Expr
    | UnaryExpr of UnaryOperator * Expr
    | BetweenExpr of Expr * Expr * Expr
    | NotBetweenExpr of Expr * Expr * Expr
    | InExpr of Expr * InSet WithSource
    | NotInExpr of Expr * InSet WithSource
    | ExistsExpr of SelectStmt
    | CaseExpr of CaseExpr
    | ScalarSubqueryExpr of SelectStmt
    | RaiseExpr of Raise

and Expr = ExprType WithSource

and CastExpr =
    {
        Expression : Expr
        AsType : TypeName
    }
 
and TableInvocation =
    {
        Table : ObjectName
        Arguments : Expr ResizeArray option // we use an option to distinguish between schema.table and schema.table()
    }

and FunctionInvocationExpr =
    {
        FunctionName : Name
        Arguments : FunctionArguments
    }

and CaseExpr =
    {
        Input : Expr option
        Cases : (Expr * Expr) ResizeArray
        Else : Expr option WithSource
    }

and Distinct = | Distinct

and DistinctColumns =
    | DistinctColumns
    | AllColumns

and FunctionArguments =
    | ArgumentWildcard
    | ArgumentList of (Distinct option * Expr ResizeArray)

and InSet =
    | InExpressions of Expr ResizeArray
    | InSelect of SelectStmt
    | InTable of TableInvocation

and SelectStmtCore =
    {
        With : WithClause option
        Compound : CompoundExpr
        OrderBy : OrderingTerm ResizeArray option
        Limit : Limit option
    }

and SelectStmt = SelectStmtCore WithSource

and WithClause =
    {
        Recursive : bool
        Tables : CommonTableExpression ResizeArray
    }

and CommonTableExpression =
    {
        Name : Name
        ColumnNames : Name ResizeArray WithSource option
        AsSelect : SelectStmt
    }

and OrderDirection =
    | Ascending
    | Descending

and OrderingTerm =
    {
        By : Expr
        Direction : OrderDirection
    }

and Limit =
    {
        Limit : Expr
        Offset : Expr option
    }

and CompoundExprCore =
    | CompoundTerm of CompoundTerm
    | Union of CompoundExpr * CompoundTerm
    | UnionAll of CompoundExpr * CompoundTerm
    | Intersect of CompoundExpr * CompoundTerm
    | Except of CompoundExpr * CompoundTerm

and CompoundExpr = CompoundExprCore WithSource

and CompoundTermCore =
    | Values of Expr ResizeArray WithSource ResizeArray
    | Select of SelectCore

and CompoundTerm = CompoundTermCore WithSource

and SelectCore =
    {
        Columns : ResultColumns
        From : TableExpr option
        Where : Expr option
        GroupBy : GroupBy option
    }

and GroupBy =
    {
        By : Expr ResizeArray
        Having : Expr option
    }

and ResultColumns =
    {
        Distinct : DistinctColumns option
        Columns : ResultColumn WithSource ResizeArray
    }

and ResultColumn =
    | ColumnsWildcard
    | TableColumnsWildcard of ObjectName
    | Column of Expr * Alias

and IndexHint =
    | IndexedBy of Name
    | NotIndexed

and QualifiedTableName =
    {
        TableName : ObjectName
        IndexHint : IndexHint option
    }

and TableOrSubquery =
    | Table of TableInvocation * Alias * IndexHint option // note: an index hint is invalid if the table has args
    | Subquery of SelectStmt * Alias

and JoinType =
    | Inner
    | LeftOuter
    | Cross
    | Natural of JoinType

and JoinConstraint =
    | JoinOn of Expr
    | JoinUsing of Name ResizeArray
    | JoinUnconstrained

and Join =
    {
        JoinType : JoinType
        LeftTable : TableExpr
        RightTable : TableExpr
        Constraint : JoinConstraint
    }

and TableExprCore =
    | TableOrSubquery of TableOrSubquery
    | AliasedTableExpr of TableExpr * Alias
    | Join of Join

and TableExpr = TableExprCore WithSource

type ConflictClause =
    | Rollback
    | Abort
    | Fail
    | Ignore
    | Replace

type ForeignKeyEvent =
    | OnDelete
    | OnUpdate

type ForeignKeyEventHandler =
    | SetNull
    | SetDefault
    | Cascade
    | Restrict
    | NoAction

type ForeignKeyRule =
    | MatchRule of Name
    | EventRule of (ForeignKeyEvent * ForeignKeyEventHandler)

type ForeignKeyDeferClause =
    {
        Deferrable : bool
        InitiallyDeferred : bool option
    }

type ForeignKeyClause =
    {
        ReferencesTable : ObjectName
        ReferencesColumns : Name ResizeArray option
        Rules : ForeignKeyRule ResizeArray
        Defer : ForeignKeyDeferClause option
    }

type PrimaryKeyClause =
    {
        Order : OrderDirection
        ConflictClause : ConflictClause option
        AutoIncrement : bool
    }

type ColumnConstraintType =
    | NullableConstraint
    | PrimaryKeyConstraint of PrimaryKeyClause
    | NotNullConstraint of ConflictClause option
    | UniqueConstraint of ConflictClause option
    | CheckConstraint of Expr
    | DefaultConstraint of Expr
    | CollateConstraint of Name
    | ForeignKeyConstraint of ForeignKeyClause

type ColumnConstraint =
    {
        Name : Name option
        ColumnConstraintType : ColumnConstraintType
    }

type ColumnDef =
    {
        Name : Name
        Type : TypeName option
        Constraints : ColumnConstraint ResizeArray
    }

type AlterTableAlteration =
    | RenameTo of Name
    | AddColumn of ColumnDef

type AlterTableStmt =
    {
        Table : ObjectName
        Alteration : AlterTableAlteration
    }

type TableIndexConstraintType =
    | PrimaryKey
    | Unique

type TableIndexConstraintClause =
    {
        Type : TableIndexConstraintType
        IndexedColumns : (Expr * OrderDirection) ResizeArray
        ConflictClause : ConflictClause option
    }

type TableConstraintType =
    | TableIndexConstraint of TableIndexConstraintClause
    | TableForeignKeyConstraint of Name ResizeArray * ForeignKeyClause
    | TableCheckConstraint of Expr

type TableConstraint =
    {
        Name : Name option
        TableConstraintType : TableConstraintType
    }

type CreateTableDefinition =
    {
        Columns : ColumnDef ResizeArray
        Constraints : TableConstraint ResizeArray
        WithoutRowId : bool
    }

type CreateTableAs =
    | CreateAsDefinition of CreateTableDefinition
    | CreateAsSelect of SelectStmt

type CreateTableStmt =
    {
        Temporary : bool
        IfNotExists : bool
        Name : ObjectName WithSource
        As : CreateTableAs
    }

type TransactionType =
    | Deferred
    | Immediate
    | Exclusive

type CreateIndexStmt =
    {
        Unique : bool
        IfNotExists : bool
        IndexName : ObjectName
        TableName : ObjectName
        IndexedColumns : (Expr * OrderDirection) ResizeArray
        Where : Expr option
    }

type DeleteStmt =
    {
        With : WithClause option
        DeleteFrom : QualifiedTableName
        Where : Expr option
        OrderBy : OrderingTerm ResizeArray option
        Limit : Limit option
    }

type UpdateOr =
    | UpdateOrRollback
    | UpdateOrAbort
    | UpdateOrReplace
    | UpdateOrFail
    | UpdateOrIgnore

type UpdateStmt =
    {
        With : WithClause option
        UpdateTable : QualifiedTableName
        Or : UpdateOr option
        Set : (Name * Expr) ResizeArray
        Where : Expr option
        OrderBy : OrderingTerm ResizeArray option
        Limit : Limit option
    }

type InsertOr =
    | InsertOrRollback
    | InsertOrAbort
    | InsertOrReplace
    | InsertOrFail
    | InsertOrIgnore

type InsertStmt =
    {
        With : WithClause option
        Or : InsertOr option
        InsertInto : ObjectName
        Columns : Name ResizeArray option
        Data : SelectStmt option // either select/values, or "default values" if none
    }

type TriggerSchedule =
    | Before
    | After
    | InsteadOf

type TriggerCause =
    | DeleteOn
    | InsertOn
    | UpdateOn of Name ResizeArray option

type TriggerAction =
    | TriggerUpdate of UpdateStmt
    | TriggerInsert of InsertStmt
    | TriggerDelete of DeleteStmt
    | TriggerSelect of SelectStmt

type CreateTriggerStmt =
    {
        Temporary : bool
        IfNotExists : bool
        TriggerName : ObjectName
        TableName : ObjectName
        Schedule : TriggerSchedule
        Cause : TriggerCause
        Condition : Expr option
        Actions : TriggerAction ResizeArray
    }

type CreateViewStmt =
    {
        Temporary : bool
        IfNotExists : bool
        ViewName : ObjectName
        ColumnNames : Name ResizeArray option
        AsSelect : SelectStmt
    }

type DropObjectType =
    | DropIndex
    | DropTable
    | DropTrigger
    | DropView

type DropObjectStmt =
    {
        Drop : DropObjectType
        IfExists : bool
        IndexName : ObjectName
    }

type PragmaValue =
    | StringPragmaValue of string
    | NumericPragmaValue of SignedNumericLiteral

type PragmaStmt =
    {
        Pragma : ObjectName
        Value : PragmaValue option
    }

type RollbackStmt =
    | RollbackToSavepoint of SavepointName
    | RollbackTransactionByName of Name
    | RollbackTransaction

type CreateVirtualTableStmt =
    {
        IfNotExists : bool
        VirtualTable : ObjectName
        UsingModule : Name
        WithModuleArguments : string ResizeArray
    }

type Stmt =
    | AlterTableStmt of AlterTableStmt
    | AnalyzeStmt of ObjectName option
    | AttachStmt of Expr * Name
    | BeginStmt of TransactionType
    | CommitStmt
    | CreateIndexStmt of CreateIndexStmt
    | CreateTableStmt of CreateTableStmt
    | CreateTriggerStmt of CreateTriggerStmt
    | CreateViewStmt of CreateViewStmt
    | CreateVirtualTableStmt of CreateVirtualTableStmt
    | DeleteStmt of DeleteStmt
    | DetachStmt of Name
    | DropObjectStmt of DropObjectStmt
    | InsertStmt of InsertStmt
    | PragmaStmt of PragmaStmt
    | ReindexStmt of ObjectName option
    | ReleaseStmt of Name
    | RollbackStmt of RollbackStmt
    | SavepointStmt of SavepointName
    | SelectStmt of SelectStmt
    | ExplainStmt of Stmt
    | UpdateStmt of UpdateStmt
    | VacuumStmt