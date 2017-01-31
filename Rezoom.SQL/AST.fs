namespace Rezoom.SQL
open System
open System.Collections.Generic
open System.Globalization

type NumericLiteral =
    | IntegerLiteral of uint64
    | FloatLiteral of float

type SignedNumericLiteral =
    {   Sign : int // -1, 0, 1
        Value : NumericLiteral
    }

type Literal =
    | NullLiteral
    | BooleanLiteral of bool
    | StringLiteral of string
    | BlobLiteral of byte array
    | NumericLiteral of NumericLiteral
    | DateTimeLiteral of DateTime
    | DateTimeOffsetLiteral of DateTimeOffset

type SavepointName = Name

type Alias = Name option

type IntegerSize =
    | Integer8
    | Integer16
    | Integer32
    | Integer64

type FloatSize =
    | Float32
    | Float64

type TypeName =
    | StringTypeName of maxLength : int option
    | BinaryTypeName of maxLength : int option
    | IntegerTypeName of IntegerSize
    | FloatTypeName of FloatSize
    | DecimalTypeName
    | BooleanTypeName
    | DateTimeTypeName
    | DateTimeOffsetTypeName

[<NoComparison>]
[<CustomEquality>]
type ObjectName<'t> =
    {   Source : SourceInfo
        SchemaName : Name option
        ObjectName : Name
        Info : 't
    }
    override this.ToString() =
        string <|
        match this.SchemaName with
        | None -> this.ObjectName
        | Some schema -> schema + "." + this.ObjectName
    member this.Equals(other) =
        this.SchemaName = other.SchemaName
        && this.ObjectName = other.ObjectName
    override this.Equals(other) =
        match other with
        | :? ObjectName<'t> as other -> this.Equals(other)
        | _ -> false
    override this.GetHashCode() = this.SchemaName +@+ this.ObjectName
    interface IEquatable<ObjectName<'t>> with
        member this.Equals(other) = this.Equals(other)

type ColumnName<'t> =
    {   Table : ObjectName<'t> option
        ColumnName : Name
    }
    override this.ToString() =
        string <|
        match this.Table with
        | None -> this.ColumnName
        | Some tbl -> string tbl + "." + this.ColumnName

type BindParameter =
    | NamedParameter of Name // prefix character : or $ or @ is ignored
    
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
    /// True if this operator expects boolean inputs and has a boolean output.
    member this.IsLogicalOperator =
        match this with
        | And
        | Or -> true
        | _ -> false

type UnaryOperator =
    | Negative
    | Not
    | BitNot
    | NotNull
    | IsNull
    /// True if this operator expects boolean inputs and has a boolean output.
    member this.IsLogicalOperator =
        match this with
        | Not -> true
        | _ -> false

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

type ExprType<'t, 'e> =
    | LiteralExpr of Literal
    | BindParameterExpr of BindParameter
    | ColumnNameExpr of ColumnName<'t>
    | CastExpr of CastExpr<'t, 'e>
    | CollateExpr of CollationExpr<'t, 'e>
    | FunctionInvocationExpr of FunctionInvocationExpr<'t, 'e>
    | SimilarityExpr of SimilarityExpr<'t, 'e>
    | BinaryExpr of BinaryExpr<'t, 'e>
    | UnaryExpr of UnaryExpr<'t, 'e>
    | BetweenExpr of BetweenExpr<'t, 'e>
    | InExpr of InExpr<'t, 'e>
    | ExistsExpr of SelectStmt<'t, 'e>
    | CaseExpr of CaseExpr<'t, 'e>
    | ScalarSubqueryExpr of SelectStmt<'t, 'e>
    | RaiseExpr of Raise

and
    [<NoComparison>]
    [<CustomEquality>]
    Expr<'t, 'e> =
    {   Value : ExprType<'t, 'e>
        Info : 'e
        Source : SourceInfo
    }
    member this.Equals(other) = this.Value = other.Value
    override this.Equals(other) =
        match other with
        | :? Expr<'t, 'e> as other -> this.Equals(other)
        | _ -> false
    override this.GetHashCode() = this.Value.GetHashCode()
    interface IEquatable<Expr<'t, 'e>> with
        member this.Equals(other) = this.Equals(other)

and InExpr<'t, 'e> =
    {   Invert : bool
        Input : Expr<'t, 'e>
        Set : InSet<'t, 'e> WithSource
    }

and CollationExpr<'t, 'e> =
    {   Input : Expr<'t, 'e>
        Collation : Name
    }

and BinaryExpr<'t, 'e> =
    {   Left : Expr<'t, 'e>
        Operator : BinaryOperator
        Right : Expr<'t, 'e>
    }

and UnaryExpr<'t, 'e> =
    {   Operator : UnaryOperator
        Operand : Expr<'t, 'e>
    }

and SimilarityExpr<'t, 'e> =
    {   Invert : bool
        Operator : SimilarityOperator
        Input : Expr<'t, 'e>
        Pattern : Expr<'t, 'e>
        Escape : Expr<'t, 'e> option
    }

and BetweenExpr<'t, 'e> =
    {   Invert : bool
        Input : Expr<'t, 'e>
        Low : Expr<'t, 'e>
        High : Expr<'t, 'e>
    }

and CastExpr<'t, 'e> =
    {   Expression : Expr<'t, 'e>
        AsType : TypeName
    }
 
and TableInvocation<'t, 'e> =
    {   Table : ObjectName<'t>
        Arguments : Expr<'t, 'e> array option // we use an option to distinguish between schema.table and schema.table()
    }

and FunctionInvocationExpr<'t, 'e> =
    {   FunctionName : Name
        Arguments : FunctionArguments<'t, 'e>
    }

and CaseExpr<'t, 'e> =
    {   Input : Expr<'t, 'e> option
        Cases : (Expr<'t, 'e> * Expr<'t, 'e>) array
        Else : Expr<'t, 'e> option WithSource
    }

and Distinct = | Distinct

and DistinctColumns =
    | DistinctColumns
    | AllColumns

and FunctionArguments<'t, 'e> =
    | ArgumentWildcard
    | ArgumentList of (Distinct option * Expr<'t, 'e> array)

and InSet<'t, 'e> =
    | InExpressions of Expr<'t, 'e> array
    | InSelect of SelectStmt<'t, 'e>
    | InTable of TableInvocation<'t, 'e>
    | InParameter of BindParameter

and
    [<NoComparison>]
    [<CustomEquality>]
    SelectStmtCore<'t, 'e> =
    {   With : WithClause<'t, 'e> option
        Compound : CompoundExpr<'t, 'e>
        OrderBy : OrderingTerm<'t, 'e> array option
        Limit : Limit<'t, 'e> option
        Info : 't
    }
    member this.Equals(other) =
        this.With = other.With
        && this.Compound = other.Compound
        && this.OrderBy = other.OrderBy
        && this.Limit = other.Limit
    override this.Equals(other) =
        match other with
        | :? SelectStmtCore<'t, 'e> as other -> this.Equals(other)
        | _ -> false
    override this.GetHashCode() =
        this.With
        +@+ this.Compound
        +@+ this.OrderBy
        +@+ this.Limit
    interface IEquatable<SelectStmtCore<'t, 'e>> with
        member this.Equals(other) = this.Equals(other)


and SelectStmt<'t, 'e> = SelectStmtCore<'t, 'e> WithSource

and WithClause<'t, 'e> =
    {   Recursive : bool
        Tables : CommonTableExpression<'t, 'e> array
    }

and
    [<NoComparison>]
    [<CustomEquality>]
    CommonTableExpression<'t, 'e> =
    {   Name : Name
        ColumnNames : Name WithSource array WithSource option
        AsSelect : SelectStmt<'t, 'e>
        Info : 't
    }
    member this.Equals(other) =
        this.Name = other.Name
        && this.ColumnNames = other.ColumnNames
        && this.AsSelect = other.AsSelect
    override this.Equals(other) =
        match other with
        | :? CommonTableExpression<'t, 'e> as other -> this.Equals(other)
        | _ -> false
    override this.GetHashCode() =
        this.Name
        +@+ this.ColumnNames
        +@+ this.AsSelect
    interface IEquatable<CommonTableExpression<'t, 'e>> with
        member this.Equals(other) = this.Equals(other)

and OrderDirection =
    | Ascending
    | Descending

and OrderingTerm<'t, 'e> =
    {   By : Expr<'t, 'e>
        Direction : OrderDirection
    }

and Limit<'t, 'e> =
    {   Limit : Expr<'t, 'e>
        Offset : Expr<'t, 'e> option
    }

and CompoundExprCore<'t, 'e> =
    | CompoundTerm of CompoundTerm<'t, 'e>
    | Union of CompoundExpr<'t, 'e> * CompoundTerm<'t, 'e>
    | UnionAll of CompoundExpr<'t, 'e> * CompoundTerm<'t, 'e>
    | Intersect of CompoundExpr<'t, 'e> * CompoundTerm<'t, 'e>
    | Except of CompoundExpr<'t, 'e> * CompoundTerm<'t, 'e>
    member this.Info =
        match this with
        | CompoundTerm term -> term.Info
        | Union (ex, _)
        | UnionAll (ex, _)
        | Intersect (ex, _)
        | Except (ex, _) -> ex.Value.Info

and CompoundExpr<'t, 'e> = CompoundExprCore<'t, 'e> WithSource

and CompoundTermCore<'t, 'e> =
    | Values of Expr<'t, 'e> array WithSource array
    | Select of SelectCore<'t, 'e>

and
    [<NoComparison>]
    [<CustomEquality>]
    CompoundTerm<'t, 'e> =
    {   Value : CompoundTermCore<'t, 'e>
        Source : SourceInfo
        Info : 't
    }
    member this.Equals(other) = other.Value = this.Value
    override this.Equals(other) =
        match other with
        | :? CompoundTerm<'t, 'e> as other -> this.Equals(other)
        | _ -> false
    override this.GetHashCode() = this.Value.GetHashCode()
    interface IEquatable<CompoundTerm<'t, 'e>> with
        member this.Equals(other) = this.Equals(other)

and
    [<NoComparison>]
    [<CustomEquality>]
    SelectCore<'t, 'e> =
    {   Columns : ResultColumns<'t, 'e>
        From : TableExpr<'t, 'e> option
        Where : Expr<'t, 'e> option
        GroupBy : GroupBy<'t, 'e> option
        Info : 't
    }
    member this.Equals(other) =
        this.Columns = other.Columns
        && this.From = other.From
        && this.Where = other.Where
        && this.GroupBy = other.GroupBy
    override this.Equals(other) =
        match other with
        | :? SelectCore<'t, 'e> as other -> this.Equals(other)
        | _ -> false
    override this.GetHashCode() =
        this.Columns
        +@+ this.From
        +@+ this.Where
        +@+ this.GroupBy
    interface IEquatable<SelectCore<'t, 'e>> with
        member this.Equals(other) = this.Equals(other)


and GroupBy<'t, 'e> =
    {   By : Expr<'t, 'e> array
        Having : Expr<'t, 'e> option
    }

and ResultColumns<'t, 'e> =
    {   Distinct : DistinctColumns option
        Columns : ResultColumn<'t, 'e> array
    }

and ResultColumnNavCardinality =
    | NavOne
    | NavMany
    member this.Separator =
        match this with
        | NavOne -> "$"
        | NavMany -> "*$"

and ResultColumnNav<'t, 'e> =
    {   Cardinality : ResultColumnNavCardinality
        Name : Name
        Columns : ResultColumn<'t, 'e> array
    }

and ResultColumnCase<'t, 'e> =
    | ColumnsWildcard
    | TableColumnsWildcard of Name
    | Column of Expr<'t, 'e> * Alias
    | ColumnNav of ResultColumnNav<'t, 'e>
    member this.AssumeColumn() =
        match this with
        | Column (expr, alias) -> expr, alias
        | _ -> failwith "BUG: wildcard was assumed to be a single column (should've been expanded by now)"

and ResultColumn<'t, 'e> =
    {   Case : ResultColumnCase<'t, 'e>
        Source : SourceInfo
    }

and IndexHint =
    | IndexedBy of Name
    | NotIndexed

and QualifiedTableName<'t> =
    {   TableName : ObjectName<'t>
        IndexHint : IndexHint option
    }

and TableOrSubqueryType<'t, 'e> =
    | Table of TableInvocation<'t, 'e> * IndexHint option // note: an index hint is invalid if the table has args
    | Subquery of SelectStmt<'t, 'e>

and
    [<NoComparison>]
    [<CustomEquality>]
    TableOrSubquery<'t, 'e> =
    {   Table : TableOrSubqueryType<'t, 'e>
        Alias : Name option
        Info : 't
    }
    member this.Equals(other) =
        this.Table = other.Table
        && this.Alias = other.Alias
    override this.Equals(other) =
        match other with
        | :? TableOrSubquery<'t, 'e> as other -> this.Equals(other)
        | _ -> false
    override this.GetHashCode() = this.Table +@+ this.Alias
    interface IEquatable<TableOrSubquery<'t, 'e>> with
        member this.Equals(other) = this.Equals(other)

and JoinType =
    | Inner
    | LeftOuter
    | Cross
    | Natural of JoinType
    member this.IsOuter = this = LeftOuter

and JoinConstraint<'t, 'e> =
    | JoinOn of Expr<'t, 'e>
    | JoinUnconstrained

and Join<'t, 'e> =
    {   JoinType : JoinType
        LeftTable : TableExpr<'t, 'e>
        RightTable : TableExpr<'t, 'e>
        Constraint : JoinConstraint<'t, 'e>
    }

and TableExprCore<'t, 'e> =
    | TableOrSubquery of TableOrSubquery<'t, 'e>
    | Join of Join<'t, 'e>

and TableExpr<'t, 'e> = TableExprCore<'t, 'e> WithSource

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
    {   Deferrable : bool
        InitiallyDeferred : bool option
    }

type ForeignKeyClause<'t> =
    {   ReferencesTable : ObjectName<'t>
        ReferencesColumns : Name WithSource array
        Rules : ForeignKeyRule array
        Defer : ForeignKeyDeferClause option
    }

type PrimaryKeyClause =
    {   Order : OrderDirection
        AutoIncrement : bool
    }

type ColumnConstraintType<'t, 'e> =
    | NullableConstraint
    | PrimaryKeyConstraint of PrimaryKeyClause
    | UniqueConstraint
    | DefaultConstraint of Expr<'t, 'e>
    | CollateConstraint of Name
    | ForeignKeyConstraint of ForeignKeyClause<'t>
    member this.DefaultName(columnName : Name) =
        match this with
        | NullableConstraint -> columnName + "__NULL"
        | PrimaryKeyConstraint _ -> columnName + "__PK"
        | UniqueConstraint -> columnName + "__UNIQUE"
        | DefaultConstraint _ -> columnName + "__DEFAULT"
        | CollateConstraint _ -> columnName + "__COLLATION"
        | ForeignKeyConstraint fk ->
            columnName
            + "__FK__"
            + fk.ReferencesTable.ObjectName.Value
            + "__"
            + String.concat "_" [ for c in fk.ReferencesColumns -> c.Value.Value ]

type ColumnConstraint<'t, 'e> =
    {   Name : Name
        ColumnConstraintType : ColumnConstraintType<'t, 'e>
    }

type ColumnDef<'t, 'e> =
    {   Name : Name
        Type : TypeName
        Constraints : ColumnConstraint<'t, 'e> array
    }
    member this.Nullable =
        this.Constraints
        |> Array.exists (function | { ColumnConstraintType = NullableConstraint } -> true | _ -> false)

type AlterTableAlteration<'t, 'e> =
    | RenameTo of Name
    | AddColumn of ColumnDef<'t, 'e>

type AlterTableStmt<'t, 'e> =
    {   Table : ObjectName<'t>
        Alteration : AlterTableAlteration<'t, 'e>
    }

type TableIndexConstraintType =
    | PrimaryKey
    | Unique

type TableIndexConstraintClause<'t, 'e> =
    {   Type : TableIndexConstraintType
        IndexedColumns : (Name * OrderDirection) array
    }

type TableConstraintType<'t, 'e> =
    | TableIndexConstraint of TableIndexConstraintClause<'t, 'e>
    | TableForeignKeyConstraint of Name WithSource array * ForeignKeyClause<'t>
    | TableCheckConstraint of Expr<'t, 'e>
    member this.DefaultName() =
        match this with
        | TableIndexConstraint con ->
            String.concat "_" [ for name, _ in con.IndexedColumns -> name.Value ]
            + "__"
            + (match con.Type with
                | PrimaryKey -> "PK"
                | Unique -> "UNIQUE")
        | TableForeignKeyConstraint (names, fk) ->
            String.concat "_" [ for name in names -> name.Value.Value ]
            + "__FK__"
            + fk.ReferencesTable.ObjectName.Value
            + "__"
            + String.concat "_" [ for c in fk.ReferencesColumns -> c.Value.Value ]
        | TableCheckConstraint _ -> "CHECK"   

type TableConstraint<'t, 'e> =
    {   Name : Name
        TableConstraintType : TableConstraintType<'t, 'e>
    }

type CreateTableDefinition<'t, 'e> =
    {   Columns : ColumnDef<'t, 'e> array
        Constraints : TableConstraint<'t, 'e> array
        WithoutRowId : bool
    }
    member this.AllConstraints() =
        seq {
            for column in this.Columns do
                for constr in column.Constraints ->
                    constr.Name, Set.singleton column.Name
            for constr in this.Constraints ->
                constr.Name,
                    match constr.TableConstraintType with
                    | TableIndexConstraint constr -> constr.IndexedColumns |> Seq.map fst |> Set.ofSeq
                    | TableForeignKeyConstraint (names, _) -> names |> Seq.map (fun v -> v.Value) |> Set.ofSeq
                    | TableCheckConstraint _ -> Set.empty
        }

type CreateTableAs<'t, 'e> =
    | CreateAsDefinition of CreateTableDefinition<'t, 'e>
    | CreateAsSelect of SelectStmt<'t, 'e>

type CreateTableStmt<'t, 'e> =
    {   Temporary : bool
        Name : ObjectName<'t>
        As : CreateTableAs<'t, 'e>
    }

type CreateIndexStmt<'t, 'e> =
    {   Unique : bool
        IndexName : ObjectName<'t>
        TableName : ObjectName<'t>
        IndexedColumns : (Name * OrderDirection) array
        Where : Expr<'t, 'e> option
    }

type DeleteStmt<'t, 'e> =
    {   With : WithClause<'t, 'e> option
        DeleteFrom : QualifiedTableName<'t>
        Where : Expr<'t, 'e> option
        OrderBy : OrderingTerm<'t, 'e> array option
        Limit : Limit<'t, 'e> option
    }

type UpdateOr =
    | UpdateOrRollback
    | UpdateOrAbort
    | UpdateOrReplace
    | UpdateOrFail
    | UpdateOrIgnore

type UpdateStmt<'t, 'e> =
    {   With : WithClause<'t, 'e> option
        UpdateTable : QualifiedTableName<'t>
        Or : UpdateOr option
        Set : (Name WithSource * Expr<'t, 'e>) array
        Where : Expr<'t, 'e> option
        OrderBy : OrderingTerm<'t, 'e> array option
        Limit : Limit<'t, 'e> option
    }

type InsertOr =
    | InsertOrRollback
    | InsertOrAbort
    | InsertOrReplace
    | InsertOrFail
    | InsertOrIgnore

type InsertStmt<'t, 'e> =
    {   With : WithClause<'t, 'e> option
        Or : InsertOr option
        InsertInto : ObjectName<'t>
        Columns : Name WithSource array option
        Data : SelectStmt<'t, 'e> option // either select/values, or "default values" if none
    }

type CreateViewStmt<'t, 'e> =
    {   Temporary : bool
        ViewName : ObjectName<'t>
        ColumnNames : Name WithSource array option
        AsSelect : SelectStmt<'t, 'e>
    }

type DropObjectType =
    | DropIndex
    | DropTable
    | DropView

type DropObjectStmt<'t> =
    {   Drop : DropObjectType
        ObjectName : ObjectName<'t>
    }

type VendorStmtFragment<'t, 'e> =
    | VendorEmbeddedExpr of Expr<'t, 'e>
    | VendorRaw of string

type VendorStmt<'t, 'e> =
    {   VendorName : Name WithSource
        Fragments : VendorStmtFragment<'t, 'e> array
        ImaginaryStmts : Stmt<'t, 'e> array option
    }

and Stmt<'t, 'e> =
    | AlterTableStmt of AlterTableStmt<'t, 'e>
    | CreateIndexStmt of CreateIndexStmt<'t, 'e>
    | CreateTableStmt of CreateTableStmt<'t, 'e>
    | CreateViewStmt of CreateViewStmt<'t, 'e>
    | DeleteStmt of DeleteStmt<'t, 'e>
    | DropObjectStmt of DropObjectStmt<'t>
    | InsertStmt of InsertStmt<'t, 'e>
    | SelectStmt of SelectStmt<'t, 'e>
    | UpdateStmt of UpdateStmt<'t, 'e>
    | BeginStmt
    | CommitStmt
    | RollbackStmt

type TotalStmt<'t, 'e> =
    | CoreStmt of Stmt<'t, 'e>
    | VendorStmt of VendorStmt<'t, 'e>
    member this.CoreStmts() =
        match this with
        | CoreStmt stmt -> Seq.singleton stmt
        | VendorStmt { ImaginaryStmts = None } -> Seq.empty
        | VendorStmt { ImaginaryStmts = Some stmts } -> stmts :> _ seq
    member this.SelectStmts() =
        this.CoreStmts()
        |> Seq.choose (function | SelectStmt s -> Some s | _ -> None)

type ExprType = ExprType<unit, unit>
type Expr = Expr<unit, unit>
type InExpr = InExpr<unit, unit>
type CollationExpr = CollationExpr<unit, unit>
type BetweenExpr = BetweenExpr<unit, unit>
type SimilarityExpr = SimilarityExpr<unit, unit>
type BinaryExpr = BinaryExpr<unit, unit>
type UnaryExpr = UnaryExpr<unit, unit>
type ObjectName = ObjectName<unit>
type ColumnName = ColumnName<unit>
type InSet = InSet<unit, unit>
type CaseExpr = CaseExpr<unit, unit>
type CastExpr = CastExpr<unit, unit>
type FunctionArguments = FunctionArguments<unit, unit>
type FunctionInvocationExpr = FunctionInvocationExpr<unit, unit>
    
type WithClause = WithClause<unit, unit>
type CommonTableExpression = CommonTableExpression<unit, unit>
type CompoundExprCore = CompoundExprCore<unit, unit>
type CompoundExpr = CompoundExpr<unit, unit>
type CompoundTermCore = CompoundTermCore<unit, unit>
type CompoundTerm = CompoundTerm<unit, unit>
type CreateTableDefinition = CreateTableDefinition<unit, unit>
type CreateTableStmt = CreateTableStmt<unit, unit>
type SelectCore = SelectCore<unit, unit>
type Join = Join<unit, unit>
type JoinConstraint = JoinConstraint<unit, unit>
type GroupBy = GroupBy<unit, unit>
type Limit = Limit<unit, unit>
type OrderingTerm = OrderingTerm<unit, unit>
type ResultColumnCase = ResultColumnCase<unit, unit>
type ResultColumn = ResultColumn<unit, unit>
type ResultColumns = ResultColumns<unit, unit>
type TableOrSubquery = TableOrSubquery<unit, unit>
type TableExprCore = TableExprCore<unit, unit>
type TableExpr = TableExpr<unit, unit>
type TableInvocation = TableInvocation<unit, unit>
type SelectStmt = SelectStmt<unit, unit>
type ColumnConstraint = ColumnConstraint<unit, unit>
type ColumnDef = ColumnDef<unit, unit>
type AlterTableStmt = AlterTableStmt<unit, unit>
type AlterTableAlteration = AlterTableAlteration<unit, unit>
type CreateIndexStmt = CreateIndexStmt<unit, unit>
type TableIndexConstraintClause = TableIndexConstraintClause<unit, unit>
type TableConstraint = TableConstraint<unit, unit>
type CreateViewStmt = CreateViewStmt<unit, unit>
type QualifiedTableName = QualifiedTableName<unit>
type DeleteStmt = DeleteStmt<unit, unit>
type DropObjectStmt = DropObjectStmt<unit>
type UpdateStmt = UpdateStmt<unit, unit>
type InsertStmt = InsertStmt<unit, unit>
type VendorStmt = VendorStmt<unit, unit>
type Stmt = Stmt<unit, unit>
type TotalStmt = TotalStmt<unit, unit>
type TotalStmts = TotalStmt IReadOnlyList
