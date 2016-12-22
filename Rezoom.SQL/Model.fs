namespace Rezoom.SQL
open System
open System.Data
open System.Data.Common
open System.Collections.Generic

type CoreColumnType =
    | BooleanType
    | StringType
    | IntegerType of IntegerSize
    | FloatType of FloatSize
    | DecimalType
    | BinaryType
    | DateTimeType
    | DateTimeOffsetType
    | StringishTypeClass
    | NumericTypeClass
    | IntegralTypeClass
    | FractionalTypeClass
    | AnyTypeClass
    member this.ParentType =
        match this with
        | IntegerType Integer8 -> IntegralTypeClass
        | IntegerType Integer16 -> IntegerType Integer8
        | IntegerType Integer32 -> IntegerType Integer16
        | IntegerType Integer64 -> IntegerType Integer32
        | FloatType Float32 -> FractionalTypeClass
        | FloatType Float64 -> FloatType Float32
        | DecimalType -> FractionalTypeClass
        | StringType
        | BinaryType -> StringishTypeClass
        | IntegralTypeClass
        | FractionalTypeClass -> NumericTypeClass
        | BooleanType
        | DateTimeType
        | DateTimeOffsetType
        | NumericTypeClass
        | StringishTypeClass
        | AnyTypeClass -> AnyTypeClass
    member this.HasAncestor(candidate) =
        if this = candidate then true
        elif this.ParentType = this then false
        else this.ParentType.HasAncestor(candidate)
    member left.Unify(right) =
        if left.HasAncestor(right) then
            Ok left
        elif right.HasAncestor(left) then
            Ok right
        else
            Error <| sprintf "The types %O and %O cannot be unified" left right
    override this.ToString() =
        match this with
        | BooleanType -> "BOOL"
        | StringType -> "STRING"
        | IntegerType Integer8 -> "INT8"
        | IntegerType Integer16 -> "INT16"
        | IntegerType Integer32 -> "INT"
        | IntegerType Integer64 -> "INT64"
        | FloatType Float32 -> "FLOAT32"
        | FloatType Float64 -> "FLOAT64"
        | DecimalType -> "DECIMAL"
        | BinaryType -> "BINARY"
        | DateTimeType -> "DATETIME"
        | DateTimeOffsetType -> "DATETIMEOFFSET"
        | FractionalTypeClass -> "<fractional>"
        | IntegralTypeClass -> "<integral>"
        | NumericTypeClass -> "<numeric>"
        | StringishTypeClass -> "<stringish>"
        | AnyTypeClass -> "<any>"
    static member OfTypeName(typeName : TypeName) =
        match typeName with
        | StringTypeName _ -> StringType
        | BinaryTypeName _ -> BinaryType
        | IntegerTypeName sz -> IntegerType sz
        | FloatTypeName sz -> FloatType sz
        | DecimalTypeName -> DecimalType
        | BooleanTypeName -> BooleanType
        | DateTimeTypeName -> DateTimeType
        | DateTimeOffsetTypeName -> DateTimeOffsetType

type ColumnType =
    {   Type : CoreColumnType
        Nullable : bool
    }
    static member OfTypeName(typeName : TypeName, nullable) =
        {   Type = CoreColumnType.OfTypeName(typeName)
            Nullable = nullable
        }
    member inline private ty.TypeInfo =
        match ty.Type with
        | IntegerType Integer8 -> DbType.SByte, if ty.Nullable then typeof<Nullable<sbyte>> else typeof<sbyte>
        | IntegerType Integer16 -> DbType.Int16, if ty.Nullable then typeof<Nullable<int16>> else typeof<int16>
        | IntegralTypeClass
        | IntegerType Integer32 -> DbType.Int32, if ty.Nullable then typeof<Nullable<int32>> else typeof<int32>
        | IntegerType Integer64 -> DbType.Int64, if ty.Nullable then typeof<Nullable<int64>> else typeof<int64>
        | FloatType Float32 -> DbType.Single, if ty.Nullable then typeof<Nullable<single>> else typeof<single>
        | FloatType Float64 -> DbType.Double, if ty.Nullable then typeof<Nullable<double>> else typeof<double>
        | BooleanType -> DbType.Boolean, if ty.Nullable then typeof<Nullable<bool>> else typeof<bool>
        | FractionalTypeClass
        | NumericTypeClass
        | DecimalType -> DbType.Decimal, if ty.Nullable then typeof<Nullable<decimal>> else typeof<decimal>
        | DateTimeType ->
            DbType.DateTime, if ty.Nullable then typeof<Nullable<DateTime>> else typeof<DateTime>
        | DateTimeOffsetType ->
            DbType.DateTimeOffset, if ty.Nullable then typeof<Nullable<DateTimeOffset>> else typeof<DateTimeOffset>
        | StringType -> DbType.String, typeof<string>
        | BinaryType -> DbType.Binary, typeof<byte array>
        | StringishTypeClass
        | AnyTypeClass -> DbType.Object, typeof<obj>
    member ty.CLRType = snd ty.TypeInfo
    member ty.DbType = fst ty.TypeInfo

type ArgumentType =
    | ArgumentConcrete of ColumnType
    | ArgumentTypeVariable of name : Name * constrained : CoreColumnType option

type VariableArgument =
    {   /// Maximum # of times this argument can be supplied (no limit if None).
        MaxCount : int option
        Type : ArgumentType
    }

type AggregateType =
    {   AllowWildcard : bool
        AllowDistinct : bool
    }

type FunctionType =
    {   FunctionName : Name
        FixedArguments : ArgumentType IReadOnlyList
        VariableArgument : VariableArgument option
        Output : ArgumentType
        Aggregate : FunctionArguments<unit, unit> -> AggregateType option
        Idempotent : bool
    }

type DatabaseBuiltin =
    {   Functions : Map<Name, FunctionType>
    }

type Model =
    {   Schemas : Map<Name, Schema>
        DefaultSchema : Name
        TemporarySchema : Name
        Builtin : DatabaseBuiltin
    }
    member this.Schema(name : Name option) =
        this.Schemas |> Map.tryFind (name |? this.DefaultSchema)

and Schema =
    {   SchemaName : Name
        Objects : Map<Name, SchemaObject>
    }
    static member Empty(name) =
        {   SchemaName = name
            Objects = Map.empty
        }
    member this.ContainsObject(name : Name) = this.Objects.ContainsKey(name)

and SchemaObject =
    | SchemaTable of SchemaTable
    | SchemaView of SchemaView

and SchemaIndex =
    {   SchemaName : Name
        TableName : Name
        IndexName : Name
        Columns : Name Set
    }

and SchemaConstraint =
    {   SchemaName : Name
        TableName : Name
        ConstraintName : Name
        Columns : Name Set
    }

and SchemaTable =
    {   SchemaName : Name
        TableName : Name
        Columns : Map<Name, SchemaColumn>
        Indexes : Map<Name, SchemaIndex>
        Constraints : Map<Name, SchemaConstraint>
    }
    member this.WithAdditionalColumn(col : ColumnDef<_, _>) =
        match this.Columns |> Map.tryFind col.Name with
        | Some _ -> Error <| sprintf "Column ``%O`` already exists" col.Name
        | None ->
            let hasNotNullConstraint =
                col.Constraints
                |> Seq.exists(
                    function | { ColumnConstraintType = NotNullConstraint _ } -> true | _ -> false)
            let isPrimaryKey =
                col.Constraints
                |> Seq.exists(
                    function | { ColumnConstraintType = PrimaryKeyConstraint _ } -> true | _ -> false)
            let newCol =
                {   SchemaName = this.SchemaName
                    TableName = this.TableName
                    PrimaryKey = isPrimaryKey
                    ColumnName = col.Name
                    ColumnType = ColumnType.OfTypeName(col.Type, not hasNotNullConstraint)
                }
            Ok { this with Columns = this.Columns |> Map.add newCol.ColumnName newCol }
    static member OfCreateDefinition(schemaName, tableName, def : CreateTableDefinition<_, _>) =
        let tablePkColumns =
            seq {
                for constr in def.Constraints do
                    match constr.TableConstraintType with
                    | TableIndexConstraint { Type = PrimaryKey; IndexedColumns = indexed } ->
                        for name, _ in indexed -> name
                    | _ -> ()
            } |> Set.ofSeq
        let tableColumns =
            seq {
                for column in def.Columns ->
                    let hasNotNullConstraint =
                        column.Constraints
                        |> Seq.exists(function | { ColumnConstraintType = NotNullConstraint _ } -> true | _ -> false)
                    let isPrimaryKey =
                        tablePkColumns.Contains(column.Name)
                        || column.Constraints |> Seq.exists(function
                            | { ColumnConstraintType = PrimaryKeyConstraint _ } -> true
                            | _ -> false)
                    {   SchemaName = schemaName
                        TableName = tableName
                        PrimaryKey = isPrimaryKey
                        ColumnName = column.Name
                        ColumnType = ColumnType.OfTypeName(column.Type, not hasNotNullConstraint)
                    }
            }
        {   SchemaName = schemaName
            TableName = tableName
            Columns = tableColumns |> mapBy (fun c -> c.ColumnName)
            Indexes = Map.empty
            Constraints =
                seq {
                    for constr, names in def.AllConstraints() ->
                        constr,
                            {   SchemaName = schemaName
                                TableName = tableName
                                ConstraintName = constr
                                Columns = names
                            }
                } |> Map.ofSeq
        }

and SchemaColumn =
    {   SchemaName : Name
        TableName : Name
        ColumnName : Name
        /// True if this column is part of the table's primary key.
        PrimaryKey : bool
        ColumnType : ColumnType
    }

and SchemaView =
    {   SchemaName : Name
        ViewName : Name
        Definition : TSelectStmt
    }

and ExprInfo<'t> =
    {   /// The inferred type of this expression.
        Type : 't
        /// Does this expression return the same value each time it's run?
        Idempotent : bool
        /// If this expression is a function call, the function that it calls.
        Function : FunctionType option
        /// If this expression accesses a column of a table in the schema, the column's information.
        Column : SchemaColumn option
    }
    member this.PrimaryKey =
        match this.Column with
        | None -> false
        | Some c -> c.PrimaryKey
    static member OfType(t : 't) =
        {   Type = t
            Idempotent = true
            Function = None
            Column = None
        }
    member this.Map(f : 't -> _) =
        {   Type = f this.Type
            Idempotent = this.Idempotent
            Function = this.Function
            Column = this.Column
        }

and ColumnExprInfo<'t> =
    {   Expr : Expr<'t ObjectInfo, 't ExprInfo>
        FromAlias : Name option // table alias this was selected from, if any
        ColumnName : Name
    }
    member this.Map(f : 't -> _) =
        {   Expr =
                let mapping = ASTMapping<'t ObjectInfo, 't ExprInfo, _, _>((fun t -> t.Map(f)), fun e -> e.Map(f))
                mapping.Expr(this.Expr)
            FromAlias = this.FromAlias
            ColumnName = this.ColumnName
        }

and QueryExprInfo<'t> =
    { Columns : 't ColumnExprInfo IReadOnlyList }
    member this.Idempotent =
        this.Columns |> Seq.forall (fun e -> e.Expr.Info.Idempotent)
    member this.ColumnsWithNames(names) =
        let mine = this.Columns |> toDictionary (fun c -> c.ColumnName)
        let filtered =
            seq {
                for { WithSource.Source = source; Value = name } in names do
                    let succ, found = mine.TryGetValue(name)
                    if succ then yield found
                    else failAt source <| sprintf "No such column: ``%O``" name
            } |> toReadOnlyList
        { Columns = filtered }
    member this.ColumnByName(name) =
        let matches =
            this.Columns
            |> Seq.filter (fun c -> c.ColumnName = name)
            |> Seq.truncate 2
            |> Seq.toList
        match matches with
        | [] -> NotFound <| sprintf "No such column: ``%O``" name
        | [ single ] -> Found single
        | { FromAlias = Some a1 } :: { FromAlias = Some a2 } :: _ when a1 <> a2 ->
            Ambiguous <|
                sprintf "Ambiguous columm: ``%O`` (may refer to %O.%O or %O.%O)"
                    name a1 name a2 name
        | _ -> Ambiguous <| sprintf "Ambigous column: ``%O``" name
    member this.RenameColumns(names : Name IReadOnlyList) =
        if names.Count <> this.Columns.Count then
            Error <| sprintf "%d columns named for a query with %d columns" names.Count this.Columns.Count
        else
            let newColumns =
                (this.Columns, names)
                ||> Seq.map2 (fun col newName -> { col with ColumnName = newName })
                |> toReadOnlyList
            Ok { Columns = newColumns }
    member this.Append(right : 't QueryExprInfo) =
        { Columns = appendLists this.Columns right.Columns }
    member this.Map(f : 't -> _) =
        { Columns = this.Columns |> Seq.map (fun c -> c.Map(f)) |> toReadOnlyList }

and TableReference =
    | TableReference of SchemaTable
    | ViewReference of SchemaView
    | CTEReference of Name
    | FromClauseReference of Name
    | SelectClauseReference of Name
    | SelectResults
    | CompoundTermResults

and TableLikeExprInfo<'t> =
    {   Table : TableReference
        Query : QueryExprInfo<'t>
    }
    member this.Map(f : 't -> _) =
        {   Table = this.Table
            Query = this.Query.Map(f)
        }

and ObjectInfo<'t> =
    | TableLike of 't TableLikeExprInfo
    | Index of SchemaIndex
    | Missing
    member this.Idempotent =
        match this with
        | TableLike t -> t.Query.Idempotent
        | Index _
        | Missing -> true
    member this.Table =
        match this with
        | TableLike t -> t
        | other -> failwithf "Expected table, but found reference to %A" other
    member this.Query = this.Table.Query
    member this.Columns = this.Query.Columns
    member this.Map<'t1>(f : 't -> 't1) : ObjectInfo<'t1> =
        match this with
        | TableLike t -> TableLike (t.Map(f))
        | Index i -> Index i
        | Missing -> Missing

and TSelectStmt = SelectStmt<ColumnType ObjectInfo, ColumnType ExprInfo>

type TExprType = ExprType<ColumnType ObjectInfo, ColumnType ExprInfo>
type TExpr = Expr<ColumnType ObjectInfo, ColumnType ExprInfo>
type TInExpr = InExpr<ColumnType ObjectInfo, ColumnType ExprInfo>
type TCollationExpr = CollationExpr<ColumnType ObjectInfo, ColumnType ExprInfo>
type TBetweenExpr = BetweenExpr<ColumnType ObjectInfo, ColumnType ExprInfo>
type TSimilarityExpr = SimilarityExpr<ColumnType ObjectInfo, ColumnType ExprInfo>
type TBinaryExpr = BinaryExpr<ColumnType ObjectInfo, ColumnType ExprInfo>
type TUnaryExpr = UnaryExpr<ColumnType ObjectInfo, ColumnType ExprInfo>
type TObjectName = ObjectName<ColumnType ObjectInfo>
type TColumnName = ColumnName<ColumnType ObjectInfo>
type TInSet = InSet<ColumnType ObjectInfo, ColumnType ExprInfo>
type TCaseExpr = CaseExpr<ColumnType ObjectInfo, ColumnType ExprInfo>
type TCastExpr = CastExpr<ColumnType ObjectInfo, ColumnType ExprInfo>
type TFunctionInvocationExpr = FunctionInvocationExpr<ColumnType ObjectInfo, ColumnType ExprInfo>
    
type TWithClause = WithClause<ColumnType ObjectInfo, ColumnType ExprInfo>
type TCommonTableExpression = CommonTableExpression<ColumnType ObjectInfo, ColumnType ExprInfo>
type TCompoundExprCore = CompoundExprCore<ColumnType ObjectInfo, ColumnType ExprInfo>
type TCompoundExpr = CompoundExpr<ColumnType ObjectInfo, ColumnType ExprInfo>
type TCompoundTermCore = CompoundTermCore<ColumnType ObjectInfo, ColumnType ExprInfo>
type TCompoundTerm = CompoundTerm<ColumnType ObjectInfo, ColumnType ExprInfo>
type TForeignKeyClause = ForeignKeyClause<ColumnType ObjectInfo>
type TCreateTableDefinition = CreateTableDefinition<ColumnType ObjectInfo, ColumnType ExprInfo>
type TCreateTableStmt = CreateTableStmt<ColumnType ObjectInfo, ColumnType ExprInfo>
type TSelectCore = SelectCore<ColumnType ObjectInfo, ColumnType ExprInfo>
type TJoinConstraint = JoinConstraint<ColumnType ObjectInfo, ColumnType ExprInfo>
type TJoin = Join<ColumnType ObjectInfo, ColumnType ExprInfo>
type TLimit = Limit<ColumnType ObjectInfo, ColumnType ExprInfo>
type TGroupBy = GroupBy<ColumnType ObjectInfo, ColumnType ExprInfo>
type TOrderingTerm = OrderingTerm<ColumnType ObjectInfo, ColumnType ExprInfo>
type TResultColumn = ResultColumn<ColumnType ObjectInfo, ColumnType ExprInfo>
type TResultColumns = ResultColumns<ColumnType ObjectInfo, ColumnType ExprInfo>
type TTableOrSubquery = TableOrSubquery<ColumnType ObjectInfo, ColumnType ExprInfo>
type TTableExprCore = TableExprCore<ColumnType ObjectInfo, ColumnType ExprInfo>
type TTableExpr = TableExpr<ColumnType ObjectInfo, ColumnType ExprInfo>
type TTableInvocation = TableInvocation<ColumnType ObjectInfo, ColumnType ExprInfo>

type TColumnConstraint = ColumnConstraint<ColumnType ObjectInfo, ColumnType ExprInfo>
type TColumnDef = ColumnDef<ColumnType ObjectInfo, ColumnType ExprInfo>
type TAlterTableStmt = AlterTableStmt<ColumnType ObjectInfo, ColumnType ExprInfo>
type TAlterTableAlteration = AlterTableAlteration<ColumnType ObjectInfo, ColumnType ExprInfo>
type TCreateIndexStmt = CreateIndexStmt<ColumnType ObjectInfo, ColumnType ExprInfo>
type TTableIndexConstraintClause = TableIndexConstraintClause<ColumnType ObjectInfo, ColumnType ExprInfo>
type TTableConstraint = TableConstraint<ColumnType ObjectInfo, ColumnType ExprInfo>
type TCreateViewStmt = CreateViewStmt<ColumnType ObjectInfo, ColumnType ExprInfo>
type TQualifiedTableName = QualifiedTableName<ColumnType ObjectInfo>
type TDeleteStmt = DeleteStmt<ColumnType ObjectInfo, ColumnType ExprInfo>
type TDropObjectStmt = DropObjectStmt<ColumnType ObjectInfo>
type TUpdateStmt = UpdateStmt<ColumnType ObjectInfo, ColumnType ExprInfo>
type TInsertStmt = InsertStmt<ColumnType ObjectInfo, ColumnType ExprInfo>
type TStmt = Stmt<ColumnType ObjectInfo, ColumnType ExprInfo>
type TVendorStmt = VendorStmt<ColumnType ObjectInfo, ColumnType ExprInfo>
type TTotalStmt = TotalStmt<ColumnType ObjectInfo, ColumnType ExprInfo>
type TTotalStmts = TTotalStmt IReadOnlyList