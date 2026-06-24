namespace Rezoom.SQL.Compiler
open System
open System.Data
open System.Collections.Generic
open Rezoom.SQL.Mapping
open Rezoom.SQL.Migrations
open Rezoom.SQL.Compiler
open FSharp.Quotations

type IParameterIndexer =
    abstract member ParameterIndex : parameter : BindParameter -> int

/// Used to convert parameters coming into a generated command, which are in "nice" types
/// including FSharpOption<T> and user-primitive wrappers, to a raw object that can go into
/// a DbParameter and be understood by the ADO.NET driver at runtime.
[<NoComparison>]
[<NoEquality>]
type ParameterTransform =
    {   ParameterType : XDbType
        ValueTransform : Quotations.Expr -> Quotations.Expr
    }

type IBackend =
    abstract member InitialModel : Model
    abstract member MigrationBackend : Quotations.Expr<ConnectionInfo -> IMigrationBackend>
    abstract member ParameterTransform
        : columnType : ColumnType -> ParameterTransform
    abstract member ToCommandFragments
        : indexer : IParameterIndexer * stmts : TTotalStmts -> CommandFragment IReadOnlyList

[<AbstractClass>]
type BackendBase() =
    abstract member InitialModel : Model
    abstract member MigrationBackend : Quotations.Expr<ConnectionInfo -> IMigrationBackend>
    abstract member ParameterTransform
        : columnType : ColumnType -> ParameterTransform
    abstract member ToCommandFragments
        : indexer : IParameterIndexer * stmts : TTotalStmts -> CommandFragment IReadOnlyList
    abstract member InteriorPrimitiveTransform : builtInColumnType : ColumnType -> ParameterTransform
    abstract member SQLTypeString : TypeName -> string
    abstract member AlwaysUseCustomDbType : bool
    default this.AlwaysUseCustomDbType = false
    default this.InteriorPrimitiveTransform(builtInColumnType) = { ParameterType = StdDbType builtInColumnType.DbType; ValueTransform = fun e -> e }
    default this.ParameterTransform(columnType) = 
        // Null/None -> DBNull, else continue. (For non-user types only; a
        // user-typed parameter does its own Option unwrap inside the runtime
        // converter below.) Surprisingly we don't need this for Nullable<T>: a
        // boxed Nullable<T> is already the same as the boxed form of its value.
        let optionalsToDbNull (expr : Quotations.Expr) (nextStep : Quotations.Expr -> Quotations.Expr) =
            let ty = expr.Type
            let asObj = Expr.Coerce(expr, typeof<obj>)
            if ty.IsConstructedGenericType && ty.GetGenericTypeDefinition() = typedefof<_ option> then
                let invokeValue = Expr.PropertyGet(expr, ty.GetProperty("Value"))
                let nextAsObj = Expr.Coerce(nextStep invokeValue, typeof<obj>)
                <@@ if isNull %%asObj then box DBNull.Value else %%nextAsObj @@>
            else
                let nextAsObj = Expr.Coerce(nextStep expr, typeof<obj>)
                <@@ if isNull %%asObj then box DBNull.Value else %%nextAsObj @@>
        match columnType.Type with
        | UserTypeBasedOn (userTy, underlying) ->
            // Let ToPrimitive (and the Option unwrap) be handled by RuntimeUserConvert.
            // That keeps the generated quotation free of MLC-loaded method refs
            // and Option<MLC UserType> member lookups. The interior transform
            // still runs at design time because it operates on the underlying
            // runtime primitive that comes back out of the converter.
            let underlyingColumn = { Nullable = false; Type = underlying }
            let interior = this.InteriorPrimitiveTransform underlyingColumn
            let underlyingClr = underlyingColumn.CLRType(false)
            let fdExpr = FreezeDry.FreezeDriedUserPrimitiveType.Of(userTy).Quote()
            let finalParamType =
                match userTy.SQLParameterDbType, userTy.RawBackendSQLType with
                | Some (overrideProp, overrideVal), Some overrideType ->
                    CustomDbType
                        {   DbTypePropertyName = overrideProp
                            DbTypeValue = overrideVal
                            SQLTypeName = overrideType
                        }
                | None, Some overrideType ->
                    interior.ParameterType.WithSQLTypeName(overrideType)
                | Some (overrideProp, overrideVal), None ->
                    CustomDbType
                        {   DbTypePropertyName = overrideProp
                            DbTypeValue = overrideVal
                            SQLTypeName = this.SQLTypeString(underlying.ApproximateTypeName())
                        }
                | None, None ->
                    if this.AlwaysUseCustomDbType then
                        interior.ParameterType.WithSQLTypeName(this.SQLTypeString(underlying.ApproximateTypeName()))
                    else
                        interior.ParameterType
            {   ParameterType = finalParamType
                ValueTransform = fun e ->
                    let asObj = Expr.Coerce(e, typeof<obj>)
                    let underlyingObj =
                        <@@ RuntimeUserConvert.toPrimitive
                                (%%fdExpr : FreezeDry.FreezeDriedUserPrimitiveType) (%%asObj : obj) @@>
                    let v = Var("underlying", typeof<obj>)
                    let coerced = Expr.Coerce(Expr.Var v, underlyingClr)
                    let interiorBoxed = Expr.Coerce(interior.ValueTransform coerced, typeof<obj>)
                    Expr.Let(v, underlyingObj,
                        <@@ if isNull (%%Expr.Var v : obj) then box DBNull.Value else %%interiorBoxed @@>)
            }
        | _ ->
            // The fundamental underlying primitive could still be one the backend
            // doesn't *really* support (e.g. SQLite fakes DateTime as a string),
            // so the backend gets to intercept via the interior transform.
            let interior = this.InteriorPrimitiveTransform { columnType with Nullable = false }
            {   ParameterType = interior.ParameterType
                ValueTransform = fun e ->
                    optionalsToDbNull e (fun next -> interior.ValueTransform next)
            }
    interface IBackend with
        member this.InitialModel = this.InitialModel
        member this.MigrationBackend = this.MigrationBackend
        member this.ParameterTransform (columnType : ColumnType) = this.ParameterTransform(columnType)
        member this.ToCommandFragments (indexer: IParameterIndexer, stmts: TTotalStmts) = this.ToCommandFragments(indexer, stmts)
