namespace Rezoom.SQL.Compiler
open System
open System.Data
open System.Data.Common
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
    {   ParameterType : DbType
        ValueTransform : Quotations.Expr -> Quotations.Expr
    }
    static member Default(columnType : ColumnType) = ParameterTransform.Default(columnType, fun t -> { ParameterType = t.DbType; ValueTransform = fun e -> e })
    static member Default(columnType : ColumnType, interiorPrimitiveTransform : ColumnType -> ParameterTransform) =
        // 1. First we'll have to check for the type being an Option<T>. Add null logic if so.
        //    Surprisingly we don't need this for Nullable<T>. A boxed Nullable<T> is already the same as
        //    the boxed form of its value.
        //    Any form of null, whether None or null or (eventually once we support ValueNone) becomes DBNull.Value.
        let optionalsToDbNull (expr : Quotations.Expr) (nextStep : Quotations.Expr -> Quotations.Expr) =
            let ty = expr.Type
            let asObj = Expr.Coerce(expr, typeof<obj>)
            if ty.IsConstructedGenericType && ty.GetGenericTypeDefinition() = typedefof<_ option> then
                let invokeValue = Expr.Coerce(Expr.PropertyGet(expr, ty.GetProperty("Value")), typeof<obj>)
                <@@ if isNull %%asObj then box DBNull.Value else box (%%nextStep(invokeValue)) @@>
            else
                <@@ if isNull %%asObj then box DBNull.Value else box (%%nextStep(asObj)) @@>
        // 2. After null check, if the type is a UserTypeBasedOn, and is non-null, we call ToPrimitive on it.
        //    This is usually a static method but could be an instance method, for example on auto-generated DU
        //    UserPrimitive mappings.
        let truePrimitiveType, unwrapper =
            match columnType.Type with
            | UserTypeBasedOn (userTy, underlying) ->
                let meth = userTy.RuntimeMapping.ToPrimitiveMethod
                { Nullable = false; Type = underlying },
                    if meth.IsStatic then
                        fun expr -> Expr.Call(meth, [expr])
                    else
                        fun expr -> Expr.Call(expr, meth, [])
            | _ ->
                { columnType with Nullable = false }, id
        // 3. Now the fundamental underlying primitive could still be one the backend doesn't *really* support.
        //    In the case of SQLite for example, we fake support for DateTime by using a string underlying type.
        //    For this reason the backend gets to do an extra interception on the underlying column type.
        //    This is the "interior primitive transform".
        let interior =
            interiorPrimitiveTransform truePrimitiveType
        {   ParameterType = interior.ParameterType
            ValueTransform = fun e ->
                optionalsToDbNull e (fun next ->
                    interior.ValueTransform(unwrapper next))
        }
        

type IBackend =
    abstract member InitialModel : Model
    abstract member MigrationBackend : Quotations.Expr<ConnectionInfo -> IMigrationBackend>
    abstract member ParameterTransform
        : columnType : ColumnType -> ParameterTransform
    abstract member ToCommandFragments
        : indexer : IParameterIndexer * stmts : TTotalStmts -> CommandFragment IReadOnlyList
