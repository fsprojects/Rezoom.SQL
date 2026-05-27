module Rezoom.SQL.Mapping.FreezeDry
open System.Reflection
open FSharp.Quotations
open System.Collections.Concurrent

/// Used to rebuild a UserPrimitiveType at runtime with information that was known to the design-time type provider.
/// Faster than searching a list of assemblies exhaustively for usertypes.
type FreezeDriedUserPrimitiveType =
    {   /// This is the type we are converting from/to.
        UserCLRTypeFullName : string
        UserCLRTypeAssemblyName : string

        /// This is the type that owns the ToPrimitive and FromPrimitive methods.
        /// It could be the same as the UserCLRType or it could be a totally separate extension class.
        DeclaringTypeFullName : string
        DeclaringAssemblyName : string

        FromPrimitiveMethodName : string
        ToPrimitiveMethodName : string
        ToPrimitiveIsInstanceMethod : bool
        IsAutomaticImplemention : bool
    }
    static member Of(t : UserPrimitiveType) =
        let rtMap = t.RuntimeMapping
        let dType = rtMap.FromPrimitiveMethod.DeclaringType
        {   UserCLRTypeFullName = t.UserCLRType.FullName
            UserCLRTypeAssemblyName = t.UserCLRType.Assembly.GetName().Name
            DeclaringTypeFullName = dType.FullName
            DeclaringAssemblyName = dType.Assembly.GetName().Name
            FromPrimitiveMethodName = rtMap.FromPrimitiveMethod.Name
            ToPrimitiveMethodName = rtMap.ToPrimitiveMethod.Name
            ToPrimitiveIsInstanceMethod = not rtMap.ToPrimitiveMethod.IsStatic
            IsAutomaticImplemention = t.IsAutomaticImplemention
        }
    member this.Quote() =
        <@@
            {   UserCLRTypeFullName = %%Expr.Value(this.UserCLRTypeFullName)
                UserCLRTypeAssemblyName = %%Expr.Value(this.UserCLRTypeAssemblyName)
                DeclaringTypeFullName = %%Expr.Value(this.DeclaringTypeFullName)
                DeclaringAssemblyName = %%Expr.Value(this.DeclaringAssemblyName)
                FromPrimitiveMethodName = %%Expr.Value(this.FromPrimitiveMethodName)
                ToPrimitiveMethodName = %%Expr.Value(this.ToPrimitiveMethodName)
                ToPrimitiveIsInstanceMethod = %%Expr.Value(this.ToPrimitiveIsInstanceMethod)
                IsAutomaticImplemention = %%Expr.Value(this.IsAutomaticImplemention)
            }
        @@>

/// Used to rebuild a UserTypeLibrary at runtime with information that was known to the design-time type provider.
type FreezeDriedUserTypeLibrary =
    {   Identity : string
        Types : FreezeDriedUserPrimitiveType array
    }
    static member Of(lib : UserTypeLibrary) =
        {   Identity = lib.Identity
            Types = lib.AllTypes |> Array.map FreezeDriedUserPrimitiveType.Of
        }
    member this.Quote() =
        let typeQuotes = [ for t in this.Types -> t.Quote() ]
        <@@
            {   Identity = %%Expr.Value(this.Identity)
                Types = %%Expr.NewArray(typeof<FreezeDriedUserPrimitiveType>, typeQuotes)
            }
        @@>

let private cache = ConcurrentDictionary<string, UserTypeLibrary>()
let rehydrate (freezeDried : FreezeDriedUserTypeLibrary) : UserTypeLibrary =
    cache.GetOrAdd(freezeDried.Identity, fun _ ->
        let types =
            freezeDried.Types |> Array.map (fun h ->
                let clrAsm = Assembly.Load(h.UserCLRTypeAssemblyName)
                let clrTy = clrAsm.GetType(h.UserCLRTypeFullName, throwOnError = true)
                let decAsm =
                    if h.DeclaringAssemblyName = h.UserCLRTypeAssemblyName then clrAsm
                    else Assembly.Load(h.DeclaringAssemblyName)
                let decTy =
                    if h.DeclaringAssemblyName = h.UserCLRTypeAssemblyName && h.DeclaringTypeFullName = h.UserCLRTypeFullName then clrTy
                    else decAsm.GetType(h.DeclaringTypeFullName, throwOnError = true)

                let toPrim =
                    if h.ToPrimitiveIsInstanceMethod then
                        clrTy.GetMethod(h.ToPrimitiveMethodName, BindingFlags.Instance ||| BindingFlags.Public, null, [||], null)
                    else
                        decTy.GetMethod(h.ToPrimitiveMethodName, BindingFlags.Static ||| BindingFlags.Public, null, [|clrTy|], null)
                if isNull toPrim then
                    failwithf "Rehydration failed: to-prim method '%s' not found on %s"
                        h.ToPrimitiveMethodName (if h.ToPrimitiveIsInstanceMethod then clrTy else decTy).FullName
                let underlyingTy = toPrim.ReturnType
                let fromPrim = decTy.GetMethod(h.FromPrimitiveMethodName, BindingFlags.Static ||| BindingFlags.Public, null, [|underlyingTy|], null)
                if isNull fromPrim then
                    failwithf "Rehydration failed: from-prim method '%s' not found on %s"
                        h.FromPrimitiveMethodName decTy.FullName
                {   UserCLRType = clrTy
                    UnderlyingCLRType = toPrim.ReturnType
                    RawBackendSQLType = None // don't need at runtime
                    SQLTypeLength = None // don't need at runtime
                    RuntimeMapping = { FromPrimitiveMethod = fromPrim; ToPrimitiveMethod = toPrim }
                    IsAutomaticImplemention = h.IsAutomaticImplemention
                })
        UserTypeLibrary(freezeDried.Identity, types))
