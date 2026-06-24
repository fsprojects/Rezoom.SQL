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
            }
        @@>

/// Used to rebuild a UserTypeLibrary at runtime with information that was known to the design-time type provider.
///
/// Types is a function rather than a materialized array: the generated Command method
/// runs this record's construction on *every* call, so we don't want to allocate
/// the whole array of records every time. Especially since the user's library could have
/// dozens of user types (they have a wrapper with custom conversions for every entity's ID type, for example).
/// Rehydrate forces it once (and caches the result by Identity), so subsequent callers don't actually
/// invoke Types() and build all those records.
type FreezeDriedUserTypeLibrary =
    {   Identity : string
        Types : unit -> FreezeDriedUserPrimitiveType array
    }
    static member Of(lib : UserTypeLibrary) =
        // Exclude IsAutomaticImplemention types (single-case F# DUs like `type UserId = UserId of int64`).
        // The runtime can re-derive those on the fly.
        // Example: PrimitiveConverters.converter falls back to findSingleCaseDU
        // when an explicit usertype mapping is not present.
        // So freeze drying them would be redundant and would bloat the code the type provider generates.
        let types =
            lib.AllPrimitives
            |> Array.filter (fun t -> not t.IsAutomaticImplementation)
            |> Array.map FreezeDriedUserPrimitiveType.Of
        {   Identity = lib.Identity
            Types = fun () -> types
        }
    member this.Quote() =
        let typeQuotes = [ for t in this.Types() -> t.Quote() ]
        let arr = Expr.NewArray(typeof<FreezeDriedUserPrimitiveType>, typeQuotes)
        <@@
            {   Identity = %%Expr.Value(this.Identity)
                Types = fun () -> (%%arr : FreezeDriedUserPrimitiveType array)
            }
        @@>

let private cache = ConcurrentDictionary<string, UserTypeLibrary>()
let rehydrate (freezeDried : FreezeDriedUserTypeLibrary) : UserTypeLibrary =
    cache.GetOrAdd(freezeDried.Identity, fun _ ->
        let types =
            freezeDried.Types() |> Array.map (fun h ->
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
                    SQLParameterDbType = None // don't currently use at runtime, but note: may eventually need to upgrade, when we start doing dynamic filtering
                    RuntimeMapping = { FromPrimitiveMethod = fromPrim; ToPrimitiveMethod = toPrim }
                    // Freeze-dried libraries do not include auto-implementations since the runtime would rederive them anyway
                    // so it would be code bloat.
                    IsAutomaticImplementation = false
                })
        // row types empty because these are only relevant at design-time and freezedry rehydrate is
        // for making the runtime work.
        UserTypeLibrary(freezeDried.Identity, types, rowTypes = [||]))
