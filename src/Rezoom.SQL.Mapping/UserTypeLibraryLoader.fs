module Rezoom.SQL.Mapping.UserTypeLibraryLoader
open System
open System.IO
open System.Reflection
open System.Text.RegularExpressions
open FSharp.Quotations
open Rezoom.SQL.Mapping.CodeGeneration

/// Logic to load UserTypeLibrary info from assemblies via reflection.
[<Struct>]
type private CandidateMethodType =
    | FromPrimitive
    | ToPrimitive

type private CandidateMethod =
    {   MethodInfo : MethodInfo
        CandidateType : CandidateMethodType
        CustomType : Type
        UnderlyingPrimitive : Type
    }
// Funky naming for F# extension methods. The method names themselves contain dots.
// Like "DateTimeOffset.FromPrimitive.Static" can be the MethodInfo.Name.
let private toPrimitiveFSharpExtension = Regex(@"\.ToPrimitive$")
let private fromPrimitiveFSharpExtension = Regex(@"\.FromPrimitive\.Static$")

let private checkForCandidateMethod (meth : MethodInfo) =
    if meth.IsGenericMethod then None else
    let candType =
        match meth.Name with
        | "FromPrimitive" -> Some FromPrimitive
        | "ToPrimitive" -> Some ToPrimitive
        | x when toPrimitiveFSharpExtension.IsMatch(x) -> Some ToPrimitive
        | x when fromPrimitiveFSharpExtension.IsMatch(x) -> Some FromPrimitive
        | _ -> None
    match candType with
    | None -> None
    | Some candType ->
        match meth.GetParameters() with
        | [| singleParam |] ->
            let customType, underlyingType =
                match candType with
                | FromPrimitive -> meth.ReturnType, singleParam.ParameterType
                | ToPrimitive -> singleParam.ParameterType, meth.ReturnType
            {   MethodInfo = meth
                CandidateType = candType
                CustomType = customType
                UnderlyingPrimitive = underlyingType
            } |> Some
        | _ -> None

let private findCustomMappingsInType (publicType : Type) : RuntimeMapping seq =
    seq {
        let methods = publicType.GetMethods(BindingFlags.Public ||| BindingFlags.Static ||| BindingFlags.DeclaredOnly)
        let candidateGroups =
            methods
            |> Array.choose checkForCandidateMethod
            |> Array.groupBy (fun a -> a.CustomType)
        for customType, candidateMethods in candidateGroups do
            let toPrims, fromPrims = candidateMethods |> Array.partition (fun m -> m.CandidateType = ToPrimitive)
            match toPrims, fromPrims with
            | [| singleToPrim |], [| singleFromPrim |] ->
                if singleToPrim.UnderlyingPrimitive <> singleFromPrim.UnderlyingPrimitive then
                    failwithf
                        "Custom type %s has conflicting primitive types: %s takes a %s, but %s returns a %s."
                            customType.FullName
                            (singleFromPrim.MethodInfo.DeclaringType.FullName + "." + singleFromPrim.MethodInfo.Name)
                            singleFromPrim.UnderlyingPrimitive.FullName
                            (singleToPrim.MethodInfo.DeclaringType.FullName + "." + singleToPrim.MethodInfo.Name)
                            singleToPrim.UnderlyingPrimitive.FullName
                elif singleToPrim.UnderlyingPrimitive = singleToPrim.CustomType then
                    failwithf
                        "Custom type %s is mapped to itself via %s."
                        singleToPrim.CustomType.FullName
                        (singleFromPrim.MethodInfo.DeclaringType.FullName + "." + singleFromPrim.MethodInfo.Name)
                elif not <| PrimitiveConverters.isFundamentalPrimitive singleToPrim.UnderlyingPrimitive then
                    failwithf
                        "Custom type %s converter %s maps to %s, but that is not a supported primitive database type."
                        singleToPrim.CustomType.FullName
                        (singleFromPrim.MethodInfo.DeclaringType.FullName + "." + singleFromPrim.MethodInfo.Name)
                        singleToPrim.UnderlyingPrimitive.FullName
                else
                yield
                    {   FromPrimitiveMethod = singleFromPrim.MethodInfo
                        ToPrimitiveMethod = singleToPrim.MethodInfo
                    }
            | [||], _ ->
                failwithf "Missing ToPrimitive for custom type %s." customType.FullName
            | _, [||] ->
                failwithf "Missing FromPrimitive for custom type %s." customType.FullName
            | _ ->
                failwithf "Custom type %s has multiple ToPrimitive and FromPrimitive static methods defined for it, within the same type %s."
                    customType.FullName
                    publicType.FullName
    }

let private findUserTypesInAssembly (asm : Assembly) : UserPrimitiveType seq =
    seq {
        for publicType in asm.GetExportedTypes() do
            for customMapping in findCustomMappingsInType publicType do
                yield
                    {   UserCLRType = customMapping.FromPrimitiveMethod.ReturnType
                        UnderlyingCLRType = customMapping.ToPrimitiveMethod.ReturnType
                        RawBackendSQLType = None
                        SQLTypeLength = None
                        RuntimeMapping = customMapping
                        IsAutomaticImplemention = false
                    }
            match PrimitiveConverters.findSingleCaseDU publicType with
            | ValueNone -> ()
            | ValueSome singleCase -> yield singleCase
    }

let loadUserTypeLibrary (asms : Assembly array) =
    if Array.isEmpty asms then UserTypeLibrary.Empty else
    let mappings = asms |> Seq.collect findUserTypesInAssembly |> Seq.toArray
    let identity = asms |> Seq.map (fun a -> a.GetName().Name) |> Seq.sortBy (fun n -> n) |> String.concat "&"
    UserTypeLibrary(identity, mappings)

/// An entry that contains a path separator or ends in ".dll" is
/// treated as a literal file path; otherwise it's an assembly *name*
/// to be resolved against the compilation's referenced assemblies.
let private looksLikePath (entry : string) =
    entry.IndexOfAny([| '/'; '\\' |]) >= 0
    || entry.EndsWith(".dll", System.StringComparison.OrdinalIgnoreCase)

let private resolveEntryPath
    (configDir : string)
    (referencedAssemblyPaths : string list)
    (entry : string) : string =
    if looksLikePath entry then
        if System.IO.Path.IsPathRooted(entry) then entry
        else System.IO.Path.Combine(configDir, entry)
    else
        match
            referencedAssemblyPaths
            |> List.tryFind (fun p ->
                System.String.Equals(
                    System.IO.Path.GetFileNameWithoutExtension(p),
                    entry,
                    System.StringComparison.OrdinalIgnoreCase))
        with
        | Some path -> path
        | None ->
            failwithf
                "Could not resolve UserTypes assembly '%s' listed in rzsql.json. Make sure the project references it or the path exists."
                entry

/// Module-level so the MLC (and the Types/MethodInfos it produced) stays
/// alive for the lifetime of the design-time process. Keyed by ref-set so
/// multiple TPU projects loaded in the same host can share an MLC when
/// their references match.
let private mlcCache =
    System.Collections.Concurrent.ConcurrentDictionary<string, MetadataLoadContext>()

let private getOrCreateMetadataLoadContext (referencedAssemblyPaths : string list) =
    let key = referencedAssemblyPaths |> List.sort |> String.concat "|"
    mlcCache.GetOrAdd(key, fun _ ->
        let resolver = new PathAssemblyResolver(referencedAssemblyPaths)
        new MetadataLoadContext(resolver))

// Easy path used by tests that load a UserTypeLibrary directly without a TP
// host (and so without access to the compilation's referenced assemblies).
// The TP design-time always goes through loadUserTypeLibraryFromConfig.
let loadUserTypeLibraryFromPaths (configDir : string) (paths : string seq) =
    [|  for path in paths ->
            let path =
                if System.IO.Path.IsPathRooted(path) then path else
                System.IO.Path.Combine(configDir, path)
            // avoid locking the path by loading from bytes
            Assembly.Load(System.IO.File.ReadAllBytes(path))
    |] |> loadUserTypeLibrary

let loadUserTypeLibraryFromConfig
    (configDir : string)
    (referencedAssemblyPaths : string seq)
    (entries : string seq) =
    let entriesList = entries |> Seq.toList
    if List.isEmpty entriesList then UserTypeLibrary.Empty else
    let refsList = referencedAssemblyPaths |> Seq.toList
    if List.isEmpty refsList then loadUserTypeLibraryFromPaths configDir entriesList else
    // Inspect user-types via MetadataLoadContext so the DLL is never loaded
    // into the default AssemblyLoadContext. VS's F# language service
    // introspects assemblies in the default ALC for IntelliSense, and that
    // forces CLR materialization of every type's custom attributes.
    // The F#-auto-emitted [<DebuggerDisplay>] fails to resolve in the VS host and breaks the TP.
    // MLC isolation keeps the DLL invisible to that scan
    // while still giving us the metadata the TP needs. The MethodInfos we
    // return are MLC-flavoured and can't be called, but ProvidedTypes.fs
    // source-to-target conversion (convMethodRefToTgt) can use them.
    let mlc = getOrCreateMetadataLoadContext refsList
    let asms =
        [|  for entry in entriesList ->
                let path = resolveEntryPath configDir refsList entry
                // Load from MemoryStream instead of path so TP doesn't lock
                // the assembly, which would be a huge pain for users.
                let bytes = File.ReadAllBytes(path)
                let stream = new MemoryStream(bytes, writable = false)
                mlc.LoadFromStream(stream)
        |]
    asms |> loadUserTypeLibrary

module FreezeDry =
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
                            h.ToPrimitiveMethodName clrTy.FullName
                    let underlyingTy = toPrim.ReturnType
                    let fromPrim = decTy.GetMethod(h.FromPrimitiveMethodName, BindingFlags.Static ||| BindingFlags.Public, null, [|underlyingTy|], null)
                    if isNull fromPrim then
                        failwithf "Rehydration failed: from-prim method '%s' not found on %s"
                            h.ToPrimitiveMethodName clrTy.FullName
                    {   UserCLRType = clrTy
                        UnderlyingCLRType = toPrim.ReturnType
                        RawBackendSQLType = None // don't need at runtime
                        SQLTypeLength = None // don't need at runtime
                        RuntimeMapping = { FromPrimitiveMethod = fromPrim; ToPrimitiveMethod = toPrim }
                        IsAutomaticImplemention = h.IsAutomaticImplemention
                    })
            UserTypeLibrary(freezeDried.Identity, types))