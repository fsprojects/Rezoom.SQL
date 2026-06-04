module Rezoom.SQL.Mapping.UserTypeLibraryLoader
open System
open System.IO
open System.Reflection
open System.Text.RegularExpressions
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
                elif not <| PrimitiveConverters.isFundamentalPrimitiveByFullName singleToPrim.UnderlyingPrimitive then
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
                let userCLRType = customMapping.FromPrimitiveMethod.ReturnType
                let raw, len =
                    UserTypeAnnotations.resolveExplicit
                        userCLRType.FullName
                        publicType
                        customMapping.ToPrimitiveMethod
                        customMapping.FromPrimitiveMethod
                yield
                    {   UserCLRType = userCLRType
                        UnderlyingCLRType = customMapping.ToPrimitiveMethod.ReturnType
                        RawBackendSQLType = raw
                        SQLTypeLength = len
                        RuntimeMapping = customMapping
                        IsAutomaticImplementation = false
                    }
            match PrimitiveConverters.findSingleCaseDU publicType with
            | ValueNone -> ()
            | ValueSome singleCase -> yield singleCase
    }

let private findRowTypesInAssembly (asm : Assembly) : UserRowType seq =
    seq {
        for publicType in asm.GetExportedTypes() do
            // right now we make no attempt to filter this to only interfaces
            // that would be constructable (e.g. all members are primitive types).
            // that'll error out at type-generation time anyway.
            if publicType.IsInterface then yield { UserCLRType = publicType }
    }

let loadUserTypeLibrary (asms : Assembly array) =
    if Array.isEmpty asms then UserTypeLibrary.Empty else
    let mappings = asms |> Seq.collect findUserTypesInAssembly |> Seq.toArray
    let rowTypes = asms |> Seq.collect findRowTypesInAssembly |> Seq.toArray
    let identity = asms |> Seq.map (fun a -> a.GetName().Name) |> Seq.sortBy (fun n -> n) |> String.concat "&"
    UserTypeLibrary(identity, mappings, rowTypes)

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

