module Rezoom.SQL.Mapping.UserTypeLibraryLoader
open System
open System.Reflection
open System.Text.RegularExpressions
open Microsoft.FSharp.Reflection
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

let loadUserTypeLibraryFromPaths (configDir : string) (paths : string seq) =
    [|  for path in paths ->
            let path =
                if System.IO.Path.IsPathRooted(path) then path else
                System.IO.Path.Combine(configDir, path)
            // avoid locking the path by loading from bytes
            Assembly.Load(System.IO.File.ReadAllBytes(path))
    |] |> loadUserTypeLibrary
