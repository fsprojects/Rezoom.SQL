module Rezoom.SQL.Mapping.CustomPrimitives
open System
open System.Collections.Generic
open System.Reflection

// Custom primitive types.
// Maybe you have a wrapper type that doesn't fit our auto-support (not a single-case F# DU).
// type EmailAddress(addr: string) =
//     member this.IAmAClass() = "come on buddy I'm not a discriminated union!"
// Also useful for complex types that are serialized (json, comma-sep, other representations) to store in a single column.
// Maybe you have a PhoneNumber type and you store it in the DB as "+15558675309" but your PhoneNumber class has
// members .AreaCode, .CountryCode, etc. because you're cool like that.

// Out of scope: generics. You can't do `type Id<'a> = Id of int` and use `Id<User>` and `Id<Company>` and so on.
// Just gonna get way too complicated for me right now.

type CustomPrimitiveMapping =
    {   /// The user's fancy type e.g. PhoneNumber
        CustomType : Type
        /// The actual type that goes into the database e.g. string
        UnderlyingPrimitive : Type
        /// Static method the user defined to convert string -> PhoneNumber.
        /// It is tempting to rely on the ctor as long as the type has an appropriate one, but we gotta require ToPrimitive to be
        /// written explicitly, so we might as well have total symmetry. Less magic, more obvious what's going on that way.
        FromPrimitiveMethod : MethodInfo
        /// Static method the user defined to convert PhoneNumber -> string.
        /// It is tempting to make this an instance method, but having both converter methods required to be static
        /// allows the user type to choose how it wants to handle a `null` PhoneNumber at runtime.
        /// Also it simplifies our reflection code and pathways because we support extension method converters,
        /// and those *have* to be static, so the method being static every time means one codegen path to test.
        ToPrimitiveMethod : MethodInfo
    }

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

let private checkForCandidateMethod (meth : MethodInfo) =
    if meth.IsGenericMethod then None else
    let candType =
        match meth.Name with
        | "FromPrimitive" -> Some FromPrimitive
        | "ToPrimitive" -> Some ToPrimitive
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

let findMappingsInAssembly (asm : Assembly) : CustomPrimitiveMapping seq =
    seq {
        for publicType in asm.GetExportedTypes() do
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
                                (singleFromPrim.MethodInfo.DeclaringType.Name + "." + singleFromPrim.MethodInfo.Name)
                                singleFromPrim.UnderlyingPrimitive.FullName
                                (singleToPrim.MethodInfo.DeclaringType.Name + "." + singleToPrim.MethodInfo.Name)
                                singleToPrim.UnderlyingPrimitive.FullName
                    else
                    yield
                        {   CustomType = customType
                            UnderlyingPrimitive = singleToPrim.UnderlyingPrimitive
                            FromPrimitiveMethod = singleFromPrim.MethodInfo
                            ToPrimitiveMethod = singleToPrim.MethodInfo
                        }
                | [||], _ ->
                    failwithf "Missing ToPrimitive for custom type %s" customType.FullName
                | _, [||] ->
                    failwithf "Missing FromPrimitive for custom type %s" customType.FullName
                | _ ->
                    failwithf "Custom type %s has multiple ToPrimitive and FromPrimitive static methods defined for it, within the same type %s."
                        customType.FullName
                        publicType.FullName
    }

/// Holds all the custom primitive mappings we are aware of the user defining for their model.
type CustomPrimitiveMappings(mappings : CustomPrimitiveMapping seq) =
    let byType = Dictionary<Type, CustomPrimitiveMapping>()
    do
        for mapping in mappings do
            if byType.ContainsKey(mapping.CustomType) then
                failwithf "Multiple primitive mappings defined for the same type %s" mapping.CustomType.AssemblyQualifiedName
            byType.[mapping.CustomType] <- mapping
    member this.TryGetMapping(t : Type) =
        let found, map = byType.TryGetValue(t)
        if found then ValueSome map else ValueNone

