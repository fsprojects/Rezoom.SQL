namespace Rezoom.SQL.Mapping
open System
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

type RuntimeMapping =
    {   /// Static method the user defined to convert string -> PhoneNumber.
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
    member this.UnderlyingPrimitive = this.ToPrimitiveMethod.ReturnType
    member this.CustomType = this.FromPrimitiveMethod.ReturnType

type UserPrimitiveType =
    {   UserCLRType : Type
        UnderlyingCLRType : Type
        RawBackendSQLType : string option
        /// If our underlying type is string, we may want to specify a max length in SQL.
        /// Ignored if RawBackendSQLType is Some, in which case we trust it directly.
        SQLTypeLength : int option
        RuntimeMapping : RuntimeMapping
        /// True if this is an implementation we derived automatically, such as for an F#
        /// single-case DU, as opposed to one the user specified with their own ToPrimitive
        /// and FromPrimitive method implementations.
        IsAutomaticImplemention : bool
    }
    member this.Name = this.UserCLRType.Name

type TypeResolutionByName =
    | FoundType of UserPrimitiveType
    | AmbiguousType of UserPrimitiveType array
    | NotFoundType of mistakeCandidates : string array

/// All the compile-time information we have about user types.
type UserTypeLibrary(identity : string, types : UserPrimitiveType array) =
    static let empty = UserTypeLibrary("", [||])
    // if we have two UserPrimitiveTypes for the same CLR type, one is auto, one is manual
    // only keep the manual one
    let types =
        [| for _, implementations in types |> Seq.groupBy (fun t -> t.UserCLRType) do
            yield implementations |> Seq.sortBy (fun t -> t.IsAutomaticImplemention) |> Seq.head
        |]
    let byName = types |> Array.groupBy (fun t -> t.Name) |> dict
    let byFullName = types |> Seq.map (fun t -> t.UserCLRType.FullName, t) |> dict
    let byCustomType = types |> Seq.map (fun t -> t.UserCLRType, t) |> dict
    member this.CountPrimitives = types.Length
    member this.Identity = identity
    member this.AllTypes = types
    member this.UserPrimitiveByName(name : string) =
        let succ, ty = byFullName.TryGetValue(name)
        if succ then FoundType ty else
        let succ, matches = byName.TryGetValue(name)
        if succ then
            match matches with
            | [| exactlyOne |] -> FoundType exactlyOne
            | multiple -> AmbiguousType multiple
        else
            let candidates =
                byName.Keys |> Seq.append byFullName.Keys |> Levenshtein.mistakeCandidates name |> Seq.toArray
            NotFoundType candidates
    member this.TryGetMapping(ty : Type) : RuntimeMapping voption =
        let succ, mapping = byCustomType.TryGetValue(ty)
        if succ then ValueSome mapping.RuntimeMapping
        else ValueNone
    /// The runtime assemblies that contributed types to this library.
    /// The TP design-time uses these to register source-side assemblies with
    /// ProvidedTypes' target/source conversions, so target IL refs to
    /// user types can be mapped back to their loaded reflection Types.
    member this.SourceAssemblies : Assembly seq =
        types
        |> Seq.map (fun t -> t.UserCLRType.Assembly)
        |> Seq.distinct
    static member Empty = empty