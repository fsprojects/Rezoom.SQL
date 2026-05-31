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

/// User types mapped to/from SQL primitive types.
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
        IsAutomaticImplementation : bool
    }
    member this.Name = this.UserCLRType.Name

/// User types for rows read from result sets. Currently interfaces only.
type UserRowType =
    {   UserCLRType : Type
    }

type TypeResolutionByName<'a> =
    | FoundType of 'a
    | AmbiguousType of 'a array
    | NotFoundType of mistakeCandidates : string array

type private NameMap<'a>(elements : 'a array, name : 'a -> string, fullName : 'a -> string) =
    let byName = elements |> Array.groupBy name |> dict
    let byFullName = elements |> Seq.map (fun t -> fullName t, t) |> dict
    member this.Find(name : string) =
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

/// All the compile-time information we have about user types.
type UserTypeLibrary(identity : string, primitives : UserPrimitiveType array, rowTypes : UserRowType array) =
    static let empty = UserTypeLibrary("", [||], [||])
    // if we have two UserPrimitiveTypes for the same CLR type, one is auto, one is manual
    // only keep the manual one
    let primTypes =
        [| for _, implementations in primitives |> Seq.groupBy (fun t -> t.UserCLRType) do
            yield implementations |> Seq.sortBy (fun t -> t.IsAutomaticImplementation) |> Seq.head
        |]
    let primsByName = NameMap(primTypes, (fun t -> t.Name), fun t -> t.UserCLRType.FullName)
    let primsByCLRType = primTypes |> Seq.map (fun t -> t.UserCLRType, t) |> dict
    let rowTypesByName = NameMap(rowTypes, (fun t -> t.UserCLRType.Name), fun t -> t.UserCLRType.FullName)
    member this.IsEmpty = primTypes.Length = 0
    member this.CountPrimitives = primTypes.Length
    member this.Identity = identity
    member this.AllPrimitives = primTypes
    member this.UserPrimitiveByName(name : string) = primsByName.Find(name)
    member this.UserRowTypeByName(name : string) = rowTypesByName.Find(name)
    member this.TryGetMapping(ty : Type) : RuntimeMapping voption =
        let succ, mapping = primsByCLRType.TryGetValue(ty)
        if succ then ValueSome mapping.RuntimeMapping
        else ValueNone
    /// The runtime assemblies that contributed types to this library.
    /// The TP design-time uses these to register source-side assemblies with
    /// ProvidedTypes' target/source conversions, so target IL refs to
    /// user types can be mapped back to their loaded reflection Types.
    member this.SourceAssemblies : Assembly seq =
        primTypes
        |> Seq.map (fun t -> t.UserCLRType.Assembly)
        |> Seq.distinct
    static member Empty = empty
