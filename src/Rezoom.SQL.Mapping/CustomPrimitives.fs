namespace Rezoom.SQL.Mapping.CodeGeneration
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

/// Holds all the custom primitive mappings we are aware of the user defining for their model.
type CustomPrimitiveMappings(identity : string, mappings : CustomPrimitiveMapping seq) =
    static let empty = CustomPrimitiveMappings("", Seq.empty)
    let byType = Dictionary<Type, CustomPrimitiveMapping>()
    do
        for mapping in mappings do
            if byType.ContainsKey(mapping.CustomType) then
                failwithf "Multiple primitive mappings defined for the same type %s" mapping.CustomType.AssemblyQualifiedName
            byType.[mapping.CustomType] <- mapping
    member this.TryGetMapping(t : Type) =
        let found, map = byType.TryGetValue(t)
        if found then ValueSome map else ValueNone
    /// Identity of this set of mappings. This way we can cache the entity reader generated for each mapping set
    /// in case the same row type is used by two different models with two different sets of configuration for
    /// custom type mapping. Which would be a bad idea, but if people want to do it... fine.
    /// The identity string can be anything, but we'll typically generate it from the set of assembly names that were searched
    /// to locate mappings.
    member this.Identity = identity
    static member Empty = empty

