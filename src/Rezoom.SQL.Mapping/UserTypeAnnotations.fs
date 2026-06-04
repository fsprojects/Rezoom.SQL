module Rezoom.SQL.Mapping.UserTypeAnnotations
open System
open System.Reflection

// Have to do comparisons by name, because we use this on MLC types
// and typeof<> is not going to be reference-identical.

let private rawBackendSqlTypeName =
    typeof<Rezoom.SQL.Annotations.RawBackendSQLTypeAttribute>.FullName
let private sqlTypeLengthName =
    typeof<Rezoom.SQL.Annotations.SQLTypeLengthAttribute>.FullName

/// Read both annotation attributes from a single member.
/// Returns (rawBackendSqlType, sqlTypeLength).
let readMember (m : MemberInfo) =
    let mutable raw = None
    let mutable len = None
    for attr in m.GetCustomAttributesData() do
        let fullName = attr.AttributeType.FullName
        if fullName = rawBackendSqlTypeName
           && attr.ConstructorArguments.Count >= 1 then
            match attr.ConstructorArguments.[0].Value with
            | :? string as v -> raw <- Some v
            | _ -> ()
        elif fullName = sqlTypeLengthName
             && attr.ConstructorArguments.Count >= 1 then
            match attr.ConstructorArguments.[0].Value with
            | :? int as v -> len <- Some v
            | _ -> ()
    raw, len

let private validateExclusive (label : string) (raw, len) =
    match raw, len with
    | Some _, Some _ ->
        failwithf
            "User primitive %s has both [<RawBackendSQLType>] and [<SQLTypeLength>] applied. They are mutually exclusive — RawBackendSQLType already specifies the complete SQL type string including any length parameter."
            label
    | _ -> raw, len

// If both methods set the attribute they must set the same
// value; if only one does, that one wins.
let private agreeOnMethods (attrLabel : string) (label : string) (a : 'a option) (b : 'a option) =
    match a, b with
    | None, x | x, None -> x
    | Some av, Some bv when av = bv -> Some av
    | Some av, Some bv ->
        failwithf
            "User primitive %s has conflicting [<%s>] attributes on its ToPrimitive (%A) and FromPrimitive (%A) methods."
            label attrLabel av bv

/// Resolve attributes for an explicit ToPrimitive/FromPrimitive
/// user primitive. `declaring` is the wrapper class that holds
/// type-level attributes; the two methods may also be annotated.
/// Method-level wins over type-level.
let resolveExplicit (label : string) (declaring : Type) (toPrim : MethodInfo) (fromPrim : MethodInfo) =
    let typeRaw, typeLen = readMember (declaring :> MemberInfo)
    let toRaw, toLen = readMember (toPrim :> MemberInfo)
    let fromRaw, fromLen = readMember (fromPrim :> MemberInfo)
    let methodRaw = agreeOnMethods "RawBackendSQLType" label toRaw fromRaw
    let methodLen = agreeOnMethods "SQLTypeLength" label toLen fromLen
    let raw = methodRaw |> Option.orElse typeRaw
    let len = methodLen |> Option.orElse typeLen
    validateExclusive label (raw, len)

/// Resolve attributes for the auto-DU path where there is just one
/// type (the DU itself) to inspect.
let resolveType (typ : Type) =
    readMember (typ :> MemberInfo)
    |> validateExclusive typ.FullName
