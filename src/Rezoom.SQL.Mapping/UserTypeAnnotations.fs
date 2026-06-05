module Rezoom.SQL.Mapping.UserTypeAnnotations
open System
open System.Reflection

// Attribute identity is compared by FullName because
// (a) we read attributes off MLC-loaded types whose runtime-Type
// identity does not match the executing assembly's view, and
// (b) using typeof<Rezoom.SQL.Annotations.etc> here would force a
// live load of Rezoom.SQL.Annotations the first time this
// module is touched. The F# compiler TP host resolves dependencies
// via its own load contexts and can end up with the reference-only
// /ref/ DLL pinned, which then refuses to load for execution and blows up the TP.
// Hardcoded FullName strings are ugly but avoid both.

/// Attribute name from Rezoom.SQL.Annotations assembly.
let private rawBackendSqlTypeName =
    "Rezoom.SQL.Annotations.RawBackendSQLTypeAttribute"

/// Attribute name from Rezoom.SQL.Annotations assembly.
let private sqlTypeLengthName =
    "Rezoom.SQL.Annotations.SQLTypeLengthAttribute"

/// Attribute name from Rezoom.SQL.Annotations assembly.
let private sqlParameterDbTypeName =
    "Rezoom.SQL.AnnotationsSQLParameterDbTypeAttribute"

type AnnotationsForMember =
    {   TypeName : string
        RawType : string option
        Length : int option
        ParameterDbType : (string * int) option
    }
    member this.ValidateExclusive() =
        match this.RawType, this.Length with
        | Some _, Some _ ->
            failwithf
                "User primitive %s has both [<RawBackendSQLType>] and [<SQLTypeLength>] applied. They are mutually exclusive — RawBackendSQLType already specifies the complete SQL type string including any length parameter."
                this.TypeName
        | _ -> this
    member this.Merge(other : AnnotationsForMember) =
        let tName = this.TypeName
        let inline agree attrLabel l r =
            match l, r with
            | Some lv, Some rv when lv = rv -> l
            | Some lv, Some rv ->
                failwithf
                    "User primitive %s has conflicting [<%s>] attributes."
                    tName attrLabel
            | Some _, None -> l
            | None, Some _ -> r
            | None, None -> None
        {   TypeName = this.TypeName
            RawType = agree rawBackendSqlTypeName this.RawType other.RawType
            Length = agree sqlTypeLengthName this.Length other.Length
            ParameterDbType = agree sqlParameterDbTypeName this.ParameterDbType other.ParameterDbType
        }

let readMember (typeName : string) (m : MemberInfo) : AnnotationsForMember =
    let mutable acc = { TypeName = typeName; RawType = None; Length = None; ParameterDbType = None }
    for attr in m.GetCustomAttributesData() do
        acc <-
            let fullName = attr.AttributeType.FullName
            if fullName = rawBackendSqlTypeName
               && attr.ConstructorArguments.Count >= 1 then
                match attr.ConstructorArguments.[0].Value with
                | :? string as v -> acc.Merge({ acc with RawType = Some v }).ValidateExclusive()
                | _ -> acc
            elif fullName = sqlTypeLengthName
                 && attr.ConstructorArguments.Count >= 1 then
                match attr.ConstructorArguments.[0].Value with
                | :? int as v -> acc.Merge({ acc with Length = Some v }).ValidateExclusive()
                | _ -> acc
            elif fullName = sqlParameterDbTypeName
                && attr.ConstructorArguments.Count >= 2 then
                match attr.ConstructorArguments.Count with
                | 1 ->
                    // single-arg ctor is the DbType-only version
                    match attr.ConstructorArguments.[0].Value with
                    | :? int as v -> acc.Merge({ acc with ParameterDbType = Some ("DbType", v) })
                    | _ -> acc
                | _ ->
                    match attr.ConstructorArguments.[0].Value, attr.ConstructorArguments.[1].Value with
                    | (:? string as propName), (:? int as dbType) ->
                        acc.Merge({ acc with ParameterDbType = Some (propName, dbType) })
                    | _ -> acc
            else acc
    acc

/// Resolve attributes for an explicit ToPrimitive/FromPrimitive
/// user primitive. `declaring` is the wrapper class that holds
/// type-level attributes; the two methods may also be annotated.
let resolveExplicit (declaring : Type) (toPrim : MethodInfo) (fromPrim : MethodInfo) =
    let typeAttrs = readMember declaring.Name (declaring :> MemberInfo)
    let toPrimAttrs = readMember declaring.Name (toPrim :> MemberInfo)
    let fromPrimAttrs = readMember declaring.Name (fromPrim :> MemberInfo)
    typeAttrs.Merge(toPrimAttrs).Merge(fromPrimAttrs).ValidateExclusive()

/// Resolve attributes for the auto-DU path where there is just one
/// type (the DU itself) to inspect.
let resolveType (typ : Type) =
    readMember typ.Name (typ :> MemberInfo)
