namespace Rezoom.SQL.Mapping
open System

/// Marks a constructor as being the one to use when creating entities from blueprints.
[<AttributeUsage(AttributeTargets.Constructor, Inherited = false, AllowMultiple = false)>]
[<AllowNullLiteral>]
type BlueprintConstructorAttribute() =
    inherit Attribute()

/// Marks a property as being part of the primary key of its composite type.
[<AttributeUsage(AttributeTargets.Property ||| AttributeTargets.Field, Inherited = false, AllowMultiple = false)>]
[<AllowNullLiteral>]
type BlueprintKeyAttribute() =
    inherit Attribute()

/// Indicates that a property is represented with a different column name than its own member name.
[<AttributeUsage(AttributeTargets.Property ||| AttributeTargets.Field, Inherited = false, AllowMultiple = false)>]
[<AllowNullLiteral>]
type BlueprintColumnNameAttribute(name : string) =
    inherit Attribute()
    member __.Name = name

/// Indicates that a class has no key properties and should not be de-duplicated.
[<AttributeUsage(AttributeTargets.Class, Inherited = false, AllowMultiple = false)>]
[<AllowNullLiteral>]
type BlueprintNoKeyAttribute() =
    inherit Attribute()