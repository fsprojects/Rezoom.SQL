namespace Rezoom.ORM
open System

/// Marks a constructor as being the one to use when creating entities from blueprints.
[<AttributeUsage(AttributeTargets.Constructor, Inherited = false, AllowMultiple = false)>]
[<AllowNullLiteral>]
type BlueprintConstructorAttribute() =
    inherit Attribute()