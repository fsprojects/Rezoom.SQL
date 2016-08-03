namespace Rezoom.ORM
open System

[<AttributeUsage(AttributeTargets.Constructor, Inherited = false, AllowMultiple = false)>]
[<AllowNullLiteral>]
type BlueprintConstructorAttribute() =
    inherit Attribute()

[<AttributeUsage
    ( AttributeTargets.Field ||| AttributeTargets.Property ||| AttributeTargets.Parameter
    , Inherited = true
    , AllowMultiple = false
    )>]
[<AllowNullLiteral>]
type BlueprintQueryParentAttribute() =
    inherit Attribute()