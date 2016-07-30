namespace Rezoom.ORM
open System

[<AttributeUsage(AttributeTargets.Constructor, Inherited = false, AllowMultiple = false)>]
[<AllowNullLiteral>]
type BlueprintConstructorAttribute() =
    inherit Attribute()