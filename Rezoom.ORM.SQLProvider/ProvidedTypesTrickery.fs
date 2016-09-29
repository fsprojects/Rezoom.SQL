module internal Microsoft.FSharp.Quotations.DerivedPatterns
open System
open System.Text
open System.IO
open System.Reflection
open System.Linq.Expressions
open System.Collections.Generic
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.DerivedPatterns
open Microsoft.FSharp.Core.CompilerServices

let private metadataToken (minfo : MethodInfo) =
    try minfo.MetadataToken with
    | _ -> -1

let private methodsMatch isg1 gmd minfo1 minfo2 =
    metadataToken minfo1 = metadataToken minfo2
    &&  if isg1 then minfo2.IsGenericMethod && gmd = minfo2.GetGenericMethodDefinition()
        else minfo1 = minfo2

[<CompiledName("SpecificCallPattern")>]
let (|SpecificCall|_|) templateParameter = 
    match templateParameter with
    | (Lambdas(_, Call(_, minfo1, _)) | Call(_, minfo1, _)) ->
        // precompute these two
        let isg1 = minfo1.IsGenericMethod 
        let gmd = if isg1 then minfo1.GetGenericMethodDefinition() else null
        (fun tm -> 
            match tm with
            | Call(obj, minfo2, args) when methodsMatch isg1 gmd minfo1 minfo2 -> 
                Some(obj, (minfo2.GetGenericArguments() |> Array.toList), args)
            | _ -> None)
    | _ -> 
        invalidArg "templateParameter" "unrecognized method call"