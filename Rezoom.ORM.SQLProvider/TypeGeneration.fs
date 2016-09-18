module Rezoom.ORM.SQLProvider.TypeGeneration
open System
open System.Collections.Generic
open System.IO
open System.Reflection
open Microsoft.FSharp.Core.CompilerServices
open Microsoft.FSharp.Quotations
open ProviderImplementation.ProvidedTypes
open SQLow
open SQLow.CommandEffect

let generateType assembly ns typeName (command : CommandWithEffect) =
    let baseType = typeof<obj> // TODO pick base command type based on result sets
    let genTy = ProvidedTypeDefinition(assembly, ns, typeName, Some baseType, IsErased = false)
    genTy