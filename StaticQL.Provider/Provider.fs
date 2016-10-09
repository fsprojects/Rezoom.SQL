namespace StaticQL.Provider
open System
open System.Collections.Generic
open System.IO
open System.Reflection
open Microsoft.FSharp.Core.CompilerServices
open Microsoft.FSharp.Quotations
open ProviderImplementation.ProvidedTypes
open StaticQL
open StaticQL.Provider.TypeGeneration

[<TypeProvider>]
type public Provider(cfg : TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces()

    // Get the assembly and namespace used to house the provided types.
    let thisAssembly = Assembly.LoadFrom(cfg.RuntimeAssembly)
    let tmpAssembly = ProvidedAssembly(Path.GetTempFileName())
    let rootNamespace = "StaticQL.Provider"
    let staticParams =
        [   ProvidedStaticParameter("sql", typeof<string>)
            ProvidedStaticParameter("model", typeof<string>, "")
        ]
    let sqlTy =
        ProvidedTypeDefinition(thisAssembly, rootNamespace, "SQL", Some typeof<obj>, IsErased = false)

    let modelCache = new UserModelCache()

    let buildSQLFromStaticParameters typeName (parameterValues : obj array) =
        match parameterValues with 
        | [| :? string as sql; :? string as model |] ->
            let model = modelCache.Load(cfg.ResolutionFolder, model)
            let parsed = CommandEffect.OfSQL(model.Model, typeName, sql)
            let ty =
                {   Assembly = thisAssembly
                    Namespace = rootNamespace
                    TypeName = typeName
                    UserModel = model
                    Command = parsed
                } |> generateType
            tmpAssembly.AddTypes([ty])
            ty
        | _ -> failwith "Invalid parameters (expected 2 strings: sql, model)"
    do
        sqlTy.DefineStaticParameters(staticParams, buildSQLFromStaticParameters)
        tmpAssembly.AddTypes([ sqlTy ])
        this.AddNamespace(rootNamespace, [ sqlTy ])
        modelCache.Invalidated.Add(fun _ -> this.Invalidate())
        this.Disposing.Add(fun _ -> modelCache.Dispose())

[<TypeProviderAssembly>]
do ()