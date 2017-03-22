namespace Rezoom.SQL.Provider
open System.IO
open System.Reflection
open Microsoft.FSharp.Core.CompilerServices
open ProviderImplementation.ProvidedTypes
open Rezoom.SQL.Provider.TypeGeneration

[<TypeProvider>]
type public Provider(cfg : TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces()

    // Get the assembly and namespace used to house the provided types.
    let thisAssembly = Assembly.LoadFrom(cfg.RuntimeAssembly)
    let tmpAssembly = ProvidedAssembly(Path.GetTempFileName())
    let rootNamespace = "Rezoom.SQL"

    let modelCache = new UserModelCache()
    let generateType typeName model case =
        let tmpAssembly = ProvidedAssembly(Path.GetTempFileName())
        let model = modelCache.Load(cfg.ResolutionFolder, model)
        let ty =
            {   Assembly = thisAssembly
                Namespace = rootNamespace
                TypeName = typeName
                UserModel = model
                Case = case
            } |> generateType
        tmpAssembly.AddTypes([ ty ])
        ty

    let sqlTy =
        let sqlTy =
            ProvidedTypeDefinition(thisAssembly, rootNamespace, "SQL", Some typeof<obj>, IsErased = false)
        let staticParams =
            [   ProvidedStaticParameter("sql", typeof<string>)
                ProvidedStaticParameter("model", typeof<string>, "")
            ]
        let buildSQLFromStaticParams typeName (parameterValues : obj array) =
            match parameterValues with 
            | [| :? string as sql; :? string as model |] -> generateType typeName model (GenerateSQL sql)
            | _ -> failwith "Invalid parameters (expected 2 strings: sql, model)"
        sqlTy.DefineStaticParameters(staticParams, buildSQLFromStaticParams)
        sqlTy

    let modelTy =
        let modelTy =
            ProvidedTypeDefinition(thisAssembly, rootNamespace, "SQLModel", Some typeof<obj>, IsErased = false)
        let staticParams = [ ProvidedStaticParameter("model", typeof<string>, "") ]
        let buildModelFromStaticParams typeName (parameterValues : obj array) =
            match parameterValues with
            | [| :? string as model |] -> generateType typeName model GenerateModel
            | _ -> failwith "Invalid parameters (expected 1 string: model)"
        modelTy.DefineStaticParameters(staticParams, buildModelFromStaticParams)
        modelTy

    do
        let tys = [ sqlTy; modelTy ]
        tmpAssembly.AddTypes(tys)
        this.AddNamespace(rootNamespace, tys)
        modelCache.Invalidated.Add(fun _ -> this.Invalidate())
        this.Disposing.Add(fun _ -> modelCache.Dispose())

    static do
        System.AppDomain.CurrentDomain.add_AssemblyResolve(fun _ args ->
            AssemblyResolver.resolve args.Name |> Option.toObj)

[<TypeProviderAssembly>]
do ()