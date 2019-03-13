namespace Rezoom.SQL.Provider
open System.IO
open System.Reflection
open Microsoft.FSharp.Core.CompilerServices
open ProviderImplementation.ProvidedTypes
open Rezoom.SQL.Provider.TypeGeneration

[<TypeProvider>]
type public Provider(cfg : TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces(cfg)

    // Get the assembly and namespace used to house the provided types.
    let thisAssembly = Assembly.GetExecutingAssembly () // Assembly.LoadFrom(cfg.RuntimeAssembly)
    //let tmpAssembly = ProvidedAssembly()
    let rootNamespace = "Rezoom.SQL"

    let modelCache = new UserModelCache()
    let generateType typeName model case =
        let tmpAssembly = ProvidedAssembly()
        let model = modelCache.Load(cfg.ResolutionFolder, model)
        let ty =
            {   Assembly = tmpAssembly
                Namespace = rootNamespace
                TypeName = typeName
                UserModel = model
                Case = case
            } |> generateType
        tmpAssembly.AddTypes([ ty ])
        ty

    let sqlTy =
        let sqlTy =
            ProvidedTypeDefinition(thisAssembly, rootNamespace, "SQL", Some typeof<obj>, isErased = false)
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
            ProvidedTypeDefinition(thisAssembly, rootNamespace, "SQLModel", Some typeof<obj>, isErased = false)
        let staticParams = [ ProvidedStaticParameter("model", typeof<string>, "") ]
        let buildModelFromStaticParams typeName (parameterValues : obj array) =
            match parameterValues with
            | [| :? string as model |] -> generateType typeName model GenerateModel
            | _ -> failwith "Invalid parameters (expected 1 string: model)"
        modelTy.DefineStaticParameters(staticParams, buildModelFromStaticParams)
        modelTy

    let assemblies =
      cfg.ReferencedAssemblies
      |> Seq.choose (fun asm ->
        try asm |> (File.ReadAllBytes >> Assembly.Load >> Some)
        with | _ -> None)
      |> Array.ofSeq

    do
        let tys = [ sqlTy; modelTy ]
        //tmpAssembly.AddTypes(tys)
        this.AddNamespace(rootNamespace, tys)
        modelCache.Invalidated.Add(fun _ -> this.Invalidate())
        this.Disposing.Add(fun _ -> modelCache.Dispose())

    override __.ResolveAssembly args =        
        let name = AssemblyName args.Name
        let existingAssembly =
            System.AppDomain.CurrentDomain.GetAssemblies ()
            |> Seq.append assemblies
            |> Seq.tryFind (fun x -> AssemblyName.ReferenceMatchesDefinition (name, x.GetName()))
        match existingAssembly with
        | Some x -> x
        | None ->
          match AssemblyResolver.tryLoadFromLibFolder args.Name with
          | Some x -> x
          | _ ->
              match AssemblyResolver.resolve args.Name with
              | Some x -> x
              | _ -> 
                  AssemblyResolver.log (sprintf "%s> Not found" args.Name)
                  base.ResolveAssembly args

[<TypeProviderAssembly>]
do ()