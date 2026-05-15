namespace Rezoom.SQL.Provider
open System.IO
open System.Reflection
open System.Runtime.Loader
open Microsoft.FSharp.Core.CompilerServices
open ProviderImplementation.ProvidedTypes
open Rezoom.SQL.Provider.TypeGeneration
open System

[<TypeProvider>]
type public Provider(cfg : TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces(cfg)

    // The TP runs inside fsc's isolated AssemblyLoadContext on .NET Core/5+. The base
    // class's AppDomain.AssemblyResolve hook loads bytes into the DEFAULT context, but
    // the calling code lives in this TP's ALC, so the load doesn't satisfy the request.
    // Hook the TP's own ALC and load deps into it from cfg.ReferencedAssemblies.
    do
        let thisAlc = AssemblyLoadContext.GetLoadContext(typeof<Provider>.Assembly)
        if not (isNull thisAlc) then
            let byName =
                cfg.ReferencedAssemblies
                |> Array.choose (fun p ->
                    try Some (Path.GetFileNameWithoutExtension(p), p)
                    with _ -> None)
                |> Map.ofArray
            thisAlc.add_Resolving(fun _ name ->
                match Map.tryFind name.Name byName with
                | Some path when File.Exists(path) -> thisAlc.LoadFromAssemblyPath(path)
                | _ -> null)

    // Get the assembly and namespace used to house the provided types.
    let thisAssembly = Assembly.GetExecutingAssembly ()
    let rootNamespace = "Rezoom.SQL"

    // IMPORTANT: build `assemblies` BEFORE constructing anything (like UserModelCache)
    // that might trigger a CLR load of a referenced runtime assembly. ResolveAssembly
    // is hooked up by the base ctor and falls back to this list — if it's still null
    // when the load happens, the TP fails with a confusing "operation is not legal"
    // FileNotFoundException.
    let assemblies =
      let alts =
        [ Path.DirectorySeparatorChar
          Path.AltDirectorySeparatorChar ]
        |> List.map (fun x -> sprintf "%cref%c" x x, sprintf "%clib%c" x x)
      cfg.ReferencedAssemblies
      |> Seq.choose (fun asm ->
        try asm |> (File.ReadAllBytes >> Assembly.Load >> Some)
        with
        | :? BadImageFormatException as e ->
          //hack to point to the lib dir if it is using ref
          let file =
            alts
            |> List.tryFind (fun (l, _) -> asm.IndexOf(l, StringComparison.OrdinalIgnoreCase) > -1)
            |> Option.map (fun (l, r) -> asm.Replace(l, r))
            |> Option.filter System.IO.File.Exists
          match file with
          | None ->
            None
          | Some file ->
            try file |> (File.ReadAllBytes >> Assembly.Load >> Some)
            with | e ->
              None
        | _ ->
          None)
      |> Array.ofSeq

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

    do
        let tys = [ sqlTy; modelTy ]
        this.AddNamespace(rootNamespace, tys)
        modelCache.Invalidated.Add(fun _ -> this.Invalidate())
        this.Disposing.Add(fun _ -> modelCache.Dispose())

    override __.ResolveAssembly args =   
        let name = AssemblyName args.Name
        let existingAssembly =
            System.AppDomain.CurrentDomain.GetAssemblies ()
            |> Seq.tryFind (fun x -> AssemblyName.ReferenceMatchesDefinition (name, x.GetName()))
            |> function
              | None -> 
                assemblies
                |> Seq.tryFind (fun x -> AssemblyName.ReferenceMatchesDefinition (name, x.GetName()))
              | x -> x
        match existingAssembly with
        | Some x -> x
        | None ->
              match AssemblyResolver.resolve args.Name with
              | Some x -> x
              | _ -> base.ResolveAssembly args

[<TypeProviderAssembly>]
do ()