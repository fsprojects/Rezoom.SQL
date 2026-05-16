namespace Rezoom.SQL.Provider
open System.IO
open System.Reflection
open Microsoft.FSharp.Core.CompilerServices
open ProviderImplementation.ProvidedTypes
open Rezoom.SQL.Provider.TypeGeneration
open System

/// Static state + resolver used from Provider.ResolveAssembly so the override never
/// touches `this`. F#'s `as this` machinery wraps every instance field/member access
/// inside a class with a "has the ctor finished?" guard that throws FailInit if it
/// hasn't. That guard fires when the runtime calls our ResolveAssembly during ctor
/// (which is exactly what happens — the base class hooks AppDomain.AssemblyResolve in
/// its own ctor, so any later assembly load during *our* ctor re-enters us). The
/// CLR wraps the InvalidOperationException as "An operation is not legal in the
/// current state. (HRESULT: 0x80131509)", reported as if the originally-requested
/// assembly was the problem.
[<RequireQualifiedAccess>]
module private TpResolver =
    // Debug-only diagnostics. The TP runs inside fsc / IDE tooling; there's no console
    // to write to, so a file is the only practical channel. Release builds are silent
    // — invaluable when iterating on TP plumbing, intolerable in CI.
    let plog (s : string) =
#if DEBUG
        try
            let path = Path.Combine(Path.GetTempPath(), "rezoom-sql-tp.log")
            File.AppendAllText(path,
                sprintf "[%s pid=%d] %s%s"
                    (DateTime.Now.ToString("HH:mm:ss.fff"))
                    (System.Diagnostics.Process.GetCurrentProcess().Id)
                    s System.Environment.NewLine)
        with _ -> ()
#else
        ignore s
#endif

    let mutable byName : Map<string, string> = Map.empty

    let resolve (assemblyFullName : string) : Assembly =
        try
            let n = AssemblyName(assemblyFullName)
            // Prefer an assembly the AppDomain already has loaded, then fall back to
            // a file-system load from cfg.ReferencedAssemblies (cached as byName).
            let inDomain =
                System.AppDomain.CurrentDomain.GetAssemblies()
                |> Array.tryFind (fun a ->
                    AssemblyName.ReferenceMatchesDefinition(n, a.GetName()))
            match inDomain with
            | Some a ->
                plog (sprintf "resolve '%s' -> already loaded: %s" n.Name a.FullName)
                a
            | None ->
                match Map.tryFind n.Name byName with
                | Some path when File.Exists(path) ->
                    let a = Assembly.LoadFrom(path)
                    plog (sprintf "resolve '%s' -> LoadFrom %s" n.Name path)
                    a
                | _ ->
                    plog (sprintf "resolve '%s' -> not in byName, deferring" n.Name)
                    null
        with ex ->
            plog (sprintf "resolve '%s' -> threw %s" assemblyFullName ex.Message)
            null


[<TypeProvider>]
type public Provider(cfg : TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces(cfg)

    // *** STATIC INITIALIZER ***
    // Runs ONCE when the Provider class is first loaded, *before* any instance ctor
    // and before the JIT compiles the instance ctor body. Critical for breaking the
    // chicken-and-egg: the JIT of the instance ctor itself eagerly resolves types
    // referenced anywhere in the ctor (e.g. UserModelCache, whose field types
    // transitively reference Rezoom.SQL.Compiler.UserModel). That resolution fires
    // AppDomain.AssemblyResolve. If we wait until the instance ctor to install a
    // handler, it's already too late — the JIT load has been attempted.
    //
    // Install an AppDomain-wide handler that resolves Rezoom.SQL.* by walking the
    // nuget packages cache adjacent to *this* assembly's own folder. This needs no
    // cfg, no `this`, and no F# class fields, so FailInit can't fire on it.
    static let staticResolverInstalled =
        let onNetFx =
            try
                System.Runtime.InteropServices.RuntimeInformation.FrameworkDescription
                    .StartsWith(".NET Framework", StringComparison.OrdinalIgnoreCase)
            with _ -> false
        // If we're a netfx host and we were handed a path under a .NET Core ref pack or
        // a net8.0/net9.0/etc. lib folder, try to redirect to a netstandard2.0 sibling.
        // The ref-pack assemblies are reference-only and can't be loaded; the net8.0
        // libs reference System.Runtime 8.0 which doesn't bind on netfx.
        let redirectForNetFx (path : string) : string option =
            if not onNetFx then Some path
            elif String.IsNullOrEmpty(path) then None
            elif path.IndexOf(@"\packs\Microsoft.NETCore.App.Ref\", StringComparison.OrdinalIgnoreCase) >= 0
                 || path.IndexOf(@"\ref\", StringComparison.OrdinalIgnoreCase) >= 0 then
                // Pure reference assembly. Don't try to load it; let CLR's normal binding
                // find the runtime version in mscorlib / netfx BCL.
                None
            else
                // Replace e.g. "\lib\net8.0\" with "\lib\netstandard2.0\" if available.
                let m = System.Text.RegularExpressions.Regex.Match(path, @"\\lib\\net(coreapp)?[0-9]+\.[0-9]+\\", System.Text.RegularExpressions.RegexOptions.IgnoreCase)
                if m.Success then
                    let ns20 = path.Substring(0, m.Index) + @"\lib\netstandard2.0\" + path.Substring(m.Index + m.Length)
                    if File.Exists(ns20) then Some ns20 else Some path
                else
                    Some path
        let probeNugetSibling (asmName : AssemblyName) : Assembly =
            try
                let myLoc = typeof<Provider>.Assembly.Location
                if String.IsNullOrEmpty(myLoc) then null else
                // Walk up from .../<pkg>/<ver>/lib/<tfm>/Provider.dll to .../packages/
                let tfmDir = Path.GetDirectoryName(myLoc)
                let libDir = Path.GetDirectoryName(tfmDir)
                let verDir = Path.GetDirectoryName(libDir)
                let pkgDir = Path.GetDirectoryName(verDir)
                let packagesRoot = Path.GetDirectoryName(pkgDir)
                if String.IsNullOrEmpty(packagesRoot) then null else
                let pkgDir = Path.Combine(packagesRoot, asmName.Name.ToLowerInvariant())
                if not (Directory.Exists(pkgDir)) then null else
                // Pick the highest version directory available.
                let versions =
                    Directory.GetDirectories(pkgDir)
                    |> Array.sortDescending
                let probe =
                    versions
                    |> Array.tryPick (fun v ->
                        let libRoot = Path.Combine(v, "lib")
                        if not (Directory.Exists(libRoot)) then None else
                        // On netfx prefer netstandard2.0; otherwise sort descending.
                        let tfms =
                            Directory.GetDirectories(libRoot)
                            |> (if onNetFx then
                                    Array.sortBy (fun d ->
                                        if Path.GetFileName(d).Equals("netstandard2.0", StringComparison.OrdinalIgnoreCase) then 0
                                        elif Path.GetFileName(d).StartsWith("netstandard", StringComparison.OrdinalIgnoreCase) then 1
                                        else 2)
                                else Array.sortDescending)
                        tfms
                        |> Array.tryPick (fun tfm ->
                            let dll = Path.Combine(tfm, asmName.Name + ".dll")
                            if File.Exists(dll) then Some dll else None))
                match probe with
                | Some dll -> Assembly.LoadFrom(dll)
                | None -> null
            with _ -> null
        let handler = ResolveEventHandler(fun _ args ->
            try
                TpResolver.plog (sprintf "[static handler] resolve '%s'" args.Name)
                let n = AssemblyName(args.Name)
                // Prefer already-loaded.
                let inDomain =
                    System.AppDomain.CurrentDomain.GetAssemblies()
                    |> Array.tryFind (fun a ->
                        AssemblyName.ReferenceMatchesDefinition(n, a.GetName()))
                match inDomain with
                | Some a ->
                    TpResolver.plog (sprintf "  -> already loaded: %s" a.FullName)
                    a
                | None ->
                    // First try the cfg-derived map (populated by the first instance
                    // ctor; empty during early JIT). Apply netfx redirects.
                    let byNamePath =
                        match Map.tryFind n.Name TpResolver.byName with
                        | Some p -> redirectForNetFx p
                        | None -> None
                    match byNamePath with
                    | Some path when File.Exists(path) ->
                        let a = Assembly.LoadFrom(path)
                        TpResolver.plog (sprintf "  -> LoadFrom (byName) %s" path)
                        a
                    | _ ->
                        // Fall back to walking the nuget cache (probe prefers
                        // netstandard2.0 on netfx).
                        let a = probeNugetSibling n
                        if isNull a then
                            TpResolver.plog "  -> not found"
                        else
                            TpResolver.plog (sprintf "  -> found via nuget probe: %s" a.Location)
                        a
            with ex ->
                TpResolver.plog (sprintf "  -> static handler threw: %s" ex.Message)
                null)
        AppDomain.CurrentDomain.add_AssemblyResolve(handler)
        TpResolver.plog "static AppDomain.AssemblyResolve handler installed"
        true

    // First let-binding: populate the path map. The do-block right below registers
    // it into the static TpResolver. Note we still do this so subsequent loads
    // (post-ctor) prefer the cfg-supplied paths over filesystem probing.
    let assembliesByName : Map<string, string> =
        cfg.ReferencedAssemblies
        |> Array.choose (fun p ->
            try Some (Path.GetFileNameWithoutExtension(p), p)
            with _ -> None)
        |> Map.ofArray

    do
        TpResolver.byName <- assembliesByName
        TpResolver.plog (sprintf "==== Provider ctor (resolver installed=%b) ==== Framework=%s RefCount=%d Resolution=%s Runtime=%s"
                            staticResolverInstalled
                            System.Runtime.InteropServices.RuntimeInformation.FrameworkDescription
                            cfg.ReferencedAssemblies.Length
                            cfg.ResolutionFolder
                            cfg.RuntimeAssembly)

    let thisAssembly = Assembly.GetExecutingAssembly ()
    let rootNamespace = "Rezoom.SQL"

    // On .NET Core/5+ fsc loads type providers in isolated AssemblyLoadContexts; the
    // base class's AppDomain.AssemblyResolve hook loads bytes into the DEFAULT ALC,
    // which the TP's ALC can't see. Hook the TP's own ALC's Resolving event when
    // available. But this is irrelevant on .NET Framework — and we can't even probe
    // for the System.Runtime.Loader type by name there, because Type.GetType triggers
    // AssemblyResolve. Check FrameworkDescription (a pure string lookup) first.
    do
        let isNetCore =
            try
                System.Runtime.InteropServices.RuntimeInformation.FrameworkDescription
                    .StartsWith(".NET Framework", StringComparison.OrdinalIgnoreCase) |> not
            with _ -> false
        if isNetCore then
            try
                let alcType =
                    Type.GetType("System.Runtime.Loader.AssemblyLoadContext, System.Runtime.Loader", false)
                if not (isNull alcType) then
                    let getLoadContext = alcType.GetMethod("GetLoadContext", [| typeof<Assembly> |])
                    let alc = getLoadContext.Invoke(null, [| box (typeof<Provider>.Assembly) |])
                    if not (isNull alc) then
                        let loadFromPath = alcType.GetMethod("LoadFromAssemblyPath", [| typeof<string> |])
                        let byName = assembliesByName
                        let resolve =
                            Func<AssemblyName, Assembly>(fun name ->
                                match Map.tryFind name.Name byName with
                                | Some path when File.Exists(path) ->
                                    loadFromPath.Invoke(alc, [| box path |]) :?> Assembly
                                | _ -> null)
                        // Event signature is Func<AssemblyLoadContext, AssemblyName, Assembly?>;
                        // build the delegate via Linq.Expressions so AssemblyLoadContext only
                        // enters via `alcType` (a runtime Type), never as a static type name.
                        let resolvingEvent = alcType.GetEvent("Resolving")
                        let handlerType = resolvingEvent.EventHandlerType
                        let alcParam = System.Linq.Expressions.Expression.Parameter(alcType, "_alc")
                        let nameParam = System.Linq.Expressions.Expression.Parameter(typeof<AssemblyName>, "name")
                        let invoke = resolve.GetType().GetMethod("Invoke")
                        let body =
                            System.Linq.Expressions.Expression.Call(
                                System.Linq.Expressions.Expression.Constant(resolve), invoke, nameParam)
                        let lambda =
                            System.Linq.Expressions.Expression.Lambda(handlerType, body, alcParam, nameParam)
                        let del = lambda.Compile()
                        resolvingEvent.AddEventHandler(alc, del)
            with _ -> ()

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

    // ResolveAssembly is overridden to route through TpResolver — a static module.
    // No `this.<field>` access, no member calls, no `base.<member>`. That avoids
    // F#'s FailInit guard during the constructor.
    override __.ResolveAssembly args = TpResolver.resolve args.Name

[<TypeProviderAssembly>]
do ()
