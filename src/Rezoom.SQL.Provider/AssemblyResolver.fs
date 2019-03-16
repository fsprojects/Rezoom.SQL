module private Rezoom.SQL.Provider.AssemblyResolver
open System
open System.IO
open System.Reflection

let private log msg =
#if DEBUG
    let home = Environment.GetFolderPath(Environment.SpecialFolder.UserProfile)
    let log = Path.Combine(home, "rzsqlproviderassembly_log.txt")
    File.AppendAllLines(log, [msg])
#else
    ()
#endif

let rec private walkUp parents path =
    if String.IsNullOrEmpty(path) then None else
    match parents with
    | [] -> Some path
    | immediate :: ancestors ->
        let up = Path.GetDirectoryName(path)
        match immediate with
        | None ->
            walkUp ancestors up
        | Some expected when Path.GetFileName(up).Equals(expected, StringComparison.OrdinalIgnoreCase) ->
            walkUp ancestors up
        | Some _ ->
            None

let private directoriesToSearch =
    seq {
        // we expect our own assembly to be in a NuGet packages folder, with nuget packages under
        // the solution folder / packages.
        let dllPath = Path.GetFullPath(Assembly.GetExecutingAssembly().Location)
        //log (sprintf "dll path = %s" dllPath)
        match dllPath |> walkUp [Some "net45"; Some "lib"; None; Some "packages"] with
        | None -> ()
        | Some packages ->
            for dir in Directory.GetDirectories(packages) do
                let lib = Path.Combine(dir, "lib")
                if Directory.Exists(lib) then
                    for framework in Directory.GetDirectories(lib) -> framework
    } |> Seq.cache

let private versionNumber (v : Version) = v.Major, v.Minor, v.Build, v.Revision

let private compatibleWith (searching : AssemblyName) (found : AssemblyName) =
    searching.Name = found.Name
    && (isNull searching.Version
        || isNull found.Version
        || searching.Version.Major = found.Version.Major
            && versionNumber searching.Version <= versionNumber found.Version)

/// We only do hacky assembly resolution for these dependencies, which we expect to find
/// in the NuGet packages folder.
let private nameWhitelist =
    [|  "FParsec"
        "FParsecCS"
        "FParsec-Pipes"
        "LicenseToCIL"
        "Rezoom"
    |] |> Set.ofArray
let resolve (name : string) =
    //log (sprintf "resolving %s" name)
    //log (sprintf "in %s" Environment.CurrentDirectory)
    if name.IndexOf(".resources", StringComparison.OrdinalIgnoreCase) >= 0 then
        //log (sprintf "ignoring %s because resources" name)
        None
    else
    let searchingName = AssemblyName(name)
    if not (nameWhitelist |> Set.contains searchingName.Name) then
        log (sprintf "ignoring %s because whitelist" name)
        None
    else
    let alreadyLoaded = 
        AppDomain.CurrentDomain.GetAssemblies()
        |> Seq.tryFind (fun a -> a.GetName() |> compatibleWith searchingName)
    match alreadyLoaded with
    | Some alreadyLoaded ->
        Some alreadyLoaded
    | None ->
        let dllName = searchingName.Name + ".dll"
        let matched =
            seq {
                for dir in directoriesToSearch do
                    log (sprintf "searching %s" dir)
                    let assemblyPath = Path.Combine(dir, dllName)
                    if File.Exists(assemblyPath) then
                        log (sprintf "checking %s" assemblyPath)
                        let foundName = AssemblyName.GetAssemblyName(assemblyPath)
                        if foundName |> compatibleWith searchingName then
                            log (sprintf "found %s" assemblyPath)
                            yield assemblyPath
            } |> Seq.tryHead
        matched 
        |> Option.map Assembly.LoadFile

