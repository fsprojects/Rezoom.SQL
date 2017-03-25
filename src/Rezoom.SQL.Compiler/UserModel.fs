namespace Rezoom.SQL.Compiler
open System
open System.IO
open System.Text.RegularExpressions
open System.Collections.Generic
open Rezoom.SQL.Compiler
open Rezoom.SQL.Compiler.InferredTypes
open Rezoom.SQL.Mapping
open Rezoom.SQL.Migrations

module private UserModelLoader =
    let private sortOfMigrationPattern =
        """
            ^V[0-9]+\.
        """ |> fun pat -> Regex(pat, RegexOptions.IgnoreCase ||| RegexOptions.IgnorePatternWhitespace)
    let private migrationPattern =
        """
            ^V(?<majorVersion> [0-9]+ )
            \.
            (?<name> [a-z0-9_]+ )
            ( - (?<name2> [a-z0-9_]+ ))?
            \.SQL$
        """ |> fun pat -> Regex(pat, RegexOptions.IgnoreCase ||| RegexOptions.IgnorePatternWhitespace)

    let parseMigrationInfo path =
        let rematch = migrationPattern.Match(path)
        if not rematch.Success then None else
        let majorVersion = rematch.Groups.["majorVersion"].Value |> int
        let name = rematch.Groups.["name"].Value
        let name2 =
            let group = rematch.Groups.["name2"]
            if group.Success then Some group.Value
            else None
        Some <|
        match name2 with
        | Some target ->
            {   ParentName = Some name
                Name = target
                MajorVersion = majorVersion
            }
        | None ->
            {   ParentName = None
                Name = name
                MajorVersion = majorVersion
            }

    let loadMigrations migrationsFolder =
        let builder = MigrationTreeListBuilder()
        for path in Directory.GetFiles(migrationsFolder, "*.sql", SearchOption.AllDirectories) do
            let fileName = Path.GetFileName(path)
            match parseMigrationInfo fileName with
            | None ->
                if sortOfMigrationPattern.IsMatch(fileName) then
                    failwithf "%s seems like it's supposed to be a migration, but is not well formed" fileName
            | Some migrationName ->
                let text = File.ReadAllText(path)
                let parsed = CommandEffect.ParseSQL(path, text)
                builder.Add(migrationName, parsed)
        builder.ToTrees()

    let revalidateViews (model : Model) =
        let inference = TypeInferenceContext()
        let typeChecker = TypeChecker(inference, InferredSelectScope.Root(model))
        let concrete = concreteMapping inference
        for KeyValue(_, schema) in model.Schemas do
            for KeyValue(_, obj) in schema.Objects do
                match obj with
                | SchemaView view ->
                    let inferredDefinition = typeChecker.Select(view.CreateDefinition.AsSelect, SelfQueryShape.Unknown)
                    ignore <| concrete.Select(inferredDefinition)
                | _ -> ()

    let nextModel initialModel (migrationTrees : TotalStmts MigrationTree seq) =
        let folder isRoot (model : Model) (migration : TotalStmts Migration) =
            let effect = CommandEffect.OfSQL(model, migration.Source)
            if not isRoot && effect.DestructiveUpdates.Value then
                failwith <| sprintf
                    "The migration ``%s`` contains destructive statements. This requires a version bump."
                    migration.MigrationName
            effect.Statements, effect.ModelChange |? model
        let _, finalModel as pair = MigrationUtilities.foldMigrations folder initialModel migrationTrees
        revalidateViews finalModel
        pair

    let stringizeMigrationTree (backend : IBackend) (migrationTrees : TTotalStmt IReadOnlyList MigrationTree seq) =
        seq {
            let indexer =
                { new IParameterIndexer with
                    member __.ParameterIndex(par) =
                        failwith "Migrations cannot be parameterized"
                }
            for tree in migrationTrees ->
                tree.Map(fun stmts ->
                    backend.ToCommandFragments(indexer, stmts) |> CommandFragment.Stringize)
        }

    let tableIds (model : Model) =
        seq {
            let mutable i = 0
            for KeyValue(_, schema) in model.Schemas do
            for KeyValue(_, obj) in schema.Objects do
                match obj with
                | SchemaTable tbl ->
                    yield (tbl.SchemaName, tbl.TableName), i
                    i <- i + 1
                | _ -> ()
        } |> Map.ofSeq

open UserModelLoader

type UserModel =
    {   ConnectionName : string
        ConfigDirectory : string
        Config : Config.Config
        MigrationsDirectory : string
        Backend : IBackend
        Model : Model
        TableIds : Map<Name * Name, int> Lazy
        Migrations : string MigrationTree IReadOnlyList
    }
    static member ConfigFileName = "rzsql.json"
    static member Load(resolutionFolder : string, modelPath : string) =
        let config, configDirectory =
            if String.IsNullOrEmpty(modelPath) then // implicit based on location of dbconfig.json
                let configPath =
                    Directory.GetFiles(resolutionFolder, "*.json", SearchOption.AllDirectories)
                    |> Array.tryFind (fun f -> f.EndsWith(UserModel.ConfigFileName, StringComparison.OrdinalIgnoreCase))
                match configPath with
                | None -> Config.defaultConfig, resolutionFolder
                | Some path ->
                    Config.parseConfigFile path, Path.GetDirectoryName(path)
            else
                let path = Path.Combine(resolutionFolder, modelPath)
                if path.EndsWith(".json", StringComparison.OrdinalIgnoreCase) then
                    Config.parseConfigFile path, Path.GetDirectoryName(path)
                else
                    let configPath = Path.Combine(path, UserModel.ConfigFileName)
                    if File.Exists(configPath) then
                        Config.parseConfigFile configPath, path
                    else
                        Config.defaultConfig, path
        let migrationsDirectory = Path.Combine(configDirectory, config.MigrationsPath) |> Path.GetFullPath
        let migrations = loadMigrations migrationsDirectory
        let backend = config.Backend.ToBackend()
        let migrations, model = nextModel backend.InitialModel migrations
        let migrations = stringizeMigrationTree backend migrations |> toReadOnlyList
        {   ConnectionName = config.ConnectionName
            MigrationsDirectory = migrationsDirectory
            ConfigDirectory = Path.GetFullPath(configDirectory)
            Config = config
            Backend = backend
            Model = model
            TableIds = lazy tableIds model
            Migrations = migrations
        }