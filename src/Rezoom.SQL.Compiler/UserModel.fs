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
        for path in Directory.GetFiles(migrationsFolder, "*.sql") do
            let fileName = Path.GetFileName(path)
            match parseMigrationInfo fileName with
            | None ->
                if sortOfMigrationPattern.IsMatch(fileName) then
                    fail <| Error.migrationFileNameWrong fileName
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

    let foldMigrations
        (folder : bool -> Model -> Model -> 's1 Migration -> 's2 * Model * Model)
        (initialModel : Model)
        (migrationTrees : 's1 MigrationTree seq) =
        let mutable totalModel = initialModel
        let rec mapFold isRoot parentModel tree =
            let s2, childModel, newTotalModel = folder isRoot parentModel totalModel tree.Node
            totalModel <- newTotalModel
            {   Node =
                    {   MajorVersion = tree.Node.MajorVersion
                        Name = tree.Node.Name
                        Source = s2
                    }
                Children = tree.Children |> Seq.map (mapFold false childModel) |> ResizeArray
            }
        let trees = [ for tree in migrationTrees -> mapFold true totalModel tree ]
        trees, totalModel

    let nextModel initialModel (migrationTrees : TotalStmts MigrationTree seq) =
        let folder isRoot (parentModel : Model) (totalModel : Model) (migration : TotalStmts Migration) =
            let totalEffect = CommandEffect.OfSQL(totalModel, migration.Source)
            if not isRoot && totalEffect.DestructiveUpdates.Value then
                fail <| Error.minorMigrationContainsDestruction migration.MigrationName
            let childModel =
                CommandEffect.OfSQL(parentModel, migration.Source).ModelChange |? parentModel
            let totalModel =
                totalEffect.ModelChange |? totalModel
            totalEffect.Statements, childModel, totalModel
        let _, finalModel as pair = foldMigrations folder initialModel migrationTrees
        revalidateViews finalModel
        pair

    let stringizeMigrationTree (backend : IBackend) (migrationTrees : TTotalStmt IReadOnlyList MigrationTree seq) =
        let rec stringize tree =
            let indexer =
                { new IParameterIndexer with
                    member __.ParameterIndex(par) =
                        fail <| Error.migrationContainsParameter tree.Node.MigrationName
                }
            {   Node =
                    {   MajorVersion = tree.Node.MajorVersion
                        Name = tree.Node.Name
                        Source = backend.ToCommandFragments(indexer, tree.Node.Source) |> CommandFragment.Stringize
                    }
                Children =
                    tree.Children |> Seq.map stringize |> ResizeArray
            }
        seq {
            for tree in migrationTrees -> stringize tree
        }

    let tableIds (model : Model) =
        seq {
            let mutable i = 0
            for KeyValue(_, schema) in model.Schemas do
            if schema.SchemaName = model.TemporarySchema then () else
            for KeyValue(_, obj) in schema.Objects do
                match obj with
                | SchemaTable tbl ->
                    yield tbl.Name, i
                    i <- i + 1
                | _ -> ()
        } |> Map.ofSeq

open UserModelLoader

[<NoComparison>]
[<NoEquality>]
type UserModel =
    {   ConnectionName : string
        ConfigDirectory : string
        Config : Config.Config
        MigrationsDirectory : string
        Backend : IBackend
        Model : Model
        TableIds : Map<QualifiedObjectName, int> Lazy
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