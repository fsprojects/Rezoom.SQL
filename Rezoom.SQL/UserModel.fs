namespace Rezoom.SQL
open System
open System.IO
open System.Text.RegularExpressions
open System.Collections.Generic
open Rezoom.SQL
open Rezoom.SQL.Migrations

module private UserModelLoader =
    let private migrationPattern =
        """
            \bV(?<majorVersion>
                [0-9]+
            )
            (?<featurePart>             # feature part is optional: implicitly ""/0 if left out
                [-_/\\]                 # separator can be a filename character like -, or a directory
                (?<featureName>
                    [^-_/\\]+           # any non-separator character is allowed in the feature name
                )
                (?<featureVersionPart>  # feature version is optional: implicitly 0 if left out
                    [-_/\\]
                    (?<featureVersion>
                        [0-9]+
                    )
                )?
            )?
            \.SQL$
        """ |> fun pat -> Regex(pat, RegexOptions.IgnoreCase ||| RegexOptions.IgnorePatternWhitespace)

    let parseMigrationInfo path =
        let rematch = migrationPattern.Match(path)
        if not rematch.Success then None else
        let majorVersion = rematch.Groups.["majorVersion"].Value |> int
        let featureName, featureVersion =
            if not rematch.Groups.["featurePart"].Success then "", 0
            else
                rematch.Groups.["featureName"].Value,
                    if not rematch.Groups.["featureVersionPart"].Success then 0
                    else rematch.Groups.["featureVersion"].Value |> int
        Some (majorVersion, featureName, featureVersion)

    let loadMigrations migrationsFolder =
        let migrations =
            [| for path in Directory.GetFiles(migrationsFolder, "*.sql", SearchOption.AllDirectories) do
                match parseMigrationInfo path with
                | None -> ()
                | Some (_, _, featureVersion as migrationInfo) ->
                    let text = File.ReadAllText(path)
                    let parsed = CommandEffect.ParseSQL(path, text)
                    yield
                        {   SourceFileName = path
                            FeatureVersion = featureVersion
                            Command = parsed
                        }, migrationInfo
            |] |> Array.sortBy snd
        let groups = migrations |> Seq.groupBy (function _, (major, _, _) -> major)
        [| for majorVersion, migrations in groups ->
            let features =
                let groups = migrations |> Seq.groupBy (function _, (_, feature, _) -> feature)
                [| for featureName, migrations in groups ->
                    {   FeatureName = Name(featureName)
                        Migrations = migrations |> Seq.map fst |> toReadOnlyList
                    }
                |]
            {   MajorVersion = majorVersion
                Features = features
            }
        |]

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
        MigrationsDirectory : string
        Backend : IBackend
        Model : Model
        Migrations : string MigrationMajorVersion IReadOnlyList
        TableIds : Map<Name * Name, int> Lazy
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
        let migrations = migrations |> Seq.map (stringizeMajorVersion backend) |> toReadOnlyList
        {   ConnectionName = config.ConnectionName
            MigrationsDirectory = migrationsDirectory
            ConfigDirectory = Path.GetFullPath(configDirectory)
            Backend = backend
            Model = model
            Migrations = migrations
            TableIds = lazy tableIds model
        }