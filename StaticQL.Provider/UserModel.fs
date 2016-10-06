namespace StaticQL.Provider
open System
open System.IO
open System.Text.RegularExpressions
open System.Collections.Generic
open StaticQL
open StaticQL.Provider.Migrations

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
            [| for path in Directory.GetFiles(migrationsFolder, "*.SQL", SearchOption.AllDirectories) do
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

open UserModelLoader

type UserModel =
    {   ModelPath : string
        Backend : IBackend
        Model : Model
        Migrations : string MigrationMajorVersion IReadOnlyList
    }
    static member ConfigFileName = "dbconfig.json"
    static member Load(resolutionFolder : string, modelPath : string) =
        let modelPath = Path.Combine(resolutionFolder, modelPath) // TODO search for config file
        let migrations = loadMigrations modelPath
        let backend = SQLiteBackend() :> IBackend // TODO choose backend based on config file
        let migrations, model = nextModel backend.InitialModel migrations
        let migrations = migrations |> Seq.map (stringizeMajorVersion backend) |> toReadOnlyList
        {   ModelPath = modelPath
            Backend = backend
            Model = model
            Migrations = migrations
        }