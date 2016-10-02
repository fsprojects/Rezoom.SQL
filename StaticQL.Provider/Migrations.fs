module StaticQL.Provider.Migrations
open System
open System.Collections.Generic
open FSharp.Reflection
open FSharp.Quotations
open StaticQL
open StaticQL.Mapping

// Migrations have three version parts:
// Major Version - Feature Name - Feature Version
//
// There is no strict ordering of feature names -- so the migrations for Feature A can run before or after
// Feature B. The code in this file validates that each ordering of feature migrations results in the same
// model.
//         
//    +---V2 Feature A.1---V2 Feature A.2----+                       +---V4 Feature A
//   /                                        \                     /
// V1-----V2 Feature B------------------------V2---V3 Feature A---V3-----V4 Feature B.1---V4 Feature B.2
//   \                                        /
//    +---V2 Feature C-----------------------+
//
// Given a validated migration track, here are the rules for running migrations:
// 1. Get the list of migrations that have already been run on the database -- including the order they've been run in.
// 2. Look for the first major version whose migrations have *not* all been run.
// 3. Check if any migrations from a higher major version number have been run. If so, panic!
//    Major versions can't be run out of order.
// 4. If there is a feature within the major version for which some but not all feature versions have been run,
//    run the remaining ones first. If there is more than one such feature, panic!
// 5. Run the remaining feature migrations in no particular order (other than by version *within* each feature).
// 6. Proceed to the next major version and repeat steps 4, 5, and 6.

type Migration<'stmts> =
    {   Command : 'stmts
        FeatureVersion : int
        SourceFileName : string
    }

let private migrationNextModel model (migration : Stmts Migration) =
    let effect = CommandEffect.OfSQL(model, migration.Command)
    if effect.Parameters.Count > 0 then
        failwithf
            "Migrations may not contain parameters: parameter named ``%O`` in %s"
            effect.Parameters.[0]
            migration.SourceFileName
    {   Command = effect.Statements
        FeatureVersion = migration.FeatureVersion
        SourceFileName = migration.SourceFileName
    }
    , effect.ModelChange |? model
        
type MigrationFeature<'stmts> =
    {  
        FeatureName : Name
        Migrations : 'stmts Migration IReadOnlyList
    }

let private featureNextModel model (feature : Stmts MigrationFeature) =
    let mutable model = model
    {   FeatureName = feature.FeatureName
        Migrations =
            seq {
                for migration in feature.Migrations do
                    let migration, newModel = migrationNextModel model migration
                    model <- newModel
                    yield migration
            } |> toReadOnlyList
    }
    , model

type MigrationMajorVersion<'stmts> =
    {   Features : 'stmts MigrationFeature IReadOnlyCollection
        MajorVersion : int
    }

let rec private fac x =
    if x <= 2L then x else x * fac (x - 1L)

let private majorVersionNextModel model (this : Stmts MigrationMajorVersion) =
    let maxFeatures = 6
    if this.Features.Count > maxFeatures then
        failwithf
            "No more than %d features per major version are permitted -- would have to validate %d migration paths"
            maxFeatures (fac (int64 this.Features.Count))
    let mutable commonNextModel = None
    for featureSequence in this.Features |> permutations do
        match commonNextModel with
        | None ->
            let mutable model = model
            let features =
                seq {
                    for feature in featureSequence do
                        let feature, newModel = featureNextModel model feature
                        model <- newModel
                        yield feature
                } |> toReadOnlyList
            commonNextModel <- Some (model, features)
        | Some (commonNextModel, _) ->
            let possibleModel =
                featureSequence |> Seq.fold (fun model feature -> featureNextModel model feature |> snd) model
            if possibleModel <> commonNextModel then
                failwithf "Features of major version %d are interdependent" this.MajorVersion
    {   Features =
            match commonNextModel with
            | None -> [||] :> _ IReadOnlyCollection
            | Some (_, features) -> features :> _ IReadOnlyCollection
        MajorVersion = this.MajorVersion
    }
    , match commonNextModel with | None -> model | Some (m, _) -> m

let nextModel model (majorVersions : Stmts MigrationMajorVersion seq) =
    let mutable model = model
    let majorVersions =
        seq {
            for majorVersion in majorVersions do
                let majorVersion, newModel = majorVersionNextModel model majorVersion
                model <- newModel
                yield majorVersion
        } |> toReadOnlyList
    majorVersions, model

let private mapStmts f (major : 'stmts MigrationMajorVersion) =
    {   MajorVersion = major.MajorVersion
        Features =
            seq {
                for feature in major.Features ->
                    {   FeatureName = feature.FeatureName
                        Migrations =
                            seq {
                                for migration in feature.Migrations ->
                                    {   FeatureVersion = migration.FeatureVersion
                                        SourceFileName = migration.SourceFileName
                                        Command = f migration.Command
                                    }
                            } |> toReadOnlyList
                    }
            } |> toReadOnlyList
    }

let private stringizeFragments (fragments : CommandFragment seq) =
    seq {
        for fragment in fragments do
            match fragment with
            | LocalName name -> yield name
            | CommandText text -> yield text
            | Whitespace -> yield " "
            | Parameter _ -> failwith "Migrations may not reference parameters"
    } |> String.concat ""

let stringizeMajorVersion (backend : IBackend) (major : TStmts MigrationMajorVersion) =
    let indexer =
        { new IParameterIndexer with
            member this.ParameterIndex(parameter) = failwith "Migrations may not be parameterized"
        }
    let stringize stmts = backend.ToCommandFragments(indexer, stmts) |> stringizeFragments
    mapStmts stringize major

let quotationize (major : string MigrationMajorVersion) =
    let quotationizeMigration migration =
        Expr.NewRecord
            ( typeof<string Migration>
            ,   [   Quotations.Expr.Value(migration.Command)
                    Quotations.Expr.Value(migration.FeatureVersion)
                    Quotations.Expr.Value(migration.SourceFileName)
                ])
    let quotationizeFeature feature =
        Expr.NewRecord
            ( typeof<string MigrationFeature>
            ,   [   Quotations.Expr.Value(feature.FeatureName)
                    Expr.NewArray
                        ( typeof<string Migration>
                        , [ for migration in feature.Migrations -> quotationizeMigration migration ])
                ]
            )
    let features =
        Expr.NewArray
            ( typeof<string MigrationFeature>
            , [ for feature in major.Features -> quotationizeFeature feature ])
    Expr.NewRecord
        ( typeof<string MigrationMajorVersion>
        ,   [   features
                Quotations.Expr.Value(major.MajorVersion)
            ])