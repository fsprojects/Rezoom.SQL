namespace Rezoom.ORM.SQLProvider
open System
open System.Collections.Generic
open SQLow

// Migrations have three version parts:
// Major Version - Feature Name - Feature Version
//
// There is no strict ordering of feature names -- so the migrations for Feature A can run before or after
// Feature B. The code in this file validates that each ordering of feature migrations results in the same
// model.
//         
//    +---V2 Feature A.1---V2 Feature A.2----+                          +---V4 Feature A
//   /                                        \                        /
// V1-----V2 Feature B-----------------------V2-----V3 Feature A-----V3-----V4 Feature B.1---V4 Feature B.2
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

type CheckMigration =
    {   Command : Stmts
        FeatureVersion : int
        SourceFileName : string
    }
    member this.NextModel(model : Model) =
        let effect = CommandEffect.OfSQL(model, this.Command)
        if effect.Parameters.Count > 0 then
            failwithf
                "Migrations may not contain parameters: parameter named ``%O`` in %s"
                effect.Parameters.[0]
                this.SourceFileName
        effect.ModelChange |? model
        
type CheckMigrationFeature =
    {  
        FeatureName : Name
        Migrations : CheckMigration IReadOnlyList
    }
    member this.NextModel(model : Model) =
        this.Migrations
        |> Seq.fold (fun model migration -> migration.NextModel(model)) model

type CheckMigrationMajorVersion =
    {   Features : CheckMigrationFeature IReadOnlyCollection
        MajorVersion : int
    }
    static member private Factorial(x) =
        if x <= 2L then x else x * CheckMigrationMajorVersion.Factorial(x - 1L)
    member this.NextModel(model : Model) =
        let maxFeatures = 6
        if this.Features.Count > maxFeatures then
            failwithf
                "No more than %d features per major version are permitted -- would have to validate %d migration paths"
                maxFeatures (CheckMigrationMajorVersion.Factorial(int64 this.Features.Count))
        let mutable commonNextModel = None
        for featureSequence in this.Features |> permutations do
            let nextModel =
                featureSequence
                |> Seq.fold (fun model feature -> feature.NextModel(model)) model
            match commonNextModel with
            | Some commonNextModel ->
                if nextModel <> commonNextModel then
                    failwithf
                        "The features of major version %d result in different models when run in different orders"
                        this.MajorVersion
            | None ->
                commonNextModel <- Some nextModel
        commonNextModel |? model

type CheckMigrationTrack(majorVersions : CheckMigrationMajorVersion IReadOnlyList) =
    member this.CurrentVersionModel(initial : Model) =
        majorVersions
        |> Seq.fold (fun model version -> version.NextModel(model)) initial
    