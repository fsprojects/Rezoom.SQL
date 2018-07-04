namespace Rezoom.SQL.Compiler
open System
open System.Data
open System.Collections.Generic
open System.Globalization
open Rezoom.SQL.Compiler
open Rezoom.SQL.Compiler.BackendUtilities
open Rezoom.SQL.Compiler.Translators
open Rezoom.SQL.Mapping
open Rezoom.SQL.Migrations

type DefaultBackend() =
    static let initialModel =
        let main, temp = Name("main"), Name("temp")
        {   Schemas =
                [   Schema.Empty(main)
                    Schema.Empty(temp)
                ] |> List.map (fun s -> s.SchemaName, s) |> Map.ofList
            DefaultSchema = main
            TemporarySchema = temp
            Builtin =
                {   Functions = DefaultFunctions.extendedBy [||]
                }
            BackendCharacteristics =
                {   CanDropColumnWithDefaultValue = true
                }
        }

    interface IBackend with
        member this.MigrationBackend =
            <@ fun settings ->
                new DefaultMigrationBackend(settings) :> IMigrationBackend
            @>
        member this.InitialModel = initialModel
        member this.ParameterTransform(columnType) = ParameterTransform.Default(columnType)
        member this.ToCommandFragments(indexer, stmts) =
            let translator = DefaultStatementTranslator(Name("RZSQL"), indexer)
            translator.TotalStatements(stmts)
            |> BackendUtilities.simplifyFragments
            |> ResizeArray
            :> _ IReadOnlyList
       