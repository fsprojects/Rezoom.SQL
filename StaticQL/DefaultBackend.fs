namespace StaticQL
open System
open System.Data
open System.Collections.Generic
open System.Globalization
open StaticQL
open StaticQL.Mapping
open StaticQL.BackendUtilities
open StaticQL.Translators

type DefaultBackend() =
    static let initialModel =
        let main, temp = Name("main"), Name("temp")
        {   Schemas =
                [   {   SchemaName = main
                        Tables = Map.empty
                        Views = Map.empty
                    }
                    {   SchemaName = temp
                        Tables = Map.empty
                        Views = Map.empty
                    }
                ] |> List.map (fun s -> s.SchemaName, s) |> Map.ofList
            DefaultSchema = main
            TemporarySchema = temp
            Builtin =
                {   Functions = Map.empty
                }
        }
    interface IBackend with
        member this.InitialModel = initialModel
        member this.ParameterTransform(columnType) = ParameterTransform.Default(columnType)
        member this.ToCommandFragments(indexer, stmts) =
            let translator = DefaultStatementTranslator(indexer)
            translator.Statements(stmts)
            |> BackendUtilities.simplifyFragments
            |> ResizeArray
            :> _ IReadOnlyList
       