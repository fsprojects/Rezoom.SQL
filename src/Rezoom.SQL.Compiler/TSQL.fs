namespace Rezoom.SQL.Compiler.TSQL
open System.Collections.Generic
open Rezoom.SQL.Compiler
open Rezoom.SQL.Migrations

type TSQLBackend() =
    inherit BackendBase()
    static let initialModel =
        let main, temp = Name("dbo"), Name("temp")
        {   Schemas =
                [   Schema.Empty(main)
                    Schema.Empty(temp)
                ] |> List.map (fun s -> s.SchemaName, s) |> Map.ofList
            DefaultSchema = main
            TemporarySchema = temp
            Builtin =
                {   Functions = TSQLFunctions.functions
                }
            BackendCharacteristics =
                {   CanDropColumnWithDefaultValue = false
                }
        }
    override this.MigrationBackend = <@ fun conn -> new TSQLMigrationBackend(conn) :> IMigrationBackend @>
    override this.InitialModel = initialModel
    override this.ToCommandFragments(indexer, stmts) =
        let translator = TSQLStatement(indexer)
        translator.TotalStatements(stmts)
        |> BackendUtilities.simplifyFragments
        |> ResizeArray
        :> _ IReadOnlyList
    override this.SQLTypeString (tyName : TypeName) = 
        TSQLExpression.TSQLTypeString(tyName)