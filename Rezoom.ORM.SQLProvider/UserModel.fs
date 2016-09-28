namespace Rezoom.ORM.SQLProvider
open System
open System.Collections.Generic
open SQLow

type UserModel(backend : IBackend, model : Model) =
    member __.Backend = backend
    member __.Model = model
    static member Load(path : string) =
        // go parse config file / find migration scripts
        // be sure to cache this
        let backend = SQLiteBackend() :> IBackend
        UserModel
            ( backend = backend
            , model =
                {   Schemas =
                        [   {   SchemaName = Name("main")
                                Tables = Map.empty
                                Views = Map.empty
                            }
                            {   SchemaName = Name("temp")
                                Tables = Map.empty
                                Views = Map.empty
                            }
                        ] |> List.map (fun s -> s.SchemaName, s) |> Map.ofList
                    DefaultSchema = Name("main")
                    TemporarySchema = Name("temp")
                    Builtin = backend.Builtin
                }
            )