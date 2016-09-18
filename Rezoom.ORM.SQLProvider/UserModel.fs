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
        failwith "not implemented" : UserModel