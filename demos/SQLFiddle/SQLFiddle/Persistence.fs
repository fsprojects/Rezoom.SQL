module SQLFiddle.Persistence
open System
open System.IO
open System.Security.Cryptography
open System.Text
open Rezoom
open Rezoom.SQL
open Rezoom.SQL.Plans

let private noNulls bytes =
    for by in bytes do
        if by = 0uy then
            failwith "No NULL bytes allowed!"
    bytes

let private sha1 (fiddleData :  FiddleInput) =
    use hasher = SHA1.Create()
    let modelBytes = Encoding.UTF8.GetBytes(fiddleData.Model) |> noNulls
    ignore <| hasher.TransformBlock(modelBytes, 0, modelBytes.Length, modelBytes, 0)

    ignore <| hasher.TransformBlock([| 0uy |], 0, 1, [| 0uy |], 0)

    let commandBytes = Encoding.UTF8.GetBytes(fiddleData.Command) |> noNulls
    ignore <| hasher.TransformBlock(commandBytes, 0, commandBytes.Length, commandBytes, 0)
    
    ignore <| hasher.TransformBlock([| 0uy |], 0, 1, [| 0uy |], 0)

    let backendBytes = Encoding.UTF8.GetBytes(fiddleData.Backend.ToString()) |> noNulls
    ignore <| hasher.TransformFinalBlock(backendBytes, 0, backendBytes.Length)

    hasher.Hash

type SaveFiddleSQL = SQL<"""
insert into Fiddles(SHA1, Backend, Model, Command)
select @sha1, @backend, @model, @command
where not exists(select null x from Fiddles where SHA1 = @sha1)
""">


let saveFiddle fiddleData =
    plan {
        let sha1 = sha1 fiddleData
        let cmd =
            SaveFiddleSQL.Command
                ( fiddleData.Backend.ToString()
                , fiddleData.Command
                , fiddleData.Model
                , sha1
                )
        do! cmd.Plan()
        return FiddleId sha1
    }

type GetFiddleSQL = SQL<"""
select Backend, Model, Command from Fiddles
where SHA1 = @sha1
""">

let getFiddle (FiddleId sha1) =
    plan {
        let! found = GetFiddleSQL.Command(sha1).ExactlyOne()
        let backend = FiddleBackend.Parse(found.Backend)
        match backend with
        | None -> return failwith "Bad data: invalid backend"
        | Some backend ->
            return
                {   Backend = backend
                    Model = found.Model
                    Command = found.Command
                }
    }