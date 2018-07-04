namespace Rezoom.SQL.Provider
open System
open System.Collections.Generic
open System.IO
open Rezoom.SQL.Compiler

type private UserModelCache() as this =
    let watchers = Dictionary()
    let cache = Dictionary()
    let invalidated = Event<EventHandler, EventArgs>()

    let addWatcher path invalidateKey =
        let succ, watcher = watchers.TryGetValue(path)
        let watcher =
            if succ then watcher else
            let watcher = new Watcher(path)
            watcher.Invalidated.Add(fun _ -> invalidated.Trigger(this, EventArgs.Empty))
            watchers.Add(path, watcher)
            watcher
        watcher.Invalidating.Add(fun _ -> ignore <| cache.Remove(invalidateKey)) // remove from cache on changes

    [<CLIEvent>]
    member __.Invalidated = invalidated.Publish

    member this.Load(resolutionFolder, modelPath) =
        let key = (resolutionFolder, modelPath)
        let succ, cachedModel = cache.TryGetValue(key)
        if succ then cachedModel else
        let model = UserModel.Load(resolutionFolder, modelPath)
        cache.[key] <- model
        addWatcher model.ConfigDirectory key
        addWatcher model.MigrationsDirectory key
        model

    member this.Dispose() =
        for KeyValue(_, w) in watchers do
            w.Dispose()
        watchers.Clear()
    interface IDisposable with
        member this.Dispose() = this.Dispose()
