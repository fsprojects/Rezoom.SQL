namespace Rezoom.SQL.Provider
open System
open System.Threading
open System.IO
open Rezoom.SQL.Compiler

type private Watcher(path : string) as this =
    let fs = new FileSystemWatcher(path, IncludeSubdirectories = true)
    let invalidating = Event<EventHandler, EventArgs>()
    let invalidated = Event<EventHandler, EventArgs>()
    let timer = new Timer(fun _ -> // we use a timer to buffer changes so we don't invalidate many times in a couple ms
        invalidating.Trigger(this, EventArgs.Empty)
        invalidated.Trigger(this, EventArgs.Empty))
    static let isRelevant (path : string) =
        path.EndsWith(".SQL", StringComparison.OrdinalIgnoreCase)
        || path.EndsWith(UserModel.ConfigFileName, StringComparison.OrdinalIgnoreCase)
    let handler (ev : FileSystemEventArgs) =
        if isRelevant ev.FullPath then
            ignore <| timer.Change(TimeSpan.FromMilliseconds(100.0), Timeout.InfiniteTimeSpan)

    do
        fs.Created.Add(handler)
        fs.Deleted.Add(handler)
        fs.Changed.Add(handler)
        fs.Renamed.Add(handler)
        fs.EnableRaisingEvents <- true

    member __.Path = path

    [<CLIEvent>]
    member __.Invalidating = invalidating.Publish
    [<CLIEvent>]
    member __.Invalidated = invalidated.Publish
    member __.Dispose() =
        fs.Dispose()
        timer.Dispose()

    interface IDisposable with
        member this.Dispose() = this.Dispose()