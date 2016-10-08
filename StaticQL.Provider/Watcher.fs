namespace StaticQL.Provider
open System
open System.IO
open StaticQL

type Watcher(path : string) as this =
    let fs = new FileSystemWatcher(path, IncludeSubdirectories = true)
    let invalidate = Event<EventHandler, EventArgs>()
    static let isRelevant (path : string) =
        path.EndsWith(".SQL", StringComparison.OrdinalIgnoreCase)
        || path.EndsWith(UserModel.ConfigFileName, StringComparison.OrdinalIgnoreCase)
    let handler (ev : FileSystemEventArgs) =
        if isRelevant ev.FullPath then invalidate.Trigger(this, EventArgs.Empty)

    do
        fs.Created.Add(handler)
        fs.Deleted.Add(handler)
        fs.Changed.Add(handler)
        fs.Renamed.Add(handler)

    [<CLIEvent>]
    member __.Invalidated = invalidate.Publish
    member __.Dispose() = fs.Dispose()

    interface IDisposable with
        member this.Dispose() = this.Dispose()