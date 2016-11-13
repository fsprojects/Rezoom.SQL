module Rezoom.SQL.Mapping.MigrationRunner
open System
open System.Collections.Generic

type MigrationName =
    {   MajorVersion : int
        ParentName : string option
        Name : string
    }
    override this.ToString() =
        match this.ParentName with
        | None -> sprintf "V%d.%s" this.MajorVersion this.Name
        | Some parent -> sprintf "V%d.%s-%s" this.MajorVersion parent this.Name

type Migration<'src> =
    {   MajorVersion : int
        Name : string
        Source : 'src
    }

type MigrationTree<'src> =
    {   Node : 'src Migration
        Children : 'src MigrationTree IReadOnlyList
    }
    member this.Migrations() =
        seq {
            yield this.Node
            for child in this.Children do
                yield! child.Migrations()
        }

type private MigrationTreeBuilderNode<'src> =
    {   mutable Source : 'src option
        Name : string
        Children : 'src MigrationTreeBuilderNode ResizeArray
    }

type private MigrationTreeBuilder<'src>(majorVersionNumber) =
    let migrations = Dictionary()
    let mutable root = None
    member __.ToTree() =
        match root with
        | None ->
            failwithf "No root migration for V%d" majorVersionNumber
        | Some (root, rootName) ->
            let rec toTree (node : 'src MigrationTreeBuilderNode) =
                {   Node =
                        {   MajorVersion = majorVersionNumber
                            Name = node.Name
                            Source =
                                match node.Source with
                                | None ->
                                    failwithf "No source for migration V%d.%s"
                                        majorVersionNumber node.Name
                                | Some src -> src
                        }
                    Children =
                        node.Children |> Seq.map toTree |> ResizeArray
                }
            toTree root         
    member __.Add(name : MigrationName, source : 'src) =
        let succ, self = migrations.TryGetValue(name.Name)
        let self =
            if succ then
                if Option.isSome self.Source then 
                    failwithf "Multiple sources given for migration %O" name
                self.Source <- Some source
                self
            else
                let newNode =
                    {   Source = Some source
                        Name = name.Name
                        Children = ResizeArray()
                    }
                migrations.[name.Name] <- newNode
                newNode
        match name.ParentName with
        | None ->
            match root with
            | Some (node, rootName) ->
                failwithf "Multiple root migrations given (%O, %O)" rootName name
            | None ->
                root <- Some (self, name)
        | Some parentName ->
            let succ, parent = migrations.TryGetValue(parentName)
            if succ then
                parent.Children.Add(self)
            else
                let parent =
                    {   Source = None
                        Name = name.Name
                        Children = ResizeArray([|self|])
                    }
                migrations.[parentName] <- parent

type MigrationTreeListBuilder() =
    let majorVersions = Dictionary()
    member __.Add(name : MigrationName, source : 'src) =
        let succ, found = majorVersions.TryGetValue(name.MajorVersion)
        let found =
            if succ then found else
            let builder = MigrationTreeBuilder(name.MajorVersion)
            majorVersions.[name.MajorVersion] <- builder
            builder
        found.Add(name, source)
    member __.ToTrees() =
        majorVersions
        |> Seq.sortBy (fun v -> v.Key)
        |> Seq.map (fun v -> v.Value.ToTree())
        |> ResizeArray

type IMigrationBackend =
    abstract member GetMigrationsRun : unit -> (int * string) seq
    abstract member RunMigration : string Migration -> unit

type MigrationConfig =
    {   AllowMigrationsFromOlderMajorVersions : bool
    }

let runMigrations config (backend : IMigrationBackend) (migrationTrees : string MigrationTree seq) =
    let already = HashSet(backend.GetMigrationsRun())
    let currentMajorVersion =
        already
        |> Seq.map fst
        |> Seq.sortByDescending id
        |> Seq.tryHead
    let currentMajorVersion =
        match currentMajorVersion with
        | Some version -> version
        | None -> Int32.MinValue
    for migrationTree in migrationTrees do
        for migration in migrationTree.Migrations() do
            let pair = migration.MajorVersion, migration.Name
            if not <| already.Contains(pair) then
                if migration.MajorVersion < currentMajorVersion then
                    failwith "oh that's no good, you can't do that"
                else
                    backend.RunMigration(migration)
                    ignore <| already.Add(pair) // actually we don't need this but ok

