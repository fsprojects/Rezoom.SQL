namespace Rezoom.SQL.Migrations
open System
open System.Collections.Generic
open System.Data.Common
open System.Runtime.CompilerServices
open FSharp.Quotations

type MigrationFileName =
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
    member this.FileName = "V" + string this.MajorVersion + "." + this.Name

type MigrationTree<'src> =
    {   Node : 'src Migration
        Children : 'src MigrationTree IReadOnlyList
    }
    member this.Map(f) =
        {   Node =
                {   MajorVersion = this.Node.MajorVersion
                    Name = this.Node.Name
                    Source = f this.Node.Source
                }
            Children = this.Children |> Seq.map (fun t -> t.Map(f)) |> ResizeArray
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
    member __.Add(name : MigrationFileName, source : 'src) =
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

type MigrationTreeListBuilder<'src>() =
    let majorVersions = Dictionary()
    member __.Add(name : MigrationFileName, source : 'src) =
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
    inherit IDisposable
    abstract member Initialize : unit -> unit
    abstract member GetMigrationsRun : unit -> (int * string) seq
    abstract member RunMigration : string Migration -> unit

type MigrationConfig =
    {   AllowMigrationsFromOlderMajorVersions : bool
        LogMigrationRan : string Migration -> unit
    }

module MigrationUtilities =
    let private quotationizeMigration (migration : string Migration) =
        <@@ {   MajorVersion = %%Expr.Value(migration.MajorVersion)
                Name = %%Expr.Value(migration.Name)
                Source = %%Expr.Value(migration.Source)
            } : string Migration @@>

    let rec quotationizeMigrationTree (tree : string MigrationTree) =
        let children =
            Expr.NewArray(typeof<string MigrationTree>,
                [ for child in tree.Children ->
                    quotationizeMigrationTree child
                ])
        let children = Expr.Coerce(children, typeof<string MigrationTree IReadOnlyList>)
        <@@ {   Node = %%quotationizeMigration tree.Node
                Children = %%children
            } : string MigrationTree @@>

    let foldMigrations
        (folder : bool -> 'acc -> 's1 Migration -> 's2 * 'acc)
        (acc : 'acc)
        (migrationTrees : 's1 MigrationTree seq) =
        let mutable acc = acc
        let rec mapFold root tree =
            let s2, acc2 = folder root acc tree.Node
            acc <- acc2
            {   Node =
                    {   MajorVersion = tree.Node.MajorVersion
                        Name = tree.Node.Name
                        Source = s2
                    }
                Children = tree.Children |> Seq.map (mapFold false) |> ResizeArray
            }
        let trees = [ for tree in migrationTrees -> mapFold true tree ]
        trees, acc

    let runMigrations config (backend : IMigrationBackend) (migrationTrees : string MigrationTree seq) =
        backend.Initialize()
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
                    if migration.MajorVersion < currentMajorVersion
                        && not config.AllowMigrationsFromOlderMajorVersions then
                        failwith <|
                            sprintf "Can't run migration V%d.%s because database has a newer major version (V%d)"
                                migration.MajorVersion migration.Name
                                currentMajorVersion
                    else
                        backend.RunMigration(migration)
                        config.LogMigrationRan migration
                        ignore <| already.Add(pair) // actually we don't need this but ok

[<Extension>]
 type MigrationExtensions =
    [<Extension>]
    static member Run
        ( migrations : string MigrationTree array
        , config : MigrationConfig
        , backend : unit -> IMigrationBackend
        ) =
        use backend = backend()
        MigrationUtilities.runMigrations config backend migrations

