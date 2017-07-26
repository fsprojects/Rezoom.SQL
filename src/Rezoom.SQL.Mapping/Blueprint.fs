namespace Rezoom.SQL.Mapping
open Rezoom.SQL.Mapping.CodeGeneration
open System
open System.Collections.Generic
open System.Reflection

[<NoComparison>]
type Setter =
    /// We initialize this column by passing it to the composite's constructor.
    | SetConstructorParameter of ParameterInfo
    /// We initialize this column by setting a field post-construction.
    | SetField of FieldInfo
    /// We initialize this column by setting a property post-construction.
    | SetProperty of PropertyInfo

[<NoComparison>]
type Getter =
    | GetField of FieldInfo
    | GetProperty of PropertyInfo
    member this.MemberInfo =
        match this with
        | GetField f -> f :> MemberInfo
        | GetProperty p -> p :> MemberInfo

[<NoComparison>]
type Column =
    {
        ColumnId : ColumnId
        /// The name of this column. This is the basename of the SQL column name that
        /// will represent it. This should always be treated case-insensitively.
        Name : string
        /// The blueprint for this column's type.
        Blueprint : Blueprint Lazy
        /// The way to set this column when initializing an instance of the composite type.
        Setter : Setter
        /// The way to get this column's value (could be used for analyzing expression trees).
        Getter : Getter option
        /// The column on this column's type that points to this.
        ReverseRelationship : Column option Lazy
    }
    member this.Output = this.Blueprint.Value.Output

and [<NoComparison>] Composite =
    {
        Output : Type
        /// The constructor to use when instantiating this composite type.
        /// All parameters must be supplied by columns.
        Constructor : ConstructorInfo
        /// The identity columns for this composite type, if any.
        Identity : Column IReadOnlyList
        /// All the columns of this composite type (including the identity, if any).
        /// Indexed by name, case insensitive.
        Columns : IReadOnlyDictionary<string, Column>
    }
    member this.ColumnByGetter(mem : MemberInfo) =
        this.Columns.Values |> Seq.tryFind (fun col ->
            match col.Getter with
            | None -> false
            | Some getter -> getter.MemberInfo = mem)
    member this.TableName = this.Output.Name
    member this.ReferencesQueryParent =
        this.Columns.Values
        |> Seq.exists (fun c -> c.ReverseRelationship.Value |> Option.isSome)

and [<NoComparison>]
    [<NoEquality>] Primitive =
    {
        Output : Type
        /// A method converting an object to the output type.
        Converter : RowConversionMethod
    }

and [<NoComparison>]
    [<NoEquality>] BlueprintShape =
    | Primitive of Primitive
    | Composite of Composite

and [<NoComparison>]
    [<NoEquality>] ElementBlueprint =
    {
        Shape : BlueprintShape
        /// The element type this blueprint specifies how to construct.
        Output : Type
    }
    member internal this.IsOne(roots : Type HashSet) =
        match this.Shape with
        | Primitive _ -> true
        | Composite c ->
            c.Columns.Values
            |> Seq.forall (fun c ->
                let blueprint = c.Blueprint.Value
                roots.Contains(blueprint.Output)
                || roots.Add(blueprint.Output) && blueprint.IsOne(roots))

and [<NoComparison>]
    [<NoEquality>] Cardinality =
    | One of ElementBlueprint
    /// Carries an element type blueprint and a method converting an ICollection<Builder<ElementType>>
    /// to the target collection type.
    | Many of ElementBlueprint * ConversionMethod
    member this.Element =
        match this with
        | One elem -> elem
        | Many (elem, _) -> elem

and [<NoComparison>]
    [<NoEquality>] Blueprint =
    {
        Cardinality : Cardinality
        /// The type (possibly a collectiont ype) this blueprint specifies how to construct.
        Output : Type
    }
    member internal this.IsOne(roots : Type HashSet) =
        match this.Cardinality with
        | One e -> e.IsOne(roots)
        | Many _ -> false
    member this.IsOne() = this.IsOne(new HashSet<_>([| this.Output |]))
